use crate::{ minijvm };

use std::{ collections::{ HashSet }, ops::Range };
use anyhow::{ anyhow, ensure };
use itertools::Itertools;

#[derive(Debug, Clone)]
pub(super) enum CFGInstruction<'a> {
    Intsructions(&'a [minijvm::Instruction]),
    If {
        condition: &'a minijvm::GotoCondition,
        then: Vec<CFGInstruction<'a>>,
        r#else: Vec<CFGInstruction<'a>>,
    },
    // This is evaluated by first doing a ==0 condition, if true all conditions are
    // skipped, otherwise the next condition is evaluated and the procedure is restarted
    // that means the first condition is instructions before this one
    BoolAnd {
        conditions: Vec<Vec<CFGInstruction<'a>>>,
    },
}

#[derive(Debug, Clone)]
pub(super) struct CFGGoto<'a> {
    pub(super) cond: &'a minijvm::GotoCondition,
    pub(super) block_idx: usize,
}

#[derive(Debug, Clone, Default)]
pub(super) struct CFGBlock<'a> {
    pub(super) instructions: Vec<CFGInstruction<'a>>,
    pub(super) cond_goto: Option<CFGGoto<'a>>,
    pub(super) next_block_idx: Option<usize>,
}

impl CFGBlock<'_> {
    fn simple_next(&self) -> Option<usize> {
        if self.cond_goto.is_some() { return None }
        self.next_block_idx
    }
}

#[derive(Debug)]
pub(super) struct ControlFlowGraph<'a> {
    /// The first block is always the entry block
    pub(super) blocks: Vec<CFGBlock<'a>>,
}

impl<'a> ControlFlowGraph<'a> {
    pub(super) fn new(instructions: &'a [minijvm::Instruction]) -> anyhow::Result<Self> {
        construct_cfg(instructions)
    }

    pub(super) fn block_predecessor(&'_ self, block_idx: usize) -> impl Iterator<Item = (usize, &'_ CFGBlock<'a>)> {
        self.blocks.iter().enumerate()
            .filter(move |(_, b)| {
                b.next_block_idx == Some(block_idx) ||
                    b.cond_goto.as_ref().map(|p| p.block_idx) == Some(block_idx)
            })
    }

    pub(super) fn simplify(&mut self) -> anyhow::Result<()> {
        let mut taken_blocks = vec![false; self.blocks.len()];
        while simplify_cfg(self, &mut taken_blocks)? { }

        while taken_blocks.last().copied().unwrap_or(false) {
            self.blocks.pop();
            taken_blocks.pop();
        }
        
        Ok(())
    }
}

fn construct_cfg(instructions: &'_ [minijvm::Instruction]) -> anyhow::Result<ControlFlowGraph<'_>> {
    let mut current_block_start = 0;

    let mut cfg = ControlFlowGraph {
        blocks: vec![],
    };
    let mut blocks_ranges = Vec::<Range<usize>>::new();

    // Lists all instructions that are the target of gotos
    let mut jump_target_markers = HashSet::<usize>::new();

    for (pc, instr) in instructions.into_iter().enumerate() {
        if jump_target_markers.contains(&pc) && current_block_start != pc {
            blocks_ranges.push(current_block_start..pc);
            cfg.blocks.push(CFGBlock {
                instructions: vec![CFGInstruction::Intsructions(&instructions[current_block_start..pc])],
                cond_goto: None,
                next_block_idx: Some(pc),
            });
            current_block_start = pc;
        }

        if let &minijvm::Instruction::Goto { target, ref cond } = instr {
            ensure!(target != pc, "Goto pointing to itself?");
            ensure!(!jump_target_markers.contains(&pc), "Goto jumping to goto? Not supported");
            ensure!(target > pc, "Backward gotos are not yes supported");

            blocks_ranges.push(current_block_start..pc+1);
            cfg.blocks.push(CFGBlock {
                instructions: vec![CFGInstruction::Intsructions(&instructions[current_block_start..pc])],
                cond_goto: cond.as_ref().map(|cond| CFGGoto {
                    cond,
                    block_idx: target,
                }),
                next_block_idx: cond.as_ref().map(|_| pc + 1)
                    .or(Some(target)),
            });

            current_block_start = pc + 1;
            jump_target_markers.insert(target);
        }
    }

    // push the last block
    if current_block_start != instructions.len() {
        blocks_ranges.push(current_block_start..instructions.len());
        cfg.blocks.push(CFGBlock {
            instructions: vec![CFGInstruction::Intsructions(&instructions[current_block_start..])],
            cond_goto: None,
            next_block_idx: None,
        });
    }

    // Convert block_idx that before this are actually the target instruction idx
    // into the block index that contains that instruction
    {
        let ops_idx_to_block_idx = move |i: usize| blocks_ranges.iter().position(|p| p.contains(&i))
            .ok_or_else(|| anyhow!("Invalid block jump idx"));
        for block in &mut cfg.blocks {
            block.next_block_idx = block.next_block_idx.map(&ops_idx_to_block_idx).transpose()?;
            if let Some(goto) = &mut block.cond_goto {
                goto.block_idx = ops_idx_to_block_idx(goto.block_idx)?;
            }
        }
    }

    Ok(cfg)
}

fn simplify_cfg(cfg: &mut ControlFlowGraph, taken_blocks: &mut [bool]) -> anyhow::Result<bool> {
    if cfg.blocks.len() <= 1 {
        return Ok(false);
    }

    debug_assert!(taken_blocks.iter().copied().enumerate().filter(|&(_, taken)| taken).all(|(bidx, _)| cfg.block_predecessor(bidx).count() == 0), "All taken blocks should have no predecessor");

    #[derive(Debug)]
    enum SearchResult {
        If {
            then: Option<usize>,
            r#else: Option<usize>,
            finally: Option<usize>,
        },

        BoolAnd {
            conditions: Vec<usize>,
            then: usize,
            r#else: usize,
        },
    }

    macro_rules! is_only_pred {
        ($parent: expr => $child: expr) => {
            cfg.block_predecessor($child).exactly_one().is_ok_and(|(idx, _)| idx == $parent)
        };
    }

    const EQ_ZERO_CONDITION: minijvm::GotoCondition = minijvm::GotoCondition { operand: minijvm::IfOperand::Zero, cmp: minijvm::IfCmp::Eq };

    // Look for a block that is collapsable
    let Some((bidx, result)) = cfg.blocks.iter().enumerate().filter(|&(bidx, _)| !taken_blocks[bidx]).find_map(|(bidx, block)| {
        let cond_next = block.cond_goto.as_ref().map(|cond| cond.block_idx);
        let cond_next_next = cond_next.and_then(|cn| cfg.blocks[cn].simple_next());
        let next = block.next_block_idx;
        let next_next = block.next_block_idx.and_then(|cn| cfg.blocks[cn].simple_next());

        let kind = match (cond_next, cond_next_next, next, next_next) {
            // This block unconditionally jumps to a next block
            // and that next block has no other predecessor
            (None, _, Some(next), _) if is_only_pred!(bidx => next)
                => SearchResult::If { then: None, r#else: None, finally: Some(next) },
            // This block is an if without an else condition
            (Some(then), Some(finally), Some(finally2), _) if finally == finally2 && is_only_pred!(bidx => then)
                => SearchResult::If { then: Some(then), r#else: None, finally: Some(finally) },
            // This block is an if without an then condition
            (Some(finally), _, Some(r#else), Some(finally2)) if finally == finally2 && is_only_pred!(bidx => r#else)
                => SearchResult::If { then: None, r#else: Some(r#else), finally: Some(finally) },
            // This block is an `if` block where both branch directly converge
            // back
            (Some(then), finally, Some(r#else), finally2) if finally == finally2 && is_only_pred!(bidx => then) && is_only_pred!(bidx => r#else)
                => SearchResult::If { then: Some(then), r#else: Some(r#else), finally },

            // Detect && boolean chaining
            (Some(then), _, Some(r#else), _) if block.cond_goto.as_ref().is_some_and(|goto| goto.cond == &EQ_ZERO_CONDITION) && is_only_pred!(bidx => r#else) => {
                let mut conditions = vec![];

                let mut current = r#else;
                while cfg.blocks[current].cond_goto.as_ref().is_some_and(|goto| goto.block_idx == then && goto.cond == &EQ_ZERO_CONDITION) {
                    conditions.push(current);
                    let Some(new_current) = cfg.blocks[current].next_block_idx
                    else { return None };
                    current = new_current;
                }

                if conditions.is_empty() {
                    return None;
                }
                
                SearchResult::BoolAnd {
                    conditions,
                    then,
                    r#else: current,
                }
            },
            
            _ => return None,
        };

        Some((bidx, kind))
    })
    else { return Ok(false) };

    macro_rules! take_block { ($bidx: expr) => {{
        let to_take_bidx = $bidx;
        debug_assert!(!taken_blocks[to_take_bidx], "Cannot take a block twice");
        #[cfg(debug_assertions)]
        match cfg.block_predecessor(to_take_bidx).at_most_one() {
            // Ok(Some((n, _))) => assert_eq!(n, bidx, "When taking a block, its only predecessor must be the current block"),
            Ok(Some((_, _))) => (),
            Ok(None) => panic!("When taking a block, its only predecessor must be the current block, got no predecessor instead"),
            Err(e) => {
                panic!("When taking block {to_take_bidx}, its only predecessor must be the current block, got {:?} predecessor instead of just {bidx}", e.map(|(a, _)| a).collect_vec())
            },
        }
        taken_blocks[to_take_bidx] = true;
        std::mem::take(&mut cfg.blocks[to_take_bidx])
    }}; }

    match result {
        SearchResult::If { then: None, r#else: None, finally: None } => unreachable!(),
        SearchResult::If { then: None, r#else: None, finally: Some(next) } => {
            let next = take_block!(next);
            cfg.blocks[bidx].instructions.extend(next.instructions);
            cfg.blocks[bidx].cond_goto = next.cond_goto;
            cfg.blocks[bidx].next_block_idx = next.next_block_idx;
        },
        SearchResult::If { then, r#else, finally } => {
            let then = then.map(|then| take_block!(then).instructions).unwrap_or_default();
            let r#else = r#else.map(|r#else| take_block!(r#else).instructions).unwrap_or_default();

            let condition = cfg.blocks[bidx].cond_goto.take().unwrap().cond;
            cfg.blocks[bidx].instructions.push(CFGInstruction::If {
                condition,
                then,
                r#else,
            });
            cfg.blocks[bidx].next_block_idx = finally;
        },
        SearchResult::BoolAnd { conditions, then, r#else } => {
            let conditions = conditions.into_iter().map(|block| take_block!(block).instructions).collect_vec();

            cfg.blocks[bidx].instructions.push(CFGInstruction::BoolAnd {
                conditions,
            });
            cfg.blocks[bidx].cond_goto = Some(CFGGoto {
                cond: &EQ_ZERO_CONDITION,
                block_idx: then,
            });
            cfg.blocks[bidx].next_block_idx = Some(r#else);
        },
    }

    Ok(true)
}
