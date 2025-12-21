use crate::{ minijvm::{ self, decomped } };

use std::{ collections::{ HashSet }, ops::Range };
use anyhow::{ anyhow, ensure };
use itertools::Itertools;

#[derive(Debug)]
pub(super) struct CFGGoto {
    pub(super) cond: decomped::Expression,
    pub(super) block_idx: usize,
}

#[derive(Default, Debug)]
pub(super) struct CFGBlock {
    pub(super) statements: Vec<decomped::Statement>,
    pub(super) cond_goto: Option<CFGGoto>,
    pub(super) next_block_idx: Option<usize>,
}

impl CFGBlock {
    pub(super) fn simple_next(&self) -> Option<usize> {
        if self.cond_goto.is_some() { return None }
        self.next_block_idx
    }
}

#[derive(Debug)]
pub(super) struct ControlFlowGraph {
    /// The first block is always the entry block
    pub(super) blocks: Vec<CFGBlock>,
}

impl ControlFlowGraph {
    pub(super) fn new(instructions: &[minijvm::Instruction]) -> anyhow::Result<Self> {
        construct_cfg(instructions)
    }

    pub(super) fn block_predecessor(&self, block_idx: usize) -> impl Iterator<Item = (usize, &CFGBlock)> {
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

fn construct_cfg(instructions: &[minijvm::Instruction]) -> anyhow::Result<ControlFlowGraph> {
    let mut current_block_start = 0;

    let mut cfg = ControlFlowGraph {
        blocks: vec![],
    };
    let mut blocks_ranges = Vec::<Range<usize>>::new();

    // Lists all instructions that are the target of gotos
    let mut jump_target_markers = HashSet::<usize>::new();

    let mut stack = vec![];
    let mut next_temp = 0u16;

    for (pc, instr) in instructions.into_iter().enumerate() {
        if jump_target_markers.contains(&pc) && current_block_start != pc {
            blocks_ranges.push(current_block_start..pc);
            cfg.blocks.push(CFGBlock {
                statements: super::decomp_block_code(&instructions[current_block_start..pc], &mut stack, &mut next_temp)?,
                cond_goto: None,
                next_block_idx: Some(pc),
            });
            ensure!(stack.is_empty(), "Stack non empty after decompiling block");
            current_block_start = pc;
        }

        if let &minijvm::Instruction::Goto { target, ref cond } = instr {
            ensure!(target != pc, "Goto pointing to itself?");
            ensure!(!jump_target_markers.contains(&pc), "Goto jumping to goto? Not supported");
            ensure!(target > pc, "Backward gotos are not yes supported");

            let statements = super::decomp_block_code(&instructions[current_block_start..pc], &mut stack, &mut next_temp)?;
            let cond_expr = cond.as_ref().map(|cond| {
                super::convert_condition_to_expression(cond, &mut stack)
            }).transpose()?;
            ensure!(stack.is_empty(), "Stack non empty after decompiling block");

            blocks_ranges.push(current_block_start..pc+1);
            cfg.blocks.push(CFGBlock {
                statements,
                cond_goto: cond_expr.map(|cond| CFGGoto {
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
        let statements = super::decomp_block_code(&instructions[current_block_start..], &mut stack, &mut next_temp)?;
        ensure!(stack.is_empty(), "Stack non empty after decompiling block");

        blocks_ranges.push(current_block_start..instructions.len());
        cfg.blocks.push(CFGBlock {
            statements,
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

    enum Kind {
        A { next: usize },
        B { then: usize, finally: usize },
        C { r#else: usize, finally: usize },
        D { then: usize, r#else: usize, finally: usize },
    }

    // Look for a block that is collapsable
    let Some((bidx, kind)) = cfg.blocks.iter().enumerate().filter(|&(bidx, _)| !taken_blocks[bidx]).find_map(|(bidx, block)| {
        let cond_next = block.cond_goto.as_ref().map(|cond| cond.block_idx);
        let cond_next_next = cond_next.and_then(|cn| cfg.blocks[cn].simple_next());
        let next = block.next_block_idx;
        let next_next = block.next_block_idx.and_then(|cn| cfg.blocks[cn].simple_next());

        let kind = match (cond_next, cond_next_next, next, next_next) {
            // This block unconditionally jumps to a next block
            // and that next block has no other predecessor
            (None, _, Some(next), _) if cfg.block_predecessor(next).exactly_one().is_ok_and(|(idx, _)| idx == bidx)
                => Kind::A { next },
            // This block is an if without an else condition
            (Some(then), Some(finally), Some(finally2), _) if finally == finally2
                => Kind::B { then, finally },
            // This block is an if without an then condition
            (Some(finally), _, Some(r#else), Some(finally2)) if finally == finally2
                => Kind::C { r#else, finally },
            // This block is an `if` block where both branch directly converge
            // back
            (Some(then), Some(finally), Some(r#else), Some(finally2)) if finally == finally2
                => Kind::D { then, r#else, finally },
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
            Ok(Some((n, _))) => assert_eq!(n, bidx, "When taking a block, its only predecessor must be the current block"),
            Ok(None) => panic!("When taking a block, its only predecessor must be the current block, got no predecessor instead"),
            Err(e) => panic!("When taking a block, its only predecessor must be the current block, got {} predecessor instead", e.count()),
        }
        taken_blocks[to_take_bidx] = true;
        std::mem::take(&mut cfg.blocks[to_take_bidx])
    }}; }

    match kind {
        Kind::A { next } => {
            let next = take_block!(next);
            cfg.blocks[bidx].statements.extend(next.statements);
            cfg.blocks[bidx].cond_goto = next.cond_goto;
            cfg.blocks[bidx].next_block_idx = next.next_block_idx;
        },
        Kind::B { then, finally } => {
            let then = take_block!(then);

            let condition = cfg.blocks[bidx].cond_goto.take().unwrap().cond;
            cfg.blocks[bidx].statements.push(decomped::Statement::If {
                condition,
                then_branch: then.statements,
                else_branch: vec![],
            });
            cfg.blocks[bidx].next_block_idx = Some(finally);
        },
        Kind::C { r#else, finally } => {
            let r#else = take_block!(r#else);

            let condition = cfg.blocks[bidx].cond_goto.take().unwrap().cond;
            cfg.blocks[bidx].statements.push(decomped::Statement::If {
                condition: super::invert_condition_expression(condition)?,
                then_branch: r#else.statements,
                else_branch: vec![],
            });
            cfg.blocks[bidx].next_block_idx = Some(finally);
        },
        Kind::D { then, r#else, finally } => {
            let then = take_block!(then);
            let r#else = take_block!(r#else);

            let condition = cfg.blocks[bidx].cond_goto.take().unwrap().cond;
            cfg.blocks[bidx].statements.push(decomped::Statement::If {
                condition,
                then_branch: then.statements,
                else_branch: r#else.statements,
            });
            cfg.blocks[bidx].next_block_idx = Some(finally);
        },
    }

    Ok(true)
}
