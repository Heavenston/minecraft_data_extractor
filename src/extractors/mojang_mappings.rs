use std::{num::ParseIntError, ops::RangeInclusive, sync::LazyLock};

use anyhow::{ anyhow };
use nom::{ bytes::is_not, combinator::complete, sequence::{preceded, terminated} };
use tokio::fs;

// The first version to release official mapping was '1.14.4' which release on '2019-07-19T09:25:47+00:00'
static MOJMAPS_FIRST_VERSION_TIME: LazyLock<chrono::DateTime<chrono::Utc>> = LazyLock::new(|| {
    chrono::DateTime::parse_from_rfc3339("2019-07-19T09:25:47+00:00").unwrap()
        .to_utc()
});

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub struct MojmapIdent(pub String);

impl std::fmt::Display for MojmapIdent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Classes names like net.minecraft.network.protocol.Packet
#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub struct MojmapIdentPath(pub Vec<MojmapIdent>);

impl MojmapIdentPath {
    pub fn eq_str(&self, other: &str) -> bool {
        other.split('.')
            .zip(self.0.iter())
            .all(|(a, MojmapIdent(b))| a == b)
    }
}

impl std::fmt::Display for MojmapIdentPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut iter = self.0.iter();
        if let Some(first) = iter.next() {
            write!(f, "{first}")?;
        }
        for other in iter {
            write!(f, ".{other}")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub struct MojmapType {
    pub ident: MojmapIdentPath,
    pub array_depth: usize,
}

impl std::fmt::Display for MojmapType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ident)?;
        for _ in 0..self.array_depth {
            write!(f, "[]")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub struct MojmapFieldMapping {
    pub line_range: Option<RangeInclusive<usize>>,

    pub ty: MojmapType,
    pub name: MojmapIdentPath,

    pub obfuscated_name: MojmapIdentPath,
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub struct MojmapMethodMapping {
    pub line_range: Option<RangeInclusive<usize>>,

    pub return_type: MojmapType,
    pub name: MojmapIdentPath,
    pub arguments: Vec<MojmapType>,

    pub obfuscated_name: MojmapIdentPath,
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode, derive_more::From, derive_more::TryInto)]
#[try_into(ref)]
pub enum MojmapItemMapping {
    Field(MojmapFieldMapping),
    Method(MojmapMethodMapping),
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub struct MojmapClassMapping {
    pub name: MojmapIdentPath,
    pub obfuscated_name: MojmapIdentPath,
    pub item_mappings: Vec<MojmapItemMapping>,
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub struct Mojmap {
    pub class_mappings: Vec<MojmapClassMapping>,
}

// Built only with mojang mappings in mind and does not try to follow any kind of
// specification or standard, may break if there is any kind of change to the mappings
fn parse_mojmap(mappings: &str) -> Result<Mojmap, nom::Err<nom::error::Error<&str>>> {
    use nom::{
        branch::alt,
        bytes::tag,
        character::{ digit1, one_of, satisfy },
        combinator::{ opt, recognize },
        multi::{ many0, many0_count, many1_count, separated_list0, separated_list1 },
        sequence::{ delimited, separated_pair },
        Parser as _
    };

    let newline = || (
        opt(tag("\r")), tag("\n"),
    );
    let space = || many1_count(one_of(" \t"));

    let comment = || (
        tag("#"), many0_count(is_not("\n")), newline(),
    );

    let ident_start = || satisfy(|c: char| c.is_ascii_alphabetic() || "$_".contains(c));
    let ident_middle = || alt((ident_start(), satisfy(|c: char| c.is_ascii_digit() || c == '-')));
    let ident = || alt((
       recognize((ident_start(), many0_count(ident_middle()))),
       tag("<clinit>"),
       tag("<init>"),
    )).map(String::from).map(MojmapIdent);

    let ident_path = || separated_list1(tag("."), ident()).map(MojmapIdentPath);

    let ty = || (
        ident_path(),
        many0_count(tag("[]")),
    ).map(|(ident, array_depth)| MojmapType { ident, array_depth });

    let map_arrow = || tag("->");

    let line_range = || terminated(separated_pair(digit1(), tag(":"), digit1()), tag(":"))
        .map_res(|(start, end): (&str, &str)| Ok::<_, ParseIntError>(start.parse::<usize>()?..=end.parse::<usize>()?));

    let field_mapping = || (
        opt(line_range()), ty(), space(), ident_path(),
        space(), map_arrow(), space(), ident_path()
    ).map(|h| MojmapFieldMapping {
        line_range: h.0,
        ty: h.1,
        name: h.3,
        obfuscated_name: h.7,
    });
    let method_mapping = || (
        opt(line_range()), ty(), space(), ident_path(),
        delimited(tag("("), separated_list0(tag(","), ty()), tag(")")),
        space(), map_arrow(), space(), ident_path()
    ).map(|h| MojmapMethodMapping {
        line_range: h.0,
        return_type: h.1,
        name: h.3,
        arguments: h.4,
        obfuscated_name: h.8,
    });
    let item_mapping = || delimited(
        space(),
        alt((field_mapping().map(MojmapItemMapping::from), method_mapping().map(MojmapItemMapping::from))),
        newline(),
    );

    let class_mapping = || (
        ident_path(), space(), map_arrow(), space(), ident_path(), tag(":"), newline(),
        many0_count(comment()),
        many0(item_mapping()),
    ).map(|h| MojmapClassMapping {
        name: h.0,
        obfuscated_name: h.4,
        item_mappings: h.8,
    });

    let mojmap = || preceded(
        many0_count(comment()),
        many0(class_mapping()),
    ).map(|class_mappings| Mojmap { class_mappings });

    let (_, mappings): (_, Mojmap) = complete(mojmap()).parse(mappings)?;

    Ok(mappings)
}

#[derive(Debug, bincode::Encode)]
pub struct MojangMappingsExtractor;
impl super::ExtractorKind for MojangMappingsExtractor {
    type Output = Mojmap;

    fn config(&self) -> super::ExtractorConfig {
        // It is not faster to deserialized the parsed mappings then to
        // parse the txt file again (probably because of the size and that the parsing is copy-intensive)
        super::ExtractorConfig { store_output_in_cache: false }
    }
    
    fn name(&self) -> &'static str {
        "mojang_mappings_extractor"
    }

    async fn extract(self, manager: &mut super::ExtractionManager<'_>) -> anyhow::Result<Self::Output> {
        if manager.version().release_time < *MOJMAPS_FIRST_VERSION_TIME {
            return Err(super::VersionNotSupportedError.into());
        }

        // TODO: To avoid reading the bigfile all at once, parsing each class
        // one at a time using nom's streaming parsing?

        let mappings_path = manager.download_asset("server_mappings").await?;
        let mappings_content = fs::read_to_string(&mappings_path).await?;

        let parsed_mappings = tokio::task::spawn_blocking(move || {
            parse_mojmap(&mappings_content)
                .inspect_err(|e| println!("{e:#?}"))
                .map_err(|e| anyhow!("Error parsing mojang mappings: {e}"))
        }).await??;

        Ok(parsed_mappings)
    }
}
