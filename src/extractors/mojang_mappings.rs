use crate::{mappings, minijvm};

use std::{ num::ParseIntError, sync::LazyLock };

use anyhow::{ anyhow };
use nom::{ bytes::is_not, combinator::complete, sequence::{ preceded, terminated } };
use tokio::fs;

// The first version to release official mapping was '1.14.4' which release on '2019-07-19T09:25:47+00:00'
static MOJMAPS_FIRST_VERSION_TIME: LazyLock<chrono::DateTime<chrono::Utc>> = LazyLock::new(|| {
    chrono::DateTime::parse_from_rfc3339("2019-07-19T09:25:47+00:00").unwrap()
        .to_utc()
});

// Built only with mojang mappings in mind and does not try to follow any kind of
// specification or standard, may break if there is any kind of change to the mappings
fn parse_mojmap(mappings: &str) -> Result<mappings::Mappings, nom::Err<nom::error::Error<&str>>> {
    use nom::{
        branch::alt,
        bytes::tag,
        character::{ digit1, one_of, none_of, satisfy, char },
        combinator::{ opt, recognize },
        multi::{ many0, many0_count, many1_count, separated_list0, separated_list1 },
        sequence::{ delimited, separated_pair },
        Parser as _
    };

    let newline = || (
        opt(char('\r')), char('\n'),
    );
    let space = || many1_count(one_of(" \t"));

    let comment = || (
        char('#'), many0_count(is_not("\n")), newline(),
    );

    let ident_start = || satisfy(|c: char| c.is_ascii_alphabetic() || "$_".contains(c));
    let ident_middle = || alt((ident_start(), satisfy(|c: char| c.is_ascii_digit() || c == '-')));
    let ident = || alt((
       recognize((ident_start(), many0_count(ident_middle()))),
       recognize((char('<'), many0_count(none_of(">")), char('>'))),
    )).map(String::from).map(minijvm::Ident);

    let ident_path = || recognize(separated_list1(char('.'), ident())).map(String::from).map(minijvm::IdentPath::new);

    let ty = || (
        alt((
            tag("byte")   .map(|_| minijvm::TypeDescriptorKind::Byte),
            tag("char")   .map(|_| minijvm::TypeDescriptorKind::Char),
            tag("double") .map(|_| minijvm::TypeDescriptorKind::Double),
            tag("float")  .map(|_| minijvm::TypeDescriptorKind::Float),
            tag("int")    .map(|_| minijvm::TypeDescriptorKind::Int),
            tag("long")   .map(|_| minijvm::TypeDescriptorKind::Long),
            tag("short")  .map(|_| minijvm::TypeDescriptorKind::Short),
            tag("boolean").map(|_| minijvm::TypeDescriptorKind::Boolean),
            tag("void")   .map(|_| minijvm::TypeDescriptorKind::Void),
            ident_path().map(|ident| minijvm::TypeDescriptorKind::Object(ident)),
        )),
        many0_count(tag("[]")),
    ).map(|(ty, array_depth)| minijvm::TypeDescriptor { ty, array_depth });

    let map_arrow = || tag("->");

    let line_range = || terminated(separated_pair(digit1(), char(':'), digit1()), char(':'))
        .map_res(|(start, end): (&str, &str)| Ok::<_, ParseIntError>(start.parse::<usize>()?..=end.parse::<usize>()?));

    let field_mapping = || (
        opt(line_range()), ty(), space(), ident(),
        space(), map_arrow(), space(), ident()
    ).map(|h| mappings::Field {
        line_range: h.0,
        descriptor: h.1,
        name: h.3,
        obfuscated_name: h.7,
    });
    let method_mapping = || (
        opt(line_range()), ty(), space(), ident(),
        delimited(char('('), separated_list0(char(','), ty()), char(')')),
        space(), map_arrow(), space(), ident()
    ).map(|h| mappings::Method {
        line_range: h.0,
        name: h.3,
        descriptor: minijvm::MethodDescriptor {
            return_type: h.1,
            args: h.4,
        },
        obfuscated_name: h.8,
    });
    let item_mapping = || delimited(
        space(),
        alt((field_mapping().map(mappings::Item::from), method_mapping().map(mappings::Item::from))),
        newline(),
    );

    let class_mapping = || (
        ident_path(), space(), map_arrow(), space(), ident_path(), char(':'), newline(),
        many0_count(comment()),
        many0(item_mapping()),
    ).map(|h| mappings::Class {
        name: h.0,
        obfuscated_name: h.4,
        item_mappings: h.8,
    });

    let mojmap = || preceded(
        many0_count(comment()),
        many0(class_mapping()),
    ).map(|class_mappings| mappings::Mappings {
        brand: mappings::Brand::Mojmaps,
        class_mappings,
    });

    let (_, mappings): (_, mappings::Mappings) = complete(mojmap()).parse(mappings)?;

    Ok(mappings)
}

#[derive(Debug, bincode::Encode)]
pub struct MojangMappingsExtractor;
impl super::ExtractorKind for MojangMappingsExtractor {
    type Output = mappings::Mappings;

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

        let parsed_mappings = crate::spawn_cpu_bound(move || {
            parse_mojmap(&mappings_content)
                .inspect_err(|e| println!("{e:#?}"))
                .map_err(|e| anyhow!("Error parsing mojang mappings: {e}"))
        }).await??;

        Ok(parsed_mappings)
    }
}
