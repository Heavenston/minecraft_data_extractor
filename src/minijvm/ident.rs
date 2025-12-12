use std::ops::Deref;

fn is_valid_ident(s: &str) -> bool {
    if s.is_empty() {
        return false;
    }

    if s.starts_with('<') && s.ends_with('>') {
        return true;
    }

    let mut chars = s.chars();
    let first = chars.next().unwrap();
    if !(first.is_ascii_alphabetic() || first == '_' || first == '$') {
        return false;
    }

    chars.all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '$' || c == '-')
}

#[derive(derive_more::Debug, Clone, PartialEq, Eq, derive_more::Display, bincode::Encode, bincode::Decode)]
#[debug("Ident({_0:?})")]
pub struct Ident(pub String);

impl Ident {
    pub fn new(value: impl Into<String>) -> Self {
        let str = value.into();
        debug_assert!(is_valid_ident(&str), "Invalid identifier: {:?}", str);
        Self(str)
    }
}

impl From<String> for Ident {
    fn from(value: String) -> Self {
        Self::new(value)
    }
}

impl<'a> From<&'a str> for Ident {
    fn from(value: &'a str) -> Self {
        Self::new(value)
    }
}

/// Classes names like net.minecraft.network.protocol.Packet
#[derive(
    derive_more::Debug, Clone, PartialEq, Eq, derive_more::Display,
    derive_more::Into, bincode::Encode, bincode::Decode,
)]
#[debug("IdentPath({_0:?})")]
pub struct IdentPath(String);

impl IdentPath {
    pub fn new(value: impl Into<String>) -> Self {
        let str = value.into();
        debug_assert!(!str.contains('/'), "IdentPath contains '/': {:?}", str);
        debug_assert!(
            !str.is_empty() && str.split('.').all(|segment| is_valid_ident(segment)),
            "IdentPath contains invalid identifier segments: {:?}",
            str
        );
        Self(str)
    }

    pub fn last_name(&self) -> &str {
        self.0.rsplit('.').next().unwrap_or(&self.0)
    }
}

impl Deref for IdentPath {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl From<String> for IdentPath {
    fn from(value: String) -> Self {
        Self::new(value)
    }
}

impl<'a> From<&'a str> for IdentPath {
    fn from(value: &'a str) -> Self {
        Self::new(value)
    }
}

impl PartialEq<String> for IdentPath {
    fn eq(&self, other: &String) -> bool {
        &self.0 == other
    }
}

impl PartialEq<str> for IdentPath {
    fn eq(&self, other: &str) -> bool {
        self.0 == other
    }
}

impl<'a> PartialEq<&'a str> for IdentPath {
    fn eq(&self, other: &&'a str) -> bool {
        self.0.as_str() == *other
    }
}
