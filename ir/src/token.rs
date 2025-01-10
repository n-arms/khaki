use std::{cmp::Ordering, fmt, hash};

use crate::parsed::Span;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Kind {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftSquare,
    RightSquare,
    Integer,
    UpperIdentifier,
    LowerIdentifier,
    Comma,
    Period,
    Fn,
    Match,
    ThinArrow,
    ThickArrow,
    Equals,
}

#[derive(Clone)]
pub struct Token {
    pub kind: Kind,
    pub span: Span,
}

impl Kind {
    pub fn dummy(self) -> Token {
        Token {
            kind: self,
            span: Span::dummy(),
        }
    }
}

impl From<Kind> for Token {
    fn from(kind: Kind) -> Self {
        Self {
            kind,
            span: Span::dummy(),
        }
    }
}

impl hash::Hash for Token {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.kind.hash(state)
    }
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        self.kind.eq(&other.kind)
    }
}

impl Eq for Token {}

impl PartialOrd for Token {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Token {
    fn cmp(&self, other: &Self) -> Ordering {
        self.kind.cmp(&other.kind)
    }
}

impl fmt::Debug for Kind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Kind::LeftParen => "(",
                Kind::RightParen => ")",
                Kind::LeftBrace => "{",
                Kind::RightBrace => "}",
                Kind::LeftSquare => "[",
                Kind::RightSquare => "]",
                Kind::Integer => "%int%",
                Kind::UpperIdentifier => "%Id%",
                Kind::LowerIdentifier => "%id%",
                Kind::Comma => ",",
                Kind::Period => ".",
                Kind::Fn => "fn",
                Kind::Match => "match",
                Kind::ThinArrow => "->",
                Kind::ThickArrow => "=>",
            }
        )
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.kind.fmt(f)
    }
}
