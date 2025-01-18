use std::{iter::Peekable, str::CharIndices};

use ir::{
    parsed::Span,
    token::{Kind, Token},
};

pub type Stream<'a> = Peekable<CharIndices<'a>>;

pub fn next_token(stream: &mut Stream) -> Option<Token> {
    let (start, first) = stream.next()?;
    let mut end = start;
    let mut text = String::from(first);
    let kind = match first {
        ',' => Kind::Comma,
        '.' => Kind::Period,
        '(' => Kind::LeftParen,
        ')' => Kind::RightParen,
        '[' => Kind::LeftSquare,
        ']' => Kind::RightSquare,
        '{' => Kind::LeftBrace,
        '}' => Kind::RightBrace,
        '<' => {
            let (index, next) = stream.next()?;
            if next == '|' {
                end = index;
                text.push(next);
                Kind::LeftTuple
            } else {
                return None;
            }
        }
        '|' => {
            let (index, next) = stream.next()?;
            if next == '>' {
                end = index;
                text.push(next);
                Kind::RightTuple
            } else {
                return None;
            }
        }
        ':' => {
            let (index, next) = stream.peek().copied()?;
            if next == ':' {
                stream.next();
                end = index;
                text.push(next);
                Kind::DoubleColon
            } else {
                Kind::Colon
            }
        }
        '-' => {
            let (index, next) = stream.next()?;

            if next == '>' {
                end = index;
                text.push(next);
                Kind::ThinArrow
            } else {
                return None;
            }
        }
        '=' => {
            if let Some((index, next)) = stream.peek().copied() {
                if next == '>' {
                    stream.next();
                    end = index;
                    Kind::ThickArrow
                } else {
                    Kind::Equals
                }
            } else {
                Kind::Equals
            }
        }
        digit if digit.is_ascii_digit() => {
            while let Some((index, char)) = stream.peek().copied() {
                if char.is_ascii_digit() {
                    stream.next();
                    end = index;
                    text.push(char);
                } else {
                    break;
                }
            }
            Kind::Integer
        }
        letter if letter.is_ascii_lowercase() => {
            while let Some((index, char)) = stream.peek().copied() {
                if char == '_' || char.is_ascii_alphanumeric() {
                    stream.next();
                    end = index;
                    text.push(char);
                } else {
                    break;
                }
            }
            match text.as_str() {
                "match" => Kind::Match,
                "fn" => Kind::Fn,
                "enum" => Kind::Enum,
                _ => Kind::LowerIdentifier,
            }
        }
        letter if letter.is_ascii_uppercase() => {
            while let Some((index, char)) = stream.peek().copied() {
                if char == '_' || char.is_ascii_alphanumeric() {
                    stream.next();
                    end = index;
                    text.push(char);
                } else {
                    break;
                }
            }
            match text.as_str() {
                "Int" => Kind::Int,
                _ => Kind::UpperIdentifier,
            }
        }
        space if space.is_ascii_whitespace() => {
            return next_token(stream);
        }
        _ => return None,
    };
    Some(Token {
        kind,
        span: Span { start, end },
    })
}
