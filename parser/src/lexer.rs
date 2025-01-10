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
        '-' => {
            let (index, next) = stream.next()?;

            if next == '>' {
                end = index;
            }
            text.push(next);
            Kind::ThinArrow
        }
        '=' => {
            if let Some((index, next)) = stream.peek() {
                if next == '>' {
                    stream.next();
                    Kind::ThickArrow
                } else {
                    Kind::Equals
                }
            } else {
                Kind::Equals
            }
        }
        digit if digit.is_ascii_digit() => {
            while let Some((index, char)) = stream.peek() {
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
            while let Some((index, char)) = stream.peek() {
                if char == '_' || char.is_ascii_alphanumeric() {
                    stream.next();
                    end = index;
                    text.push(char);
                } else {
                    break;
                }
            }
            match &text {
                "match" => Kind::Match,
                "fn" => Kind::Fn,
                _ => Kind::LowerIdentifier,
            }
        }
        letter if letter.is_ascii_uppercase() => {
            while let Some((index, char)) = stream.peek() {
                if char == '_' || char.is_ascii_alphanumeric() {
                    stream.next();
                    end = index;
                    text.push(char);
                } else {
                    break;
                }
            }
            Kind::UpperIdentifier
        }
        space if space.is_ascii_whitespace() => {
            return next_token(stream);
        }
    };
    Some(Token {
        kind,
        span: Span { start, end, text },
    })
}
