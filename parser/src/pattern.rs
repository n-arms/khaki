use chumsky::recursive::recursive;
use chumsky::Parser;
use ir::parsed::Pattern;

use crate::parser::{identifier, paren_list_in, parser, Env};

pub(crate) fn pattern<'a>(env: &'a Env) -> parser!('a, Pattern) {
    recursive(|pattern| {
        let var = identifier(env).map(Pattern::Variable);
        let tuple = paren_list_in(pattern.clone()).map(|(span, list)| Pattern::Tuple(list, span));
        var.or(tuple)
    })
}
