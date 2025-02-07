use chumsky::recursive::recursive;
use chumsky::Parser;
use ir::parsed::Pattern;

use crate::parser::{identifier, paren_list_in, parser, Env};

pub(crate) fn pattern(env: &Env) -> parser!('_, Pattern) {
    recursive(|pattern| {
        let var = identifier(env).map(|name| Pattern::Variable(name, None));
        let tuple = paren_list_in(pattern.clone()).map(|(span, list)| Pattern::Tuple(list, span));
        var.or(tuple)
    })
}
