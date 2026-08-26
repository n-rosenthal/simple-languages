use crate::common::{
    Scanner,
    SourceLine,
};

use super::lexer::LexError;

/// Scanner da linguagem `arith`.
///
/// O scanner é responsável apenas por preservar a divisão do
/// código-fonte em linhas.
pub struct ArithScanner;

impl Scanner for ArithScanner {
    type Error = LexError;

    fn scan(
        input: &str,
    ) -> Result<Vec<SourceLine>, Self::Error> {
        Ok(
            input
                .lines()
                .enumerate()
                .map(|(index, text)| {
                    SourceLine::new(
                        index + 1,
                        text,
                    )
                })
                .collect()
        )
    }
}
