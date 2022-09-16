mod error;
mod sheet;

pub use crate::error::Error::{CyclicDependency, InvalidExpression, Parser};
pub use crate::sheet::SpreadSheet;
