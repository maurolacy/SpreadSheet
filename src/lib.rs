mod error;
mod sheet;

pub use crate::error::Error::{CircularDependency, InvalidExpression, Parser};
pub use crate::sheet::SpreadSheet;
