use santiago::lexer::LexerRules;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use crate::error::Error;
use crate::error::Error::{CircularDependency, InvalidExpression};
use santiago::grammar::Associativity;
use santiago::grammar::Grammar;
use santiago::parser::Tree;

#[derive(Debug)]
pub enum AST {
    Cell(String),
    Float(f64),
    Int(i64),
    BinaryOperation(Vec<AST>),
    OperatorAdd,
    OperatorSubtract,
    OperatorMultiply,
    OperatorDivide,
    OperatorPotentiate,
    UnaryOperation(Vec<AST>),
    Parentheses(Vec<AST>),
    LeftParenthesis,
    RightParenthesis,
}

pub struct SpreadSheet {
    lexer: LexerRules,
    grammar: Grammar<AST>,
    cells: HashMap<String, Rc<Tree<AST>>>,
    cells_cache: RefCell<HashMap<String, f64>>,
    cells_deps: RefCell<HashMap<String, HashSet<String>>>,
    cells_rev_deps: RefCell<HashMap<String, HashSet<String>>>,
}

impl Default for SpreadSheet {
    fn default() -> Self {
        Self::new()
    }
}

impl SpreadSheet {
    pub fn new() -> Self {
        Self {
            lexer: santiago::lexer_rules!(
                "DEFAULT" | "CELL" = pattern r"[A-Z][0-9]+";
                "DEFAULT" | "FLOAT" = pattern r"[0-9]+\.[0-9]*";
                "DEFAULT" | "INT" = pattern r"[0-9]+";
                "DEFAULT" | "+" = string "+";
                "DEFAULT" | "-" = string "-";
                "DEFAULT" | "*" = string "*";
                "DEFAULT" | "/" = string "/";
                "DEFAULT" | "**" = string "**";
                "DEFAULT" | "(" = string "(";
                "DEFAULT" | ")" = string ")";
                "DEFAULT" | "WS" = pattern r"\s" => |lexer| lexer.skip();
            ),
            grammar: santiago::grammar!(
                "expr" => rules "cell";
                "expr" => rules "float";
                "expr" => rules "int";

                "expr" => rules "leftp" "expr" "rightp" => AST::Parentheses;

                "expr" => rules "expr" "add" "expr" =>
                    AST::BinaryOperation;
                "expr" => rules "expr" "subtract" "expr"=>
                    AST::BinaryOperation;
                "expr" => rules "expr" "multiply" "expr"=>
                    AST::BinaryOperation;
                "expr" => rules "expr" "divide" "expr"=>
                    AST::BinaryOperation;

                "expr" => rules "add" "expr" =>
                    AST::UnaryOperation;
                "expr" => rules "subtract" "expr" =>
                    AST::UnaryOperation;

                "add" => lexemes "+" =>
                    |_| AST::OperatorAdd;
                "subtract" => lexemes "-" =>
                    |_| AST::OperatorSubtract;
                "multiply" => lexemes "*" =>
                    |_| AST::OperatorMultiply;
                "divide" => lexemes "/" =>
                    |_| AST::OperatorDivide;
                "multiply" => lexemes "**" =>
                    |_| AST::OperatorPotentiate;

                "cell" => lexemes "CELL" =>
                    |lexemes| {
                        AST::Cell(lexemes[0].raw.clone())
                    };
                "float" => lexemes "FLOAT" =>
                    |lexemes| {
                        let value = str::parse(&lexemes[0].raw).unwrap();
                        AST::Float(value)
                    };
                "int" => lexemes "INT" =>
                    |lexemes| {
                        let value = str::parse(&lexemes[0].raw).unwrap();
                        AST::Int(value)
                    };
                "leftp" => lexemes "(" =>
                    |_| {
                        AST::LeftParenthesis
                    };
                "rightp" => lexemes ")" =>
                    |_| {
                        AST::RightParenthesis
                    };

                Associativity::Left => rules "add" "subtract";
                Associativity::Left => rules "multiply" "divide";
            ),
            cells: HashMap::new(),
            cells_cache: RefCell::new(HashMap::new()),
            cells_deps: RefCell::new(HashMap::new()),
            cells_rev_deps: RefCell::new(HashMap::new()),
        }
    }

    pub fn get_cell(&self, cell: &str) -> Option<f64> {
        let cache = self.cells_cache.borrow();
        let value = cache.get(cell);
        if value.is_some() {
            return value.cloned();
        }
        drop(cache);
        let tree = self.cells.get(cell);
        let value = tree.map(|tree| self.eval(&tree.as_abstract_syntax_tree()));
        if let Some(v) = value {
            let mut cache = self.cells_cache.borrow_mut();
            cache.insert(cell.to_string(), v);
        };
        value
    }

    pub fn set_cell<'a>(&mut self, cell: &'a str, value: &'a str) -> Result<(), Error<'a>> {
        // Lex value
        let lexemes = santiago::lexer::lex(&self.lexer, value)?;

        // Parse value
        let parse_tree = santiago::parser::parse(&self.grammar, &lexemes)?;
        if parse_tree.len() != 1 {
            return Err(InvalidExpression(value));
        }
        let parse_tree = parse_tree.first().unwrap();

        // Borrow direct and reverse deps
        let mut deps = self.cells_deps.borrow_mut();
        let mut rev_deps = self.cells_rev_deps.borrow_mut();
        // First, remove all the *currently* (ascendant) dependent cells
        for cell in deps.get(cell).unwrap_or(&HashSet::new()) {
            rev_deps.get_mut(cell).map(|cells| cells.remove(cell));
        }

        // Reset deps
        deps.insert(cell.to_string(), HashSet::new());
        // Then, add all the *newly* (ascendant) dependent cells
        let cell_deps = deps.get_mut(cell).unwrap();
        for lexeme in lexemes.iter() {
            if lexeme.kind == "CELL" {
                if lexeme.raw == cell {
                    return Err(CircularDependency(cell));
                }
                cell_deps.insert(lexeme.raw.clone());
                let cell_rev_deps = rev_deps
                    .entry(lexeme.raw.to_string())
                    .or_insert_with(HashSet::new);
                cell_rev_deps.insert(cell.to_string());
            }
        }

        // Store cell
        self.cells.insert(cell.to_string(), parse_tree.clone());

        // Invalidate cache
        let mut cache = self.cells_cache.borrow_mut();
        cache.remove(cell);
        // Invalidate all the dependent cells
        for rev_cell in rev_deps.get(cell).unwrap_or(&HashSet::new()) {
            cache.remove(rev_cell);
        }
        Ok(())
    }

    pub fn eval(&self, value: &AST) -> f64 {
        match value {
            AST::Int(int) => *int as _,
            AST::Float(float) => *float,
            AST::Cell(cell) => self.get_cell(cell).unwrap_or_default(),
            AST::BinaryOperation(args) => match &args[1] {
                AST::OperatorAdd => self.eval(&args[0]) + self.eval(&args[2]),
                AST::OperatorSubtract => self.eval(&args[0]) - self.eval(&args[2]),
                AST::OperatorMultiply => self.eval(&args[0]) * self.eval(&args[2]),
                AST::OperatorDivide => self.eval(&args[0]) / self.eval(&args[2]),
                AST::OperatorPotentiate => f64::powf(self.eval(&args[0]), self.eval(&args[2])),
                _ => unreachable!(),
            },
            AST::UnaryOperation(args) => match &args[0] {
                AST::OperatorAdd => self.eval(&args[1]),
                AST::OperatorSubtract => -self.eval(&args[1]),
                _ => unreachable!(),
            },
            AST::Parentheses(args) => self.eval(&args[1]),
            _ => unreachable!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::error::Error;

    #[test]
    fn sheet_works() {
        let mut sheet = SpreadSheet::new();

        let a2 = "1 + 2";
        sheet.set_cell("A2", a2).unwrap();
        let res = sheet.get_cell("A2").unwrap();
        assert_eq!(res, 3.0);
    }

    #[test]
    fn sheet_unset_cell_defaults_to_zero() {
        let mut sheet = SpreadSheet::new();

        let a3 = "1 + B4";
        sheet.set_cell("A3", a3).unwrap();

        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, 1.0);

        sheet.set_cell("B4", "2").unwrap();

        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, 3.0);
    }

    #[test]
    fn sheet_unary_ops_work() {
        let mut sheet = SpreadSheet::new();

        // Unary ops
        sheet.set_cell("A3", "+2").unwrap();
        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, 2.0);
        sheet.set_cell("A3", "-2").unwrap();
        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, -2.0);
    }

    #[test]
    fn sheet_unary_ops_work_with_parentheses() {
        let mut sheet = SpreadSheet::new();

        // Unary ops
        sheet.set_cell("A3", "+(2)").unwrap();
        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, 2.0);
        sheet.set_cell("A3", "-(2)").unwrap();
        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, -2.0);
    }

    #[test]
    fn sheet_unary_ops_work_with_parentheses_and_spaces() {
        let mut sheet = SpreadSheet::new();

        // Unary ops
        sheet.set_cell("A3", "+ (2)").unwrap();
        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, 2.0);
        sheet.set_cell("A3", "- (2)").unwrap();
        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, -2.0);
    }

    #[test]
    fn sheet_unary_ops_work_with_parentheses_and_spaces_and_operators() {
        let mut sheet = SpreadSheet::new();

        // Unary ops
        sheet.set_cell("A3", "+ (2 + 2)").unwrap();
        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, 4.0);
        sheet.set_cell("A3", "- (2 + 2)").unwrap();
        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, -4.0);
    }

    #[test]
    fn sheet_unary_ops_algebraic_works() {
        let mut sheet = SpreadSheet::new();

        // Unary ops
        sheet.set_cell("A3", "+2 + 2").unwrap();
        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, 4.0);
        sheet.set_cell("A3", "-2 + 2").unwrap();
        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, 0.0);
    }

    #[test]
    fn sheet_unary_ops_syntax_errors() {
        let mut sheet = SpreadSheet::new();

        // Unary errors
        let err = sheet.set_cell("A3", "+-2");
        assert_eq!(err, Err(InvalidExpression("+-2")));
        let err = sheet.set_cell("A3", "3//2");
        assert!(matches!(err, Err(Error::Parser(_))));
        let err = sheet.set_cell("A3", "**2");
        assert!(matches!(err, Err(Error::Parser(_))));
    }

    #[test]
    fn sheet_power_works() {
        let mut sheet = SpreadSheet::new();

        sheet.set_cell("A2", "2").unwrap();
        // Power
        let a3 = "3**A2+0.1";
        sheet.set_cell("A3", a3).unwrap();
        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, 9.1);
    }

    #[test]
    fn sheet_parentheses_work() {
        let mut sheet = SpreadSheet::new();

        sheet.set_cell("A2", "3").unwrap();
        // Parentheses
        let a3 = "3**(A2+1.)";
        sheet.set_cell("A3", a3).unwrap();
        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, 81.);

        let a3 = "3**((A2+1)*0.1*(1+2))";
        sheet.set_cell("A3", a3).unwrap();
        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, 3.7371928188465526);

        let a3 = "3**(A2+0.1+0.2)";
        sheet.set_cell("A3", a3).unwrap();
        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, 37.540507598529565);
    }

    // Broken parentheses
    #[test]
    fn sheet_parentheses_syntax_errors() {
        let mut sheet = SpreadSheet::new();

        let err = sheet.set_cell("A3", "3**((A2+1)*0.1*(1+2)");
        assert!(matches!(err, Err(Error::Parser(_))));
        let err = sheet.set_cell("A3", "(A2+1))");
        assert!(matches!(err, Err(Error::Parser(_))));
    }

    #[test]
    fn sheet_naive_circular_dep_detected() {
        let mut sheet = SpreadSheet::new();

        // (Naïve) Circular dep detected
        let err = sheet.set_cell("A3", "1 + A3");
        assert_eq!(err, Err(CircularDependency("A3")));
    }

    #[ignore]
    #[test]
    fn sheet_circular_dep_currently_panics() {
        let mut sheet = SpreadSheet::new();

        // Circular dep (stack overflow)
        sheet.set_cell("A3", "A1").unwrap();
        sheet.set_cell("A1", "A3 + 1").unwrap();

        sheet.get_cell("A3");
    }
}
