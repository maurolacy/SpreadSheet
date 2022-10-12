use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use santiago::grammar::Associativity;
use santiago::grammar::Grammar;
use santiago::lexer::{Lexeme, LexerRules};
use santiago::parser::Tree;

use crate::error::Error;
use crate::error::Error::{
    CyclicDependency, InvalidCellName, InvalidExpression, InvalidNumericLiteral,
};

#[derive(Debug)]
pub enum AST {
    Formula(Vec<AST>),
    FormulaStart,
    Cell(String),
    Float(f64),
    Int(i64),
    UnaryOperation(Vec<AST>),
    BinaryOperation(Vec<AST>),
    OperatorAdd,
    OperatorSubtract,
    OperatorMultiply,
    OperatorDivide,
    OperatorPotentiate,
    Parentheses(Vec<AST>),
    LeftParenthesis,
    RightParenthesis,
}

const CELL_NAME_PATTERN: &str = r"[A-Za-z]{1,2}[1-9][0-9]*";

pub enum CellValue {
    Literal(String),
    Tree(Rc<Tree<AST>>),
}

pub struct SpreadSheet {
    cell_name_lexer: LexerRules,
    lexer: LexerRules,
    grammar: Grammar<AST>,
    cells: HashMap<String, CellValue>,
    cells_cache: RefCell<HashMap<String, f64>>,
    cells_children: HashMap<String, HashSet<String>>,
    cells_parents: HashMap<String, HashSet<String>>,
}

impl Default for SpreadSheet {
    fn default() -> Self {
        Self::new()
    }
}

impl SpreadSheet {
    pub fn new() -> Self {
        Self {
            cell_name_lexer: santiago::lexer_rules!(
                "DEFAULT" | "CELL" = pattern CELL_NAME_PATTERN;
            ),
            lexer: santiago::lexer_rules!(
                "DEFAULT" | "FORMULA" = string "=" => |lexer| {
                    lexer.push_state("INSIDE_FORMULA");
                    lexer.take()
                };
                "INSIDE_FORMULA" | "CELL" = pattern CELL_NAME_PATTERN;
                "INSIDE_FORMULA" | "FLOAT" = pattern r"[0-9]+\.[0-9]*";
                "INSIDE_FORMULA" | "INT" = pattern r"[0-9]+";
                "INSIDE_FORMULA" | "+" = string "+";
                "INSIDE_FORMULA" | "-" = string "-";
                "INSIDE_FORMULA" | "*" = string "*";
                "INSIDE_FORMULA" | "/" = string "/";
                "INSIDE_FORMULA" | "**" = string "**";
                "INSIDE_FORMULA" | "(" = string "(";
                "INSIDE_FORMULA" | ")" = string ")";
                "INSIDE_FORMULA" | "WS" = pattern r"\s" => |lexer| lexer.skip();

                "DEFAULT" | "INT_LITERAL" = pattern r"[+-]{0,1}[0-9]+";
                "DEFAULT" | "FLOAT_LITERAL" = pattern r"[+-]{0,1}[0-9]+\.[0-9]*";

                "INSIDE_FORMULA" | "" = string "" => |lexer| {
                    lexer.pop_state();
                    lexer.skip()
                };
            ),
            grammar: santiago::grammar!(
                "full_expr" => rules "formula" "expr" => AST::Formula;
                "full_expr" => rules "int_literal" => AST::Formula;
                "full_expr" => rules "float_literal" => AST::Formula;

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
                "expr" => rules "expr" "power" "expr"=>
                    AST::BinaryOperation;

                "expr" => rules "unary_add" "expr" =>
                    AST::UnaryOperation;
                "expr" => rules "unary_subtract" "expr" =>
                    AST::UnaryOperation;

                "unary_add" => lexemes "+" =>
                    |_| AST::OperatorAdd;
                "unary_subtract" => lexemes "-" =>
                    |_| AST::OperatorSubtract;
                "add" => lexemes "+" =>
                    |_| AST::OperatorAdd;
                "subtract" => lexemes "-" =>
                    |_| AST::OperatorSubtract;
                "multiply" => lexemes "*" =>
                    |_| AST::OperatorMultiply;
                "divide" => lexemes "/" =>
                    |_| AST::OperatorDivide;
                "power" => lexemes "**" =>
                    |_| AST::OperatorPotentiate;

                "formula" => lexemes "FORMULA" =>
                    |_| {
                        AST::FormulaStart
                    };

                "cell" => lexemes "CELL" =>
                    |lexemes| {
                        AST::Cell(lexemes[0].raw.clone())
                    };
                "float" => lexemes "FLOAT" =>
                    |lexemes| {
                        let value = str::parse(&lexemes[0].raw).unwrap();
                        AST::Float(value)
                    };
                "float_literal" => lexemes "FLOAT_LITERAL" =>
                    |lexemes| {
                        let value = str::parse(&lexemes[0].raw).unwrap();
                        AST::Float(value)
                    };
                "int" => lexemes "INT" =>
                    |lexemes| {
                        let value = str::parse(&lexemes[0].raw).unwrap();
                        AST::Int(value)
                    };
                "int_literal" => lexemes "INT_LITERAL" =>
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
                Associativity::Right => rules "unary_add" "unary_subtract";
                Associativity::Left => rules "power";
            ),
            cells: HashMap::new(),
            cells_cache: RefCell::new(HashMap::new()),
            cells_children: HashMap::new(),
            cells_parents: HashMap::new(),
        }
    }

    pub fn get_cell(&self, cell: &str) -> Result<String, Error> {
        // Lex name
        santiago::lexer::lex(&self.cell_name_lexer, cell)
            .map_err(|_| InvalidCellName(cell.to_string()))?;

        let cache = self.cells_cache.borrow();
        let value = cache.get(cell);
        if let Some(v) = value {
            return Ok(v.to_string());
        }
        drop(cache);

        let cell_value = self.cells.get(cell);

        cell_value
            .map(|value| match value {
                CellValue::Literal(lit) => Ok(lit.to_owned()),
                CellValue::Tree(tree) => {
                    let value = self.eval(&tree.as_abstract_syntax_tree())?;
                    let mut cache = self.cells_cache.borrow_mut();
                    cache.insert(cell.to_string(), value);
                    Ok(value.to_string())
                }
            })
            .unwrap_or_else(|| Ok("0".to_string()))
    }

    pub fn set_cell<'a>(&mut self, cell: &'a str, value: &'a str) -> Result<(), Error<'a>> {
        // Lex name
        santiago::lexer::lex(&self.cell_name_lexer, cell)
            .map_err(|_| InvalidCellName(cell.to_string()))?;

        // Lex value
        // If it cannot be lexed, take it as a string literal
        let res = santiago::lexer::lex(&self.lexer, value);
        let lexemes = if let Ok(lexemes) = res {
            lexemes
        } else {
            self.cells
                .insert(cell.to_string(), CellValue::Literal(value.to_string()));
            return Ok(());
        };

        // Try to parse value
        let res = santiago::parser::parse(&self.grammar, &lexemes);
        // If it cannot be parsed, take it as a string literal (with exceptions)
        let parse_tree = if let Ok(tree) = res {
            tree
        } else {
            // FIXME: Proper error handling / error types differentiation
            let err = res.unwrap_err().to_string();
            println!("Expr: {}, Parser error: {}", value, err);
            // Error on broken parentheses
            if err.starts_with("At: )") {
                return Err(InvalidExpression(value));
            }
            // Error on broken formula
            if err.starts_with("At: INT ") {
                return Err(InvalidExpression(value));
            }
            // Error on broken formula
            if err.starts_with("At: FLOAT ") {
                return Err(InvalidExpression(value));
            }
            self.cells
                .insert(cell.to_string(), CellValue::Literal(value.to_string()));
            return Ok(());
        };
        if parse_tree.len() != 1 {
            return Err(InvalidExpression(value));
        }
        let parse_tree = parse_tree.first().unwrap();

        // Backup cell's children and parents
        let old_children = self.cells_children.get(cell).cloned();
        let old_parents = self.cells_parents.clone();

        // Update dependency graph
        self.dependencies(cell, &lexemes);

        // Check the dependency graph for cycles
        let res = self.cyclic(cell);
        // Restore children and parents if cyclic
        if res.is_err() {
            self.cells_children
                .insert(cell.to_string(), old_children.unwrap_or_default());
            self.cells_parents = old_parents;
            return res;
        }
        drop(old_children);
        drop(old_parents);

        // Store cell's syntax tree
        self.cells
            .insert(cell.to_string(), CellValue::Tree(parse_tree.clone()));

        // Check the parents sub-tree and invalidate cached values
        self.invalidate(cell);
        Ok(())
    }

    fn dependencies(&mut self, cell: &str, lexemes: &[Rc<Lexeme>]) {
        // First, remove this cell from all of its parents (cache disassociation)
        for c in self.cells_children.get(cell).unwrap_or(&HashSet::new()) {
            self.cells_parents
                .get_mut(c)
                .map(|parents| parents.remove(cell));
        }

        // Reset children
        self.cells_children.insert(cell.to_string(), HashSet::new());
        // Then, add all the new cells to this cell's children and parents maps
        let cell_children = self.cells_children.get_mut(cell).unwrap();
        for lexeme in lexemes.iter() {
            if lexeme.kind == "CELL" {
                cell_children.insert(lexeme.raw.clone());
                let cell_parents = self
                    .cells_parents
                    .entry(lexeme.raw.to_string())
                    .or_insert_with(HashSet::new);
                cell_parents.insert(cell.to_string());
            }
        }
    }

    fn cyclic<'a>(&self, cell: &'a str) -> Result<(), Error<'a>> {
        let empty = HashSet::new();
        let mut visited = HashSet::new();
        let mut stack = vec![cell];
        while let Some(c) = stack.pop() {
            if visited.contains(c) {
                return Err(CyclicDependency(cell));
            }
            visited.insert(c);
            for c in self.cells_children.get(c).unwrap_or(&empty) {
                stack.push(c);
            }
        }
        Ok(())
    }

    fn invalidate(&mut self, cell: &str) {
        let mut cache = self.cells_cache.borrow_mut();
        let mut to_invalidate = HashSet::new();
        to_invalidate.insert(cell.to_string());
        while !to_invalidate.is_empty() {
            let cell = to_invalidate.iter().next().unwrap().clone();
            to_invalidate.remove(&cell);
            cache.remove(&cell);
            for cell in self.cells_parents.get(&cell).unwrap_or(&HashSet::new()) {
                to_invalidate.insert(cell.clone());
            }
        }
    }

    pub fn eval(&self, value: &AST) -> Result<f64, Error> {
        match value {
            AST::Int(int) => Ok(*int as _),
            AST::Float(float) => Ok(*float),
            AST::Cell(cell) => self
                .get_cell(cell)?
                .parse()
                .map_err(|_| InvalidNumericLiteral(cell.to_owned())),
            AST::BinaryOperation(args) => match &args[1] {
                AST::OperatorAdd => Ok(self.eval(&args[0])? + self.eval(&args[2])?),
                AST::OperatorSubtract => Ok(self.eval(&args[0])? - self.eval(&args[2])?),
                AST::OperatorMultiply => Ok(self.eval(&args[0])? * self.eval(&args[2])?),
                AST::OperatorDivide => Ok(self.eval(&args[0])? / self.eval(&args[2])?),
                AST::OperatorPotentiate => {
                    Ok(f64::powf(self.eval(&args[0])?, self.eval(&args[2])?))
                }
                _ => unreachable!(),
            },
            AST::UnaryOperation(args) => match &args[0] {
                AST::OperatorAdd => self.eval(&args[1]),
                AST::OperatorSubtract => Ok(-self.eval(&args[1])?),
                _ => unreachable!(),
            },
            AST::Parentheses(args) => self.eval(&args[1]),
            AST::Formula(args) => match args.len() {
                1 => self.eval(&args[0]),
                2 => self.eval(&args[1]),
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::error::Error;

    #[test]
    fn sheet_formula_works() {
        let mut sheet = SpreadSheet::new();

        let a2 = "=1 + 2";
        sheet.set_cell("A2", a2).unwrap();
        let res = sheet.get_cell("A2").unwrap();
        assert_eq!(res, "3");
    }

    #[test]
    fn sheet_formula_breaks() {
        let mut sheet = SpreadSheet::new();

        let a1 = "=";
        sheet.set_cell("A1", a1).unwrap();
        let res = sheet.get_cell("A1").unwrap();
        assert_eq!(res, a1);

        let a2 = "=1 + 2=";
        // Errors for robustness
        let err = sheet.set_cell("A2", a2).unwrap_err();
        assert!(matches!(err, InvalidExpression(_)));

        let a3 = "3=1 + 2"; // Taken as a literal
        sheet.set_cell("A3", a3).unwrap();
        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, a3);
    }

    #[test]
    fn sheet_literal_works() {
        let mut sheet = SpreadSheet::new();

        let empty = "";
        sheet.set_cell("A1", empty).unwrap();
        let res = sheet.get_cell("A1").unwrap();
        assert_eq!(res, empty);

        let a2 = "This is a literal TEST";
        sheet.set_cell("A2", a2).unwrap();
        let res = sheet.get_cell("A2").unwrap();
        assert_eq!(res, a2);
    }

    #[test]
    fn sheet_literal_in_formula() {
        let mut sheet = SpreadSheet::new();

        sheet.set_cell("A1", "bullshit").unwrap();
        sheet.set_cell("A2", "= A1 + 1").unwrap();
        let err = sheet.get_cell("A2").unwrap_err();
        assert_eq!(err, InvalidNumericLiteral("A1".to_string()));
    }

    #[test]
    fn sheet_int_literal_works() {
        let mut sheet = SpreadSheet::new();

        let a1 = "123";
        sheet.set_cell("A1", a1).unwrap();
        let res = sheet.get_cell("A1").unwrap();
        assert_eq!(res, "123");

        let a2 = "+123";
        sheet.set_cell("A2", a2).unwrap();
        let res = sheet.get_cell("A2").unwrap();
        assert_eq!(res, "123");

        let a3 = "-123";
        sheet.set_cell("A3", a3).unwrap();
        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, "-123");
    }

    #[test]
    fn sheet_float_literal_works() {
        let mut sheet = SpreadSheet::new();

        let a1 = "123.456";
        sheet.set_cell("A1", a1).unwrap();
        let res = sheet.get_cell("A1").unwrap();
        assert_eq!(res, "123.456");

        let a2 = "+123.456";
        sheet.set_cell("A2", a2).unwrap();
        let res = sheet.get_cell("A2").unwrap();
        assert_eq!(res, "123.456");

        let a3 = "-1231";
        sheet.set_cell("A3", a3).unwrap();
        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, "-1231");
    }

    #[test]
    fn sheet_long_cell_works() {
        let mut sheet = SpreadSheet::new();

        let aa2 = "=3";
        sheet.set_cell("AA2", aa2).unwrap();
        let res = sheet.get_cell("AA2").unwrap();
        assert_eq!(res, "3");

        let zz9999 = "=9999";
        sheet.set_cell("ZZ9999", zz9999).unwrap();
        let res = sheet.get_cell("ZZ9999").unwrap();
        assert_eq!(res, "9999");

        // Invalid names
        let err = sheet.set_cell("A0", "=1").unwrap_err();
        assert_eq!(err, InvalidCellName("A0".to_string()));

        let err = sheet.set_cell("A01", "=1").unwrap_err();
        assert_eq!(err, InvalidCellName("A01".to_string()));

        let err = sheet.set_cell("AAA1", "=1").unwrap_err();
        assert_eq!(err, InvalidCellName("AAA1".to_string()));
    }

    #[test]
    fn sheet_wrong_cell_name_errors() {
        let sheet = SpreadSheet::new();
        let err = sheet.get_cell("AAA1").unwrap_err();
        assert!(matches!(err, InvalidCellName(_)))
    }

    #[test]
    fn sheet_wrong_cell_formula_as_literal() {
        let mut sheet = SpreadSheet::new();

        sheet.set_cell("A1", "=AAA1 + 1").unwrap(); // FIXME? Map invalid cell name to zero
        let res = sheet.get_cell("A1").unwrap();
        assert_eq!(res, "=AAA1 + 1");
    }

    #[test]
    fn sheet_int_works() {
        let mut sheet = SpreadSheet::new();

        for i in &[
            "0", "00", "+0", "-0", "00000", "+0000", "-0000000", "001", "+001", "-001", "10", "1",
            "+1", "-1", "10000", "+1000", "-1000000", "1234", "+1234", "-1234",
        ] {
            sheet.set_cell("I1", &["=", i].concat()).unwrap();
            let res = sheet.get_cell("I1").unwrap();
            assert_eq!(res, i.parse::<f64>().unwrap().to_string());
        }
    }

    #[test]
    fn sheet_float_works() {
        let mut sheet = SpreadSheet::new();

        for f in &[
            "0", "0.", "+0.", "-0.", "0", "+0", "-0", "01", "+01", "-01", "1", "1.", "+1.", "-1.",
            "1", "+1", "-1", "1.234", "+1.234", "-1.234",
        ] {
            sheet.set_cell("F1", &["=", f].concat()).unwrap();
            let res = sheet.get_cell("F1").unwrap();
            assert_eq!(res, f.parse::<f64>().unwrap().to_string());
        }
    }

    #[test]
    fn sheet_unset_cell_defaults_to_zero() {
        let mut sheet = SpreadSheet::new();

        let a3 = "=1 + B4";
        sheet.set_cell("A3", a3).unwrap();

        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, "1");

        sheet.set_cell("B4", "=2").unwrap();

        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, "3");
    }

    #[test]
    fn sheet_cache_invalidation_one_level_works() {
        let mut sheet = SpreadSheet::new();

        let b4 = "=5";
        sheet.set_cell("B4", b4).unwrap();

        let a3 = "=1 + B4";
        sheet.set_cell("A3", a3).unwrap();

        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, "6");

        sheet.set_cell("B4", "=2").unwrap();

        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, "3");
    }

    #[test]
    fn sheet_cache_invalidation_transitivity_works() {
        let mut sheet = SpreadSheet::new();

        let a3 = "=3";
        sheet.set_cell("A3", a3).unwrap();

        let a2 = "=A3 + 3";
        sheet.set_cell("A2", a2).unwrap();

        let a1 = "=A2 + 2";
        sheet.set_cell("A1", a1).unwrap();

        let res = sheet.get_cell("A1").unwrap();
        assert_eq!(res, "8");

        sheet.set_cell("A3", "=4").unwrap();

        let res = sheet.get_cell("A1").unwrap();
        assert_eq!(res, "9");
    }

    #[test]
    fn sheet_unary_ops_work() {
        let mut sheet = SpreadSheet::new();

        // Unary ops
        sheet.set_cell("A3", "=+2").unwrap();
        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, "2");
        sheet.set_cell("A3", "=-2").unwrap();
        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, "-2");
    }

    #[test]
    fn sheet_unary_ops_work_with_parentheses() {
        let mut sheet = SpreadSheet::new();

        // Unary ops
        sheet.set_cell("A3", "=+(2)").unwrap();
        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, "2");
        sheet.set_cell("A3", "=-(2)").unwrap();
        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, "-2");
    }

    #[test]
    fn sheet_unary_ops_work_with_parentheses_and_spaces() {
        let mut sheet = SpreadSheet::new();

        // Unary ops
        sheet.set_cell("A3", "= + (2)").unwrap();
        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, "2");
        sheet.set_cell("A3", "= - (2)").unwrap();
        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, "-2");
    }

    #[test]
    fn sheet_unary_ops_work_with_parentheses_and_spaces_and_operators() {
        let mut sheet = SpreadSheet::new();

        // Unary ops
        sheet.set_cell("A3", "=+ (2 + 2)").unwrap();
        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, "4");
        sheet.set_cell("A3", "= - (2 + 2)").unwrap();
        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, "-4");
    }

    #[test]
    fn sheet_unary_ops_algebraic_works() {
        let mut sheet = SpreadSheet::new();

        // Unary ops
        sheet.set_cell("A3", "=+2 + 2").unwrap();
        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, "4");
        sheet.set_cell("A3", "=-2 + 2").unwrap();
        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, "0");
    }

    #[test]
    fn sheet_unary_right_associative_works() {
        let mut sheet = SpreadSheet::new();

        // Unary works like a binary op with zero to the left
        sheet.set_cell("A3", "=+ +2").unwrap();
        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, "2");

        // Same as binary
        sheet.set_cell("A3", "=0 + +2").unwrap();
        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, "2");
        sheet.set_cell("A3", "=0 + -2").unwrap();
        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, "-2");
        sheet.set_cell("A3", "=0 - -2").unwrap();
        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, "2");
        sheet.set_cell("A3", "=3 + ++2").unwrap();
        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, "5");
        sheet.set_cell("A3", "=3 / + +2").unwrap();
        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, (3. / 2.).to_string());
        sheet.set_cell("A3", "=3 / ++2").unwrap();
        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, (3. / 2.).to_string());
    }

    #[test]
    fn sheet_formula_error_as_literal() {
        let mut sheet = SpreadSheet::new();

        let a2 = "=3//2";
        sheet.set_cell("A2", "=3//2").unwrap();
        let res = sheet.get_cell("A2").unwrap();
        assert_eq!(res, a2);

        let a3 = "=**2";
        sheet.set_cell("A3", a3).unwrap();
        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, a3);
        // TODO? Add more / document behaviour
    }

    #[test]
    fn sheet_division_works() {
        let mut sheet = SpreadSheet::new();

        // Division
        sheet.set_cell("A3", "=2 / 2").unwrap();
        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, "1");
        sheet.set_cell("A3", "=1 / 2").unwrap();
        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, (1.0 / 2.0).to_string());

        sheet.set_cell("A3", "=-1 / 2").unwrap();
        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, (-1.0 / 2.0).to_string());

        sheet.set_cell("A3", "=1 / -3").unwrap();
        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, (1.0 / -3.0).to_string());

        sheet.set_cell("A3", "=1 / 0").unwrap();
        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, f64::INFINITY.to_string());

        sheet.set_cell("A3", "=0 / 0").unwrap();
        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, f64::NAN.to_string());
    }

    #[test]
    fn sheet_power_works() {
        let mut sheet = SpreadSheet::new();

        // Power
        let a3 = "=(-1)**2";
        sheet.set_cell("A3", a3).unwrap();
        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, "1");

        let a3 = "=-1**2";
        sheet.set_cell("A3", a3).unwrap();
        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, "-1");

        let a3 = "=3**2**3";
        sheet.set_cell("A3", a3).unwrap();
        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, "729");

        // Zero powers
        let a3 = "=0**3";
        sheet.set_cell("A3", a3).unwrap();
        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, "0");

        let a3 = "=3**0";
        sheet.set_cell("A3", a3).unwrap();
        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, "1");

        let a3 = "=0**0";
        sheet.set_cell("A3", a3).unwrap();
        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, "1"); // FIXME: Should be indeterminate / NaN

        let a3 = "=3**A2+0.1";
        sheet.set_cell("A2", "=2").unwrap();
        sheet.set_cell("A3", a3).unwrap();
        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, "9.1");
    }

    #[test]
    fn sheet_parentheses_work() {
        let mut sheet = SpreadSheet::new();

        sheet.set_cell("A2", "=3").unwrap();
        // Parentheses
        let a3 = "=3**(A2+1.)";
        sheet.set_cell("A3", a3).unwrap();
        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, "81");

        let a3 = "=3**((A2+1)*0.1*(1+2))";
        sheet.set_cell("A3", a3).unwrap();
        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, "3.7371928188465526");

        let a3 = "=3**(A2+0.1+0.2)";
        sheet.set_cell("A3", a3).unwrap();
        let res = sheet.get_cell("A3").unwrap();
        assert_eq!(res, "37.540507598529565");
    }

    #[test]
    fn sheet_parentheses_syntax_errors() {
        let mut sheet = SpreadSheet::new();

        // Broken parentheses not considered as literals, for robustness
        let err = sheet.set_cell("A3", "=3**((A2+1)*0.1*(1+2)").unwrap_err();
        assert!(matches!(err, Error::InvalidExpression(_)));

        let err = sheet.set_cell("A3", "=(A2+1))").unwrap_err();
        assert!(matches!(err, Error::InvalidExpression(_)));

        let err = sheet.set_cell("A3", "=((A2+1)").unwrap_err();
        assert!(matches!(err, Error::InvalidExpression(_)));
    }

    #[test]
    fn sheet_naive_circular_dep_detected() {
        let mut sheet = SpreadSheet::new();

        // (Naïve) Circular dep detected
        let err = sheet.set_cell("A3", "=1 + A3").unwrap_err();
        assert_eq!(err, CyclicDependency("A3"));
    }

    #[test]
    fn sheet_circular_dep_detected() {
        let mut sheet = SpreadSheet::new();

        // Set a non-trivial circular dep
        sheet.set_cell("A3", "=A1").unwrap();
        let err = sheet.set_cell("A1", "=A3 + 1").unwrap_err();

        assert_eq!(err, CyclicDependency("A1"));
    }
}
