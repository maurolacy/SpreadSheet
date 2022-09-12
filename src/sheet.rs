use santiago::lexer::LexerRules;
use std::collections::HashMap;
use std::rc::Rc;

use santiago::grammar::Associativity;
use santiago::grammar::Grammar;
use santiago::parser::Tree;

pub struct SpreadSheet {
    lexer: LexerRules,
    grammar: Grammar<AST>,
    cells: HashMap<String, Rc<Tree<AST>>>,
}

impl SpreadSheet {
    pub(crate) fn new() -> Self {
        Self {
            lexer: lexer_rules(),
            grammar: grammar(),
            cells: HashMap::new(),
        }
    }

    pub(crate) fn get_cell(&self, cell: &str) -> Option<f64> {
        let tree = self.cells.get(cell);
        tree.map(|tree| self.eval(&tree.as_abstract_syntax_tree()))
    }

    pub(crate) fn set_cell(&mut self, cell: &str, value: &str) {
        // Lex value
        let lexemes = santiago::lexer::lex(&self.lexer, value).unwrap();
        // Parse value
        let parse_trees = santiago::parser::parse(&self.grammar, &lexemes).unwrap();
        let parse_tree = parse_trees.first().unwrap();
        // Insert corresponding cell type
        self.cells.insert(cell.to_string(), parse_tree.clone());
    }

    pub fn eval(&self, value: &AST) -> f64 {
        match value {
            AST::Int(int) => *int as _,
            AST::Float(float) => *float,
            AST::Cell(cell) => {
                let tree = self.cells.get(cell);
                tree.map(|tree| self.eval(&tree.as_abstract_syntax_tree()))
                    .unwrap_or_default()
            }
            AST::BinaryOperation(args) => match &args[1] {
                AST::OperatorAdd => self.eval(&args[0]) + self.eval(&args[2]),
                AST::OperatorSubtract => self.eval(&args[0]) - self.eval(&args[2]),
                AST::OperatorMultiply => self.eval(&args[0]) * self.eval(&args[2]),
                AST::OperatorDivide => self.eval(&args[0]) / self.eval(&args[2]),
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }
}

pub fn lexer_rules() -> LexerRules {
    santiago::lexer_rules!(
        "DEFAULT" | "CELL" = pattern r"[A-Z][0-9]+";
        "DEFAULT" | "FLOAT" = pattern r"[0-9]+\.[0-9]*";
        "DEFAULT" | "INT" = pattern r"[0-9]+";
        "DEFAULT" | "+" = string "+";
        "DEFAULT" | "-" = string "-";
        "DEFAULT" | "*" = string "*";
        "DEFAULT" | "/" = string "/";
        "DEFAULT" | "WS" = pattern r"\s" => |lexer| lexer.skip();
    )
}

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
}

pub fn grammar() -> Grammar<AST> {
    santiago::grammar!(
        "expr" => rules "cell";
        "expr" => rules "float";
        "expr" => rules "int";

        "expr" => rules "expr" "add" "expr" =>
            AST::BinaryOperation;
        "expr" => rules "expr" "subtract" "expr"=>
            AST::BinaryOperation;
        "expr" => rules "expr" "multiply" "expr"=>
            AST::BinaryOperation;
        "expr" => rules "expr" "divide" "expr"=>
            AST::BinaryOperation;

        "add" => lexemes "+" =>
            |_| AST::OperatorAdd;
        "subtract" => lexemes "-" =>
            |_| AST::OperatorSubtract;
        "multiply" => lexemes "*" =>
            |_| AST::OperatorMultiply;
        "divide" => lexemes "/" =>
            |_| AST::OperatorDivide;

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

        Associativity::Left => rules "add" "subtract";
        Associativity::Left => rules "multiply" "divide";
    )
}
