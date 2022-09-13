use santiago::lexer::LexerRules;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use santiago::grammar::Associativity;
use santiago::grammar::Grammar;
use santiago::parser::Tree;

pub struct SpreadSheet {
    lexer: LexerRules,
    grammar: Grammar<AST>,
    cells: HashMap<String, Rc<Tree<AST>>>,
    cells_cache: RefCell<HashMap<String, f64>>,
    cells_deps: RefCell<HashMap<String, HashSet<String>>>,
    cells_rev_deps: RefCell<HashMap<String, HashSet<String>>>,
}

impl SpreadSheet {
    pub(crate) fn new() -> Self {
        Self {
            lexer: lexer_rules(),
            grammar: grammar(),
            cells: HashMap::new(),
            cells_cache: RefCell::new(HashMap::new()),
            cells_deps: RefCell::new(HashMap::new()),
            cells_rev_deps: RefCell::new(HashMap::new()),
        }
    }

    pub(crate) fn get_cell(&self, cell: &str) -> Option<f64> {
        let cache = self.cells_cache.borrow();
        let value = cache.get(cell);
        if value.is_some() {
            return value.cloned();
        }
        drop(cache);
        let tree = self.cells.get(cell);
        let value = tree.map(|tree| self.eval(&tree.as_abstract_syntax_tree()));
        value.map(|v| {
            let mut cache = self.cells_cache.borrow_mut();
            cache.insert(cell.to_string(), v);
        });
        value
    }

    pub(crate) fn set_cell(&mut self, cell: &str, value: &str) {
        // Lex value
        let lexemes = santiago::lexer::lex(&self.lexer, value).unwrap();

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
        let cell_deps = deps.entry(cell.to_string()).or_insert_with(HashSet::new);
        for lexeme in lexemes.iter() {
            if lexeme.kind == "CELL" {
                if lexeme.raw == cell {
                    panic!("Circular dependency detected: {}", cell);
                }
                cell_deps.insert(lexeme.raw.clone());
                let cell_rev_deps = rev_deps
                    .entry(lexeme.raw.to_string())
                    .or_insert_with(HashSet::new);
                cell_rev_deps.insert(cell.to_string());
            }
        }
        // Parse value
        let parse_trees = santiago::parser::parse(&self.grammar, &lexemes).unwrap();
        let parse_tree = parse_trees.first().unwrap();
        // Store cell
        self.cells.insert(cell.to_string(), parse_tree.clone());
        // Invalidate cache
        let mut cache = self.cells_cache.borrow_mut();
        cache.remove(cell);
        // Invalidate all the dependent cells
        for rev_cell in rev_deps.get(cell).unwrap_or(&HashSet::new()) {
            cache.remove(rev_cell);
        }
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
