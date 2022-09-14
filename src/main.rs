mod error;
mod sheet;

use crate::error::Error::{CircularDependency, InvalidExpression, Parser};
use crate::sheet::SpreadSheet;

fn main() {
    let mut sheet = SpreadSheet::new();

    let a2 = "1 + 2";
    println!("set A2: {}", a2);
    sheet.set_cell("A2", a2).unwrap();

    let a3 = "10+A2+30+4./5.1 + B4";
    println!("set A3: {}", a3);
    sheet.set_cell("A3", a3).unwrap();

    println!("B4: {:?}", sheet.get_cell("B4"));
    let res = sheet.get_cell("A3").unwrap();
    println!("{} = {}", a3, res);

    let b4 = "1";
    println!("set B4: {}", b4);
    sheet.set_cell("B4", b4).unwrap();

    let res = sheet.get_cell("A3").unwrap();
    println!("{} = {}", a3, res);

    // (Naïve) Circular dep detected
    let err = sheet.set_cell("A3", "1 + A3");
    assert_eq!(err, Err(CircularDependency("A3")));

    // Circular dep (stack overflow)
    // sheet.set_cell("A3", "A1").unwrap();
    // sheet.set_cell("A1", "A3 + 1").unwrap();
    // let res = sheet.get_cell("A3");
    // println!("A3 = {:?}", res);

    // Unary ops
    sheet.set_cell("A3", "+2").unwrap();
    let res = sheet.get_cell("A3").unwrap();
    println!("A3 = {}", res);
    sheet.set_cell("A3", "-2").unwrap();
    let res = sheet.get_cell("A3").unwrap();
    println!("A3 = {}", res);

    // Unary errors
    let err = sheet.set_cell("A3", "+-2");
    assert_eq!(err, Err(InvalidExpression("+-2")));
    let err = sheet.set_cell("A3", "3//2");
    assert!(matches!(err, Err(Parser(_))));
    let err = sheet.set_cell("A3", "**2");
    assert!(matches!(err, Err(Parser(_))));

    // Power
    let a3 = "3**A2+0.1";
    sheet.set_cell("A3", a3).unwrap();
    let res = sheet.get_cell("A3").unwrap();
    println!("{} = {}", a3, res);

    // Parentheses
    let a3 = "3**(A2+0.1)";
    sheet.set_cell("A3", a3).unwrap();
    let res = sheet.get_cell("A3").unwrap();
    println!("{} = {}", a3, res);

    let a3 = "3**((A2+1)*0.1*(1+2))";
    sheet.set_cell("A3", a3).unwrap();
    let res = sheet.get_cell("A3").unwrap();
    println!("{} = {}", a3, res);
    let a3 = "3**(A2+0.1+0.2)";
    sheet.set_cell("A3", a3).unwrap();
    let res = sheet.get_cell("A3").unwrap();
    println!("{} = {}", a3, res);

    // Broken cases
    let a3 = "3**((A2+1)*0.1*(1+2)";
    let err = sheet.set_cell("A3", a3);
    assert!(matches!(err, Err(Parser(_))));
    let _res = sheet.get_cell("A3");
    let a3 = "(A2+1))";
    let err = sheet.set_cell("A3", a3);
    assert!(matches!(err, Err(Parser(_))));
}
