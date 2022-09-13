mod sheet;

use crate::sheet::SpreadSheet;

fn main() {
    let mut sheet = SpreadSheet::new();

    let a2 = "1 + 2";
    println!("set A2: {}", a2);
    sheet.set_cell("A2", a2);

    let a3 = "10+A2+30+4./5.1 + B4";
    println!("set A3: {}", a3);
    sheet.set_cell("A3", a3);

    println!("B4: {:?}", sheet.get_cell("B4"));
    let res = sheet.get_cell("A3").unwrap();
    println!("{} = {}", a3, res);

    let b4 = "1";
    println!("set B4: {}", b4);
    sheet.set_cell("B4", b4);

    let res = sheet.get_cell("A3").unwrap();
    println!("{} = {}", a3, res);

    // Circular dep detected
    // sheet.set_cell("A3", "1 + A3");

    // Unary ops
    sheet.set_cell("A3", "+2");
    let res = sheet.get_cell("A3").unwrap();
    println!("A3 = {}", res);
    sheet.set_cell("A3", "-2");
    let res = sheet.get_cell("A3").unwrap();
    println!("A3 = {}", res);
}
