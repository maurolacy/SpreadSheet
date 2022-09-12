mod sheet;

use crate::sheet::SpreadSheet;

fn main() {
    let mut sheet = SpreadSheet::new();

    let a2 = "1 + 2";
    println!("A2: {}", a2);
    sheet.set_cell("A2", a2);

    let a3 = "10+A2+30+4./5.123";
    println!("A3: {}", a3);
    sheet.set_cell("A3", a3);

    let res = sheet.get_cell("A3").unwrap();
    println!("{}= {}", a3, res);
}
