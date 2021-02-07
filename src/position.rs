use std::fmt;
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Position {
    pub row: usize,
    pub col: usize,
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "row {}, col {}", self.row, self.col)
    }
}

impl Position {
    pub fn next_col(&mut self) {
        self.col += 1;
    }

    pub fn next_row(&mut self) {
        self.row += 1;
        self.col = 1;
    }
}