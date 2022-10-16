#[derive(Debug, PartialEq)]
pub enum Value {
    Num(f64),
    Str(String),
}

impl From<&str> for Value {
    fn from(s: &str) -> Self {
        Self::Str(s.to_string())
    }
}

impl From<&f64> for Value {
    fn from(f: &f64) -> Self {
        Self::Num(*f)
    }
}

impl From<f64> for Value {
    fn from(f: f64) -> Self {
        Self::Num(f)
    }
}

impl From<&i32> for Value {
    fn from(i: &i32) -> Self {
        Self::Num(*i as f64)
    }
}

impl From<i32> for Value {
    fn from(i: i32) -> Self {
        Self::Num(i as f64)
    }
}

impl From<&i64> for Value {
    fn from(i: &i64) -> Self {
        Self::Num(*i as f64)
    }
}

impl From<i64> for Value {
    fn from(i: i64) -> Self {
        Self::Num(i as f64)
    }
}

// TODO: Implement the remaining numeric conversions

impl ToString for Value {
    fn to_string(&self) -> String {
        match self {
            Self::Num(f) => f.to_string(),
            Self::Str(s) => s.to_string(),
        }
    }
}

impl Value {
    pub fn is_num(&self) -> bool {
        matches!(self, Self::Num(_))
    }

    pub fn is_str(&self) -> bool {
        matches!(self, Self::Str(_))
    }

    pub fn as_num(&self) -> Option<f64> {
        match self {
            Self::Num(f) => Some(*f),
            _ => None,
        }
    }

    pub fn as_str(&self) -> Option<&str> {
        match self {
            Self::Str(s) => Some(s.as_ref()),
            _ => None,
        }
    }
}