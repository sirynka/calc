use crate::data::Literal;

const MIXED_TYPE_ERROR: &str = "Mixed type operations are not supported yet";

impl Literal {
    pub fn add(a: Literal, b: Literal) -> Literal {
        match (a, b) {
            (Literal::I64(a), Literal::I64(b)) => Literal::I64(a + b),
            (Literal::F64(a), Literal::F64(b)) => Literal::F64(a + b),
            _ => unimplemented!("{MIXED_TYPE_ERROR}"),
        }
    }

    pub fn sub(a: Literal, b: Literal) -> Literal {
        match (a, b) {
            (Literal::I64(a), Literal::I64(b)) => Literal::I64(a - b),
            (Literal::F64(a), Literal::F64(b)) => Literal::F64(a - b),
            _ => unimplemented!("{MIXED_TYPE_ERROR}"),
        }
    }

    pub fn mul(a: Literal, b: Literal) -> Literal {
        match (a, b) {
            (Literal::I64(a), Literal::I64(b)) => Literal::I64(a * b),
            (Literal::F64(a), Literal::F64(b)) => Literal::F64(a * b),
            _ => unimplemented!("{MIXED_TYPE_ERROR}"),
        }
    }

    pub fn div(a: Literal, b: Literal) -> Literal {
        match (a, b) {
            (Literal::I64(a), Literal::I64(b)) => Literal::I64(a / b),
            (Literal::F64(a), Literal::F64(b)) => Literal::F64(a / b),
            _ => unimplemented!("{MIXED_TYPE_ERROR}"),
        }
    }

    pub fn modulo(a: Literal, b: Literal) -> Literal {
        match (a, b) {
            (Literal::I64(a), Literal::I64(b)) => Literal::I64(a % b),
            (Literal::F64(a), Literal::F64(b)) => Literal::F64(a % b),
            _ => unimplemented!("{MIXED_TYPE_ERROR}"),
        }
    }

    pub fn pow(a: Literal, b: Literal) -> Literal {
        match (a, b) {
            (Literal::I64(a), Literal::I64(b)) => Literal::I64(a.pow(b as u32)),
            (Literal::F64(a), Literal::F64(b)) => Literal::F64(a.powf(b)),
            _ => unimplemented!("{MIXED_TYPE_ERROR}"),
        }
    }

    pub fn is_zero(&self) -> bool {
        match self {
            Literal::I64(num) => *num == 0,
            Literal::F64(num) => *num == 0.0,
        }
    }

    pub fn as_i64(&self) -> i64 {
        match self {
            Literal::I64(num) => *num,
            Literal::F64(num) => *num as i64,
        }
    }
}