pub fn f64_shl(a: f64, b: f64) -> f64 {
    ((a as i64) << b as i64) as f64
}

pub fn f64_shr(a: f64, b: f64) -> f64 {
    (a as i64 >> b as i64) as f64
}

pub fn f64_bor(a: f64, b: f64) -> f64 {
    (a as i64 | b as i64) as f64
}

pub fn f64_bxor(a: f64, b: f64) -> f64 {
    (a as i64 ^ b as i64) as f64
}

pub fn f64_band(a: f64, b: f64) -> f64 {
    (a as i64 & b as i64) as f64
}

pub fn f64_bnot(a: f64) -> f64 {
    !(a as i64) as f64
}
