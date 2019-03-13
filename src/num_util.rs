pub fn f64_shl(a: f64, b: f64) -> f64 {
    ((a as u64) << b as u64) as f64
}

pub fn f64_shr(a: f64, b: f64) -> f64 {
    (a as u64 >> b as u64) as f64
}
