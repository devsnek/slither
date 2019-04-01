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

pub fn to_string(n: f64) -> String {
    if n.is_nan() {
        return "NAN".to_string();
    }
    if n.is_infinite() {
        return if n > 0f64 { "INFINITY" } else { "-INFINITY" }.to_string();
    }
    let mut buffer = ryu::Buffer::new();
    let s = buffer.format(n);
    if s.ends_with(".0") {
        s[0..(s.len() - 2)].to_string()
    } else {
        s.to_string()
    }
}
