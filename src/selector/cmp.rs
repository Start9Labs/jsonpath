use imbl_value::imbl::Vector;
use imbl_value::Value;

pub trait Cmp {
    fn cmp_bool(&self, v1: bool, v2: bool) -> bool;

    fn cmp_f64(&self, v1: f64, v2: f64) -> bool;

    fn cmp_string(&self, v1: &str, v2: &str) -> bool;

    fn cmp_json<'a>(&self, v1: &Vector<&'a Value>, v2: &Vector<&'a Value>) -> Vector<&'a Value>;

    fn default(&self) -> bool {
        false
    }
}

pub struct CmpEq;

impl Cmp for CmpEq {
    fn cmp_bool(&self, v1: bool, v2: bool) -> bool {
        v1 == v2
    }

    fn cmp_f64(&self, v1: f64, v2: f64) -> bool {
        (v1 - v2).abs() == 0_f64
    }

    fn cmp_string(&self, v1: &str, v2: &str) -> bool {
        v1 == v2
    }

    fn cmp_json<'a>(&self, v1: &Vector<&'a Value>, v2: &Vector<&'a Value>) -> Vector<&'a Value> {
        v1.iter().fold(Vector::new(), |acc, a| {
            v2.iter().fold(acc, |mut acc, b| {
                if std::ptr::eq(*a, *b) {
                    acc.push_back(*a);
                }
                acc
            })
        })
    }
}

pub struct CmpNe;

impl Cmp for CmpNe {
    fn cmp_bool(&self, v1: bool, v2: bool) -> bool {
        v1 != v2
    }

    fn cmp_f64(&self, v1: f64, v2: f64) -> bool {
        (v1 - v2).abs() != 0_f64
    }

    fn cmp_string(&self, v1: &str, v2: &str) -> bool {
        v1 != v2
    }

    fn cmp_json<'a>(&self, v1: &Vector<&'a Value>, v2: &Vector<&'a Value>) -> Vector<&'a Value> {
        let mut ret = v1.clone();
        for v in v2 {
            for i in 0..ret.len() {
                if std::ptr::eq(*v, &*ret[i]) {
                    ret.remove(i);
                    break;
                }
            }
        }
        ret
    }

    fn default(&self) -> bool {
        true
    }
}

pub struct CmpGt;

impl Cmp for CmpGt {
    fn cmp_bool(&self, v1: bool, v2: bool) -> bool {
        v1 & !v2
    }

    fn cmp_f64(&self, v1: f64, v2: f64) -> bool {
        v1 > v2
    }

    fn cmp_string(&self, v1: &str, v2: &str) -> bool {
        v1 > v2
    }

    fn cmp_json<'a>(&self, _: &Vector<&'a Value>, _: &Vector<&'a Value>) -> Vector<&'a Value> {
        Vector::new()
    }
}

pub struct CmpGe;

impl Cmp for CmpGe {
    fn cmp_bool(&self, v1: bool, v2: bool) -> bool {
        v1 >= v2
    }

    fn cmp_f64(&self, v1: f64, v2: f64) -> bool {
        v1 >= v2
    }

    fn cmp_string(&self, v1: &str, v2: &str) -> bool {
        v1 >= v2
    }

    fn cmp_json<'a>(&self, _: &Vector<&'a Value>, _: &Vector<&'a Value>) -> Vector<&'a Value> {
        Vector::new()
    }
}

pub struct CmpLt;

impl Cmp for CmpLt {
    fn cmp_bool(&self, v1: bool, v2: bool) -> bool {
        !v1 & v2
    }

    fn cmp_f64(&self, v1: f64, v2: f64) -> bool {
        v1 < v2
    }

    fn cmp_string(&self, v1: &str, v2: &str) -> bool {
        v1 < v2
    }

    fn cmp_json<'a>(&self, _: &Vector<&'a Value>, _: &Vector<&'a Value>) -> Vector<&'a Value> {
        Vector::new()
    }
}

pub struct CmpLe;

impl Cmp for CmpLe {
    fn cmp_bool(&self, v1: bool, v2: bool) -> bool {
        v1 <= v2
    }

    fn cmp_f64(&self, v1: f64, v2: f64) -> bool {
        v1 <= v2
    }

    fn cmp_string(&self, v1: &str, v2: &str) -> bool {
        v1 <= v2
    }

    fn cmp_json<'a>(&self, _: &Vector<&'a Value>, _: &Vector<&'a Value>) -> Vector<&'a Value> {
        Vector::new()
    }
}

pub struct CmpAnd;

impl Cmp for CmpAnd {
    fn cmp_bool(&self, v1: bool, v2: bool) -> bool {
        v1 && v2
    }

    fn cmp_f64(&self, _v1: f64, _v2: f64) -> bool {
        true
    }

    fn cmp_string(&self, v1: &str, v2: &str) -> bool {
        !v1.is_empty() && !v2.is_empty()
    }

    fn cmp_json<'a>(&self, v1: &Vector<&'a Value>, v2: &Vector<&'a Value>) -> Vector<&'a Value> {
        CmpEq.cmp_json(v1, v2)
    }
}

pub struct CmpOr;

impl Cmp for CmpOr {
    fn cmp_bool(&self, v1: bool, v2: bool) -> bool {
        v1 || v2
    }

    fn cmp_f64(&self, _v1: f64, _v2: f64) -> bool {
        true
    }

    fn cmp_string(&self, v1: &str, v2: &str) -> bool {
        !v1.is_empty() || !v2.is_empty()
    }

    fn cmp_json<'a>(&self, v1: &Vector<&'a Value>, v2: &Vector<&'a Value>) -> Vector<&'a Value> {
        v2.iter().fold(v1.clone(), |mut acc, v| {
            let mut contains = false;

            for ptr in v1.iter().map(|v| *v as *const Value) {
                if ptr == *v as *const Value {
                    contains = true;
                    break;
                }
            }

            if !contains {
                acc.push_back(v);
            }

            acc
        })
    }
}

#[cfg(test)]
mod cmp_inner_tests {
    use std::sync::Arc;

    use imbl_value::Value;

    use imbl_value::imbl::vector;
    use selector::cmp::*;

    #[test]
    fn cmp_eq() {
        let cmp_fn = CmpEq;
        assert!(!cmp_fn.default());
        assert!(!cmp_fn.cmp_bool(true, false));
        assert!(cmp_fn.cmp_bool(true, true));
        assert!(cmp_fn.cmp_f64(0.1, 0.1));
        assert!(!cmp_fn.cmp_f64(0.1, 0.2));
        assert!(cmp_fn.cmp_string("1", "1"));
        assert!(!cmp_fn.cmp_string("1", "2"));
    }

    #[test]
    fn cmp_ne() {
        let cmp_fn = CmpNe;
        assert!(cmp_fn.default());
        assert!(cmp_fn.cmp_bool(true, false));
        assert!(!cmp_fn.cmp_bool(true, true));
        assert!(!cmp_fn.cmp_f64(0.1, 0.1));
        assert!(cmp_fn.cmp_f64(0.1, 0.2));
        assert!(!cmp_fn.cmp_string("1", "1"));
        assert!(cmp_fn.cmp_string("1", "2"));
    }

    #[test]
    fn cmp_gt() {
        let cmp_fn = CmpGt;
        assert!(!cmp_fn.default());
        assert!(cmp_fn.cmp_bool(true, false));
        assert!(!cmp_fn.cmp_bool(true, true));
        assert!(cmp_fn.cmp_f64(0.2, 0.1));
        assert!(!cmp_fn.cmp_f64(0.1, 0.2));
        assert!(!cmp_fn.cmp_string("a", "a"));
        assert!(cmp_fn.cmp_string("b", "a"));
        assert!(!cmp_fn.cmp_string("1", "2"));
    }

    #[test]
    fn cmp_ge() {
        let cmp_fn = CmpGe;
        assert!(!cmp_fn.default());
        assert!(cmp_fn.cmp_bool(true, false));
        assert!(cmp_fn.cmp_bool(true, true));
        assert!(cmp_fn.cmp_f64(0.2, 0.1));
        assert!(cmp_fn.cmp_f64(0.1, 0.1));
        assert!(!cmp_fn.cmp_f64(0.1, 0.2));
        assert!(cmp_fn.cmp_string("1", "1"));
        assert!(cmp_fn.cmp_string("ab", "a"));
        assert!(!cmp_fn.cmp_string("1", "2"));
    }

    #[test]
    fn cmp_lt() {
        let cmp_fn = CmpLt;
        assert!(!cmp_fn.default());
        assert!(!cmp_fn.cmp_bool(true, false));
        assert!(cmp_fn.cmp_bool(false, true));
        assert!(!cmp_fn.cmp_bool(true, true));
        assert!(!cmp_fn.cmp_bool(false, false));
        assert!(cmp_fn.cmp_f64(0.1, 0.2));
        assert!(!cmp_fn.cmp_f64(0.1, 0.1));
        assert!(!cmp_fn.cmp_f64(0.2, 0.1));
        assert!(!cmp_fn.cmp_string("a", "a"));
        assert!(cmp_fn.cmp_string("ab", "b"));
        assert!(cmp_fn.cmp_string("1", "2"));
    }

    #[test]
    fn cmp_le() {
        let cmp_fn = CmpLe;
        assert!(!cmp_fn.default());
        assert!(!cmp_fn.cmp_bool(true, false));
        assert!(cmp_fn.cmp_bool(false, true));
        assert!(cmp_fn.cmp_bool(true, true));
        assert!(cmp_fn.cmp_bool(false, false));
        assert!(cmp_fn.cmp_f64(0.1, 0.2));
        assert!(cmp_fn.cmp_f64(0.1, 0.1));
        assert!(!cmp_fn.cmp_f64(0.2, 0.1));
        assert!(cmp_fn.cmp_string("a", "a"));
        assert!(cmp_fn.cmp_string("ab", "b"));
        assert!(!cmp_fn.cmp_string("abd", "abc"));
        assert!(cmp_fn.cmp_string("1", "2"));
    }

    #[test]
    fn cmp_and() {
        let cmp_fn = CmpAnd;
        assert!(!cmp_fn.default());
        assert!(!cmp_fn.cmp_bool(true, false));
        assert!(!cmp_fn.cmp_bool(false, true));
        assert!(cmp_fn.cmp_bool(true, true));
        assert!(!cmp_fn.cmp_bool(false, false));
        assert!(cmp_fn.cmp_f64(0.0, 0.0));
        assert!(cmp_fn.cmp_string("a", "a"));
    }

    #[test]
    fn cmp_or() {
        let cmp_fn = CmpOr;
        assert!(!cmp_fn.default());
        assert!(cmp_fn.cmp_bool(true, false));
        assert!(cmp_fn.cmp_bool(false, true));
        assert!(cmp_fn.cmp_bool(true, true));
        assert!(!cmp_fn.cmp_bool(false, false));
        assert!(cmp_fn.cmp_f64(0.0, 0.0));
        assert!(cmp_fn.cmp_string("a", "a"));
    }

    #[test]
    fn cmp_json() {
        let v1 = Value::Bool(true);
        let v2 = Value::String(Arc::new("1".to_string()));
        let left = vector![&v1, &v2];
        let right = vector![&v1, &v2];
        let empty: Vector<&Value> = Vector::new();

        assert_eq!(CmpEq.cmp_json(&left, &right), left.clone());
        assert_eq!(CmpNe.cmp_json(&left, &right), empty);
        assert_eq!(CmpGt.cmp_json(&left, &right), empty);
        assert_eq!(CmpGe.cmp_json(&left, &right), empty);
        assert_eq!(CmpLt.cmp_json(&left, &right), empty);
        assert_eq!(CmpLe.cmp_json(&left, &right), empty);
        assert_eq!(CmpAnd.cmp_json(&left, &right), left.clone());
        assert_eq!(CmpOr.cmp_json(&left, &right), left.clone());

        assert_eq!(
            CmpEq.cmp_json(&vector![&Value::Bool(true)], &vector![&Value::Bool(true)]),
            vector![&Value::Bool(true)]
        );
        assert_eq!(
            CmpEq.cmp_json(&vector![&Value::Bool(true)], &vector![&Value::Bool(false)]),
            empty
        );
        assert_eq!(
            CmpNe.cmp_json(&vector![&Value::Bool(true)], &vector![&Value::Bool(true)]),
            empty
        );
        assert_eq!(
            CmpNe.cmp_json(&vector![&Value::Bool(false)], &vector![&Value::Bool(true)]),
            vector![&Value::Bool(false)]
        );
        assert_eq!(
            CmpAnd.cmp_json(&vector![&Value::Bool(true)], &vector![&Value::Bool(true)]),
            vector![&Value::Bool(true)]
        );
        assert_eq!(
            CmpOr.cmp_json(&vector![&Value::Bool(true)], &vector![&Value::Bool(false)]),
            vector![&Value::Bool(true), &Value::Bool(false)]
        );
    }
}
