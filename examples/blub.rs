use std::ops::{Add, Mul};

struct PaFn<'a, P, T>(Box<dyn Fn(P) -> T + 'a>);

impl<'a, P: 'a, T: 'a> PaFn<'a, P, T> {
    fn new<F: Fn(P) -> T + 'a>(f: F) -> Self {
        Self(Box::new(f))
    }
}

fn bind<'a, P: 'a, T: 'a, U: 'a>(f: PaFn<'a, P, T>, g: impl Fn(T) -> U + 'a) -> PaFn<'a, P, U> {
    PaFn::new(move |p| g(f.0(p)))
}

fn bind2<'a, P: 'a, Q: 'a, T: 'a, U: 'a, V: 'a>(
    f: PaFn<'a, P, T>,
    g: PaFn<'a, Q, U>,
    h: impl Fn(T, U) -> V + 'a,
) -> PaFn<'a, (P, Q), V> {
    PaFn::new(move |(p, q)| h(f.0(p), g.0(q)))
}

impl<'a, P: 'a, Q: 'a, T: Add<U> + 'a, U: 'a> Add<PaFn<'a, Q, U>> for PaFn<'a, P, T> {
    type Output = PaFn<'a, (P, Q), <T as Add<U>>::Output>;

    fn add(self, rhs: PaFn<'a, Q, U>) -> Self::Output {
        bind2(self, rhs, <T as Add<U>>::add)
    }
}

impl<'a, P: 'a, Q: 'a, T: Mul<U> + 'a, U: 'a> Mul<PaFn<'a, Q, U>> for PaFn<'a, P, T> {
    type Output = PaFn<'a, (P, Q), <T as Mul<U>>::Output>;

    fn mul(self, rhs: PaFn<'a, Q, U>) -> Self::Output {
        bind2(self, rhs, <T as Mul<U>>::mul)
    }
}

fn param<'a, P: 'a>() -> PaFn<'a, P, P> {
    PaFn::new(|p: P| p)
}

fn sigmoid_(x: f64) -> f64 {
    0.5 * ((0.5 * x).tanh() + 1.)
}

fn sigmoid<'a, P: 'a>(x: PaFn<'a, P, f64>) -> PaFn<'a, P, f64> {
    PaFn::new(move |p| sigmoid_(x.0(p)))
}

fn layer<'a, P: 'a>(x: PaFn<'a, P, f64>) -> PaFn<((f64, P), f64), f64> {
    let w = param();
    let b = param();
    sigmoid(w * x + b)
}

// Why need `Copy`?
// fn arg<'a, T: 'a + Copy>(x: T) -> PaFn<'a, (), T> {
//     PaFn::new(move |()| x)
// }

// fn deparam<'a, P : 'a, S, T, F: Fn(S) -> PaFn<'a, P, T> + 'a>(f: F, p: P) -> impl Fn(S) -> T + 'a {
//     move |x: S| f(x).0(p)
// }

fn main() {}
