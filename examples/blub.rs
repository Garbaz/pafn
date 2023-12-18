use std::ops::{Add, Mul};

struct PaFn<P, T>(Box<dyn Fn(P) -> T>);

impl<'a, P, T> PaFn<P, T>
where
    Self: 'a,
{
    fn new<F: Fn(P) -> T + 'a + 'static>(f: F) -> Self {
        Self(Box::new(f))
    }
}

impl<P: 'static, Q: 'static, T: 'static, U: 'static> Add<PaFn<Q, U>> for PaFn<P, T>
where
    T: Add<U>,
{
    type Output = PaFn<(P, Q), <T as Add<U>>::Output>;

    fn add(self, rhs: PaFn<Q, U>) -> Self::Output {
        PaFn::new(move |(p, q)| self.0(p) + rhs.0(q))
    }
}

impl<P: 'static, Q: 'static, T: 'static, U: 'static> Mul<PaFn<Q, U>> for PaFn<P, T>
where
    T: Mul<U>,
{
    type Output = PaFn<(P, Q), <T as Mul<U>>::Output>;

    fn mul(self, rhs: PaFn<Q, U>) -> Self::Output {
        PaFn::new(move |(p, q)| self.0(p) * rhs.0(q))
    }
}

fn param<P>() -> PaFn<P, P> {
    PaFn::new(|p: P| p)
}

// Why need `Copy`?
fn arg<T: 'static + Copy>(x: T) -> PaFn<(), T> {
    PaFn::new(move |()| x)
}

fn sigmoid_(x: f64) -> f64 {
    0.5 * ((0.5 * x).tanh() + 1.)
}

fn sigmoid<P: 'static>(x: PaFn<P, f64>) -> PaFn<P, f64> {
    PaFn::new(move |p| sigmoid_(x.0(p)))
}

fn layer<P: 'static>(x: PaFn<P, f64>) -> PaFn<((f64, P), f64), f64> {
    let w = param();
    let b = param();
    sigmoid(w * x + b)
}

// Why need `Copy`?
fn deparam<P: Copy, S, T, F: Fn(S) -> PaFn<P, T>>(f: F, p: P) -> impl Fn(S) -> T {
    move |x: S| f(x).0(p)
}

fn main() {}
