use std::ops::{Add, Mul};

trait PaFn<P, T>: Fn(P) -> T {}
impl<P, T, F: Fn(P) -> T> PaFn<P, T> for F {}

// impl<P, Q, S: Add<T>, T, F: PaFn<P, S>, G: PaFn<Q, T>> Add<G> for F {
//     type Output = <S as Add<T>>::Output;

//     fn add(self, rhs: G) -> Self::Output {
//         todo!()
//     }
// }

fn add<P, Q, S: Add<T>, T>(
    x: impl Fn(P) -> S,
    y: impl Fn(Q) -> T,
) -> impl Fn((P, Q)) -> <S as Add<T>>::Output {
    move |(p, q)| x(p) + y(q)
}

fn mul<P, Q, S: Mul<T>, T>(
    x: impl Fn(P) -> S,
    y: impl Fn(Q) -> T,
) -> impl Fn((P, Q)) -> <S as Mul<T>>::Output {
    move |(p, q)| x(p) * y(q)
}

fn param<P>() -> impl Fn(P) -> P {
    |p| p
}

fn sigmoid<P>(x: impl Fn(P) -> f64) -> impl Fn(P) -> f64 {
    fn sigmoid_(x: f64) -> f64 {
        0.5 * ((0.5 * x).tanh() + 1.)
    }

    move |p| sigmoid_(x(p))
}

fn layer<P>(x: impl Fn(P) -> f64) -> impl Fn(((f64, P), f64)) -> f64 {
    let w = param();
    let b = param();
    sigmoid(add(mul(w, x), b))
}

fn main() {
    
}
