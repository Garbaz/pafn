use std::ops::{Add, Mul};

struct PaFn<'a, P, T>(Box<dyn Fn(P) -> T + 'a>);

impl<'a, P: 'a, T: 'a> PaFn<'a, P, T> {
    fn new<F: Fn(P) -> T + 'a>(f: F) -> Self {
        Self(Box::new(f))
    }

    fn fmap<U: 'a>(self, g: impl Fn(T) -> U + 'a) -> PaFn<'a, P, U> {
        PaFn::new(move |p| g(self.0(p)))
    }

    fn lift<Q: 'a, U: 'a, V: 'a>(
        self,
        other: PaFn<'a, Q, U>,
        h: impl Fn(T, U) -> V + 'a,
    ) -> PaFn<'a, (P, Q), V> {
        PaFn::new(move |(p, q)| h(self.0(p), other.0(q)))
    }

    fn bind<Q: 'a, U: 'a>(self, g: impl Fn(T) -> PaFn<'a, Q, U> + 'a) -> PaFn<'a, (P, Q), U> {
        PaFn::new(move |(p, q)| g(self.0(p)).0(q))
    }
}

impl<'a, T: 'a + Copy> PaFn<'a, (), T> {
    fn pure(x: T) -> Self {
        PaFn::new(move |()| x)
    }
}

impl<'a, P: 'a> PaFn<'a, P, P> {
    fn param() -> Self {
        PaFn::new(move |p: P| p)
    }
}

impl<'a, P: 'a, T: 'a> PaFn<'a, P, T> {
    fn chain<Q: 'a, S, U: 'a>(
        f: impl Fn(S) -> Self + 'a,
        g: impl Fn(T) -> PaFn<'a, Q, U> + 'a + Copy,
    ) -> impl Fn(S) -> PaFn<'a, (P, Q), U> + 'a {
        move |s| f(s).bind(g)
    }
}

// fn bind<'a, P: 'a, Q: 'a, T: 'a, U: 'a>(
//     f: PaFn<'a, P, T>,
//     g: impl Fn(T) -> PaFn<'a, Q, U> + 'a,
// ) -> PaFn<'a, (P, Q), U> {
//     PaFn::new(move |(p, q)| {
//         g(f.0(p)).0(q)
//     })
// }

// fn bind2<'a, P: 'a, Q: 'a, R: 'a, T: 'a, U: 'a, V: 'a>(
//     f: PaFn<'a, P, T>,
//     g: PaFn<'a, Q, U>,
//     h: impl Fn(T, U) -> V + 'a,
// ) -> PaFn<'a, (P, Q), V> {
//     PaFn::new(move |(p, q)| h(f.0(p), g.0(q)))
// }

impl<'a, P: 'a, Q: 'a, T: Add<U> + 'a, U: 'a> Add<PaFn<'a, Q, U>> for PaFn<'a, P, T> {
    type Output = PaFn<'a, (P, Q), <T as Add<U>>::Output>;

    fn add(self, rhs: PaFn<'a, Q, U>) -> Self::Output {
        self.lift(rhs, <T as Add<U>>::add)
    }
}

impl<'a, P: 'a, Q: 'a, T: Mul<U> + 'a, U: 'a> Mul<PaFn<'a, Q, U>> for PaFn<'a, P, T> {
    type Output = PaFn<'a, (P, Q), <T as Mul<U>>::Output>;

    fn mul(self, rhs: PaFn<'a, Q, U>) -> Self::Output {
        self.lift(rhs, <T as Mul<U>>::mul)
    }
}

fn sigmoid_(x: f64) -> f64 {
    0.5 * ((0.5 * x).tanh() + 1.)
}

fn sigmoid<'a, P: 'a>(x: PaFn<'a, P, f64>) -> PaFn<'a, P, f64> {
    PaFn::new(move |p| sigmoid_(x.0(p)))
}

fn layer<'a, P: 'a>(x: PaFn<'a, P, f64>) -> PaFn<((f64, P), f64), f64> {
    let w = PaFn::param();
    let b = PaFn::param();
    sigmoid(w * x + b)
}

// fn sigmoid2(x: f64) -> PaFn<'static, (), f64> {
//     PaFn::pure(sigmoid_(x))
// }

fn layer2<'a>(x: f64) -> PaFn<'a, (f64, f64), f64> {
    let w = PaFn::param();
    let b = PaFn::param();
    let q = w.fmap(move |w| w * x);
    let qq = q.lift(b, |q, b| q + b);
    qq.fmap(sigmoid_)
    // let y = w.fmap(|q| q * x);
    // let yy = y.lift(b, |q, qq| q + qq);
    // yy.fmap(sigmoid_)
}

fn layer3<'a>(x: f64) -> PaFn<'a, (f64, f64), f64> {
    
}

// Why need `Copy`?
// fn arg<'a, T: 'a + Copy>(x: T) -> PaFn<'a, (), T> {
//     PaFn::new(move |()| x)
// }

// fn deparam<'a, P : 'a, S, T, F: Fn(S) -> PaFn<'a, P, T> + 'a>(f: F, p: P) -> impl Fn(S) -> T + 'a {
//     move |x: S| f(x).0(p)
// }

fn main() {}
