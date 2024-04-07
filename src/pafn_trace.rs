trait Fallback {
    fn trace(self) -> Self;
}

impl<T> Fallback for T {
    fn trace(self) -> Self {
        self
    }
}

fn f(x: f64) -> f64 {
    x.cos().sin()
}

struct GTrace(f64);

struct G(fn(f64) -> f64);

fn _g(x: f64) -> f64 {
    x.sin().cos()
}

const g: G = G(_g);

impl G {
    fn trace(self) -> fn(f64) -> f64 {
        self.0
    }
}

fn h() -> f64 {
    let a = 17.29;
    let b = (f.trace())(a);
    let c = (g.trace())(b);
    a + b + c
}
