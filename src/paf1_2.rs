use std::{marker::PhantomData, ops};

fn comp<X, Y, Z, F, G>(f: F, g: G) -> impl Fn(X) -> Z
where
    F: Fn(Y) -> Z,
    G: Fn(X) -> Y,
{
    move |x| f(g(x))
}

fn comp2<Pf, Pg, X, Y, Z, F, G>(f: F, g: G) -> impl Fn((Pf, Pg), X) -> Z
where
    F: Fn(Pf, Y) -> Z,
    G: Fn(Pg, X) -> Y,
{
    move |(pf, pg), x| f(pf, g(pg, x))
}

struct PAF<P, X, Y, F>
where
    F: Fn(P, X) -> Y,
{
    f: F,
    _phantom: PhantomData<(P, X, Y)>,
}

impl<Pf, Pg, X, Y, Z, F, G> ops::Shr<PAF<Pg, X, Y, G>> for PAF<Pf, Y, Z, F>
where
    F: Fn(Pf, Y) -> Z + 'static,
    G: Fn(Pg, X) -> Y + 'static,
{
    type Output = PAF<(Pf, Pg), X, Z, Box<dyn Fn((Pf, Pg), X) -> Z>>;

    fn shr(self, rhs: PAF<Pg, X, Y, G>) -> Self::Output {
        PAF {
            f: Box::new(move |(pf, pg), x| (self.f)(pf, (rhs.f)(pg, x))),
            _phantom: PhantomData,
        }
    }
}

struct PaFn<P, Y>(Box<dyn Fn(P) -> Y>);

impl<P: 'static, Y: 'static> PaFn<P, Y> {
    pub fn run(&self, p: P) -> Y {
        (self.0)(p)
    }

    pub fn bind<Pg: 'static, Z: 'static, G: Fn(Y) -> PaFn<Pg, Z> + 'static>(self, g: G) -> PaFn<(P, Pg), Z> {
        PaFn(Box::new(move |(p, pg)| {
            g(self.run(p)).run(pg)
        }))
    }
}
