// trait Hidden {
//     type T;
// }

struct Param<P, Q>(P, Q);

impl<Q> Param<(), Q> {
    fn swap(q: Q) -> Self {
        Param((), q)
    }
}

impl<P> Param<P, ()> {
    fn simplify(p: P) -> Self {
        Param(p, ())
    }
}

trait Simplify {
    fn simplify(self) -> Self;
    fn swap(self) -> Self;
}

impl<T> Simplify for T {
    fn simplify(self) -> Self {
        self
    }

    fn swap(self) -> Self {
        self
    }
}

// trait Swap<T> {
//     fn swap(self) -> T;
// }

// impl<P> Swap<Param<P, ()>> for Param<(), P> {
//     fn swap(self) -> Self {
//         Param(self.1, self.0)
//     }
// }

fn main() {
    // let p: Param<bool, ()> = true.simplify().swap();
}
