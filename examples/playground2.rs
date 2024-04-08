use core::marker::PhantomData;

trait Fallback {
    fn init_param(self) -> Self;
    fn take_param<P>(self, params: P) -> (P, Self);
}

impl<T> Fallback for T {
    fn init_param(self) -> Self {
        self
    }

    fn take_param<P>(self, params: P) -> (P, Self) {
        (params, self)
    }
}

struct ParamUninit<P, F: Fn() -> P> {
    init: F,
    _phantom: PhantomData<P>,
}

impl<P, F: Fn() -> P> ParamUninit<P, F> {
    fn init_param(self) -> P {
        (self.init)()
    }
}

struct PaFn<P>(P);

impl<P> PaFn<P> {
    fn take_param<R>(self, params: (R, P)) -> (R, P) {
        params
    }
}

fn main() {}
