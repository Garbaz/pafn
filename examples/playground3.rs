use std::marker::PhantomData;

struct Rng;

trait Param {
    fn init(rng: &mut Rng) -> Self;
}

trait PaFn<A, O> {
    type Param: Param;
    // type Func: Fn(A) -> O;

    fn params(&self) -> &Self::Param;
    fn param_mut(&mut self) -> &mut Self::Param;

    fn call(&self, args: A) -> O;
}

struct PaFnWrapper<P: Param, A, O, F: Fn(&P, A) -> O> {
    pafn: F,
    params: P,
    _phantom: PhantomData<(A, O)>,
}

impl<P: Param, A, O, F: Fn(&P, A) -> O> PaFnWrapper<P, A, O, F> {
    fn new(rng: &mut Rng, pafn: F) -> Self {
        Self {
            pafn,
            params: P::init(rng),
            _phantom: PhantomData,
        }
    }
}

impl<P: Param, A, O, F: Fn(&P, A) -> O> PaFn<A, O> for PaFnWrapper<P, A, O, F> {
    type Param = P;
    // type Func = F;

    fn params(&self) -> &Self::Param {
        &self.params
    }

    fn param_mut(&mut self) -> &mut Self::Param {
        &mut self.params
    }

    // fn func(&self) -> Self::Func {
    //     (self.pafn)(&self.params)
    // }

    fn call(&self, args: A) -> O {
        (self.pafn)(&self.params, args)
    }
}

struct ParamList<H: Param, T: Param>(H, T);

impl<H: Param, T: Param> Param for ParamList<H, T> {
    fn init(rng: &mut Rng) -> Self {
        Self(H::init(rng), T::init(rng))
    }
}

impl Param for () {
    fn init(rng: &mut Rng) -> Self {}
}

struct MyF64Param(f64);

impl Param for MyF64Param {
    fn init(rng: &mut Rng) -> Self {
        MyF64Param(17.29)
    }
}

// #[pafn]
// fn linear(x: f64) -> f64
fn linear(rng: &mut Rng) -> impl PaFn<(f64,), f64> {
    PaFnWrapper::new(rng, |params, args: (f64,)| -> f64 {
        let (x,) = args;
        // param!(w: MyF64Param);

        let ParamList(w, params): &ParamList<MyF64Param, _> = params;

        // param!(b: MyF64Param)
        let ParamList(b, params): &ParamList<MyF64Param, _> = params;

        let ret = w.0 * x + b.0;

        let () = params;
        ret
    })
}

fn main() {}
