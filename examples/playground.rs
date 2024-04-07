/// This whole thing here might just work?!
///
use std::marker::PhantomData;

trait Param {
    type InitType;
    fn init(self) -> Self::InitType;
}

struct ParamList<P, T>(P, T);

impl<P: Param, T: Param> Param for ParamList<P, T> {
    type InitType = ParamList<<P as Param>::InitType, <T as Param>::InitType>;
    fn init(self) -> Self::InitType {
        ParamList(self.0.init(), self.1.init())
    }
}

struct ParamInitWrapper<P, F: Fn() -> P> {
    f: F,
    _phantom: PhantomData<P>,
}

impl<P, F: Fn() -> P> ParamInitWrapper<P, F> {
    fn new(f: F) -> Self {
        Self {
            f,
            _phantom: PhantomData,
        }
    }
}

impl<P, F: Fn() -> P> Param for ParamInitWrapper<P, F> {
    type InitType = P;

    fn init(self) -> Self::InitType {
        (self.f)()
    }
}

struct FnWrapper<P, O, F: Fn(P) -> O> {
    f: F,
    _phantom: PhantomData<(P, O)>,
}

impl<P, O, F: Fn(P) -> O> FnWrapper<P, O, F> {
    fn new(f: F) -> Self {
        Self {
            f,
            _phantom: PhantomData,
        }
    }
}

trait PaFn<O> {
    type Param;

    fn take_param(self, p: Self::Param) -> O;
}

impl<P, O, F: Fn(P) -> O> PaFn<O> for FnWrapper<P, O, F> {
    type Param = P;

    fn take_param(self, p: Self::Param) -> O {
        (self.f)(p)
    }
}

fn f(x: f64) -> impl PaFn<f64> {
    FnWrapper::new(move |param| -> f64 {
        let ParamList(w, param): ParamList<f64, _> = param;
        let ParamList(b, param): ParamList<f64, _> = param;
        let return_value = w * x + b;
        let () = param;
        return_value
    })
}

fn relu(x: f64) -> f64 {
    if x > 0. {
        x
    } else {
        0.
    }
}

fn g(x: f64) -> impl PaFn<f64> {
    FnWrapper::new(move |param| -> f64 {
        let ParamList(param, p) = param;
        let y = f(x).take_param(p);
        let return_value = relu(y);
        let () = param;
        return_value
    })
}

fn main() {}
