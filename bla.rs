#[pafn]
fn f(x: Tensor) -> Tensor {
    let w = param!();
    let b = param!();
    sigmoid(w & x + b)
}

#[pafn(w, b)]
fn f(x: Tensor) -> Tensor {
    sigmoid(w & x + b)
}
