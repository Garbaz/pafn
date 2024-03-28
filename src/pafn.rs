pub enum Primitive {
    Add,
    Mul,
    Neg,
    Sin,
    Cos,
    ReduceSum,
    Greater,
    Less,
    Transpose,
    Broadcast,
}

fn add<X>(x: X, y: X) -> _ {
    bind(Primitive::Add, (x, y))
}
fn mul<X>(x: X, y: X) -> _ {
    bind(Primitive::Mul, (x, y))
}
fn neg<X>(x: X) -> _ {
    bind(Primitive::Neg, x)
}
fn sin<X>(x: X) -> _ {
    bind(Primitive::Sin, x)
}
fn cos<X>(x: X) -> _ {
    bind(Primitive::Cos, x)
}
// fn reducesum<X, A>(x: X, axis: Option<A>) -> _ {
//     match axis {
//         None => ,
//         Some(_) => todo!(),
//     }
// }
fn greater<X>(x: X, y: X) -> _ {
    bind(Primitive::Greater, x, y)
}
fn less<X>(x: X, y: X) -> _ {
    bind(Primitive::Less, x, y)
}
fn transpose<X,P>(x: X, perm: P) -> _ {
    bind(Primitive::Transpose, (x, perm))
}
fn broadcast<X, S, A>(x: X, shape: S, axes: A) -> _ {
    bind(Primitive::Broadcast, (x, shape, axes))
}
