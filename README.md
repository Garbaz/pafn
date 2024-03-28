# ***Pa***rametrized ***F***unctio***n***s

The goal here is to create in some form (Rust crate / new programming language / ...) a clean abstraction over functions that have parameters, such as neural networks or probabilistic programs in machine learning.


## The Difference between Parameters and Arguments

Usually the terms *parameters* and *arguments* are treated as interchangeable when talking about functions, however here I would like to differentiate them.

Both parameters and arguments are inputs to a function, in that they have to be provided for the function to compute an output. However, the difference between them is in what way they are handled when we compose two parametrized functions. While arguments compose in series, parameters compose in parallel. Illustrated on an example (using haskell):

```hs
f :: Float -> Float -> Float
f param x = param * x

g :: Bool -> Float -> Float
g param x = if param then x else -x
```

Imagine `f` and `g` here to be say neural network layers that we would like to compose, the argument `x` should first go through the "layer" `g` and then through `f`. But of course we can't just write `f . g`, as both `f` and `g` have a parameter that we will provide at a later point (e.g. during training initialization or from a checkpoint). The composition operator we need instead of the normal `.` is:

```hs
(@) :: (p -> b -> c) -> (p' -> a -> b) -> (p, p') -> a -> c
(@) f g (p, p') x = f p (g p' x)
```

The parameters of `f` and `g` are composed in parallel, while the arguments are composed in series. Using this operator we get:

```hs
h :: (p, p') -> Float -> Float
h = f @ g
```

Note that `h` has the same form as `f` and `g`, taking first it's parameters and second it's argument. It again is a parametrized function that can be composed further with `@`.


## Where are Parametrized Functions to be found?

### Neural Networks are PaFns 

As already indicated, neural networks in machine learning are parametrized functions. For example (using `Float` as a stand-in for some tensor type):

```hs
-- A more intuitive flipped version of `@`
(@>) g f = f @ g

linear :: (Float, Float) -> Float -> Float
linear (weight, bias) x = weight * x + bias

relu :: () -> Float -> Float
relu () x = if x > 0 then x else 0

dense1 :: ((Float, Float), ((), ((Float, Float), ((), (Float, Float))))) -> Float -> Float
dense1 = linear @> relu @> linear @> relu @> linear
```

While the type signature of `dense1` is quite unwieldy (a cause to consider how to implement parametrized functions more ergonomically), it is again a parametrized function with a type signature of the form `p -> a -> b`, allowing us to compose it freely with further parametrized functions.


### Probabilistic Programs are PaFns

A probabilistic program is a function which contains occasions of sampling from probability distributions and conditioning on probability distributions. To use methods such as Markov Chain Monte Carlo to draw representative samples from the overall distribution defined by a probabilistic program, we have to have a way to know about and influence all primitive distributions used throughout the program. This trace of primitive distributions appearing throughout a probabilistic program again has the form of a parameter, composing in parallel.


## Are PaFns just an instance of Monads?

Yes, but also no.

It depends on whether we want to keep track of the type of the parameters. If so, we are no longer in the realm of ordinary monads, but rather *indexed monads*.

## How about effect types?

...