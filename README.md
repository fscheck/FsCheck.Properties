# FsCheck.Extras
This repository contains some basic Property Functions.

Some examples:

## Idempotent
The result of a function stays the same after running the function multiple times.

```fsharp
/// Defines extensional equality.  This allows concise, point-free, definitions of laws.
/// f(x) == g(x)
/// f <=> g
let (.=.) x y = x = y |@ sprintf "%A = %A" x y
let (<=>) f g x = f x .=. g x

/// Checks whether a function is idempotent.
/// f(f(x)) == f(x)
/// quickCheck $ idempotent (abs :: Int -> Int)
let idempotent f = f <=> (f >> f)
```

Can be tested as:

```fsharp
[<Property>] 
let ``Idepotent Property`` i = 
    idempotent abs i
```

## Inverts
One function is the inverse of another function.

```fsharp
/// Checks whether a function is invariant over an other function.
/// quickCheck $ length `invariatesOver` reverse
let invariates f g = g >> f <=> f
```

Can be tested as:

```fsharp
[<Property>]
let ``Inverts Property`` i =
    inverts (fun x -> x - 2) ((+) 2) i
```
