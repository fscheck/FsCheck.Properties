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

Can be tested like:

```fsharp
[<Property>] 
let ``Idepotent Property`` i = 
    idempotent abs i
```
