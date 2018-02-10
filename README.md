# FsCheck.Properties
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
let ``Idempotent Property`` i = 
    idempotent abs i
```

## Inverts
One function is the inverse of another function.

```fsharp
/// Checks whether a function is the inverse of another function.
/// f(g(x)) = x
/// quickCheck $ (`div` 2) `inverts` (*2)
let inverts f g = f >> g <=> id
```

Can be tested as:

```fsharp
[<Property>]
let ``Inverts Property`` i =
    inverts (fun x -> x - 2) ((+) 2) i
```

## Commutative
Two functions can be executed in different orders and still produce the same result.

```fsharp
/// Checks whether two functions are commutative (endomorphism in relation to a unary operator).
/// a * b = b * a
/// quickCheck $ commutative (+)
let commutative f g = (f >> g) <=> (g >> f)
```

Can be tested as:

```fsharp
[<Property>]
let ``Take Map Commutative Property`` xs =
    xs <> [] ==> lazy
    commutative (List.take 1) (List.map ((+) 1)) xs
```
