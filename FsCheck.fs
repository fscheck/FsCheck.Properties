module FsCheck.Properties

open FsCheck

/// Defines extensional equality.  This allows concise, point-free, definitions of laws.
/// f(x) == g(x)
/// f <=> g
let (.=.) x y = x = y |@ sprintf "%A = %A" x y
let (<=>) f g x = f x .=. g x

/// Checks whether a function is idempotent.
/// f(f(x)) == f(x)
/// idempotent abs
let idempotent f = f <=> (f >> f)

/// Checks whether a function is involutory (is its own inverse).
/// f(f(x)) = x
/// involutory negate
let involutory f = f >> f <=> id

/// Checks whether a function is the inverse of another function.
/// f(g(x)) = x
/// (fun x -> x - 2) |> inverts ((+) 2)
let areInverts f g = f >> g <=> id

/// Checks whether two functions are commutative (endomorphism in relation to a unary operator).
/// a * b = b * a
/// commutative (+)
let commutative f g = (f >> g) <=> (g >> f)

/// Checks whether a function is invariant over an other function.
/// invariates List.length List.rev
let invariates f g = g >> f <=> f

/// Checks whether a function is cyclic by applying its result to itself within n applications.
///cyclesWithin (fun x -> x / 10) 100
let cycles f n x =
    x = List.foldBack (fun _ y -> f y) [1..n] x

/// Checks whether an binary operator is commutative.
/// a * b = b * a
/// commutative (+)
let commutative' f x y = f x y = f y x

let negate = (*) (-1)

/// Checks whether a function is pointSymmetric.
/// f(-x) == -f(x)
/// pointSymmetric (fun x -> pown x 3) i
let pointSymmetric f = (f >> negate) <=> (negate >> f)

/// Checks whether a function is reflectionSymmetric.
/// f(x) == f(-x)
/// reflectionSymmetric (fun x -> pown x 2) i
let reflectionSymmetric f = negate >> f <=> f

/// Checks whether a function is monotonicIncreasing.
/// x >= y,  f(x) >= f(y)
/// monotonicIncreasing floor i
let monotonicIncreasing f x y = List.contains <| compare (f x) (f y) <| [0; compare x y]

/// Checks whether a function is strictly monotonicIncreasing.
/// x > y,  f(x) > f(y)
/// monotonicIncreasing' ((+) 1) i
let monotonicIncreasing' f x y = compare (f x) (f y) = compare x y

/// Checks whether a function is monotonicDecreasing.
/// x >= y,  f(x) <= f(y)
/// monotonicDecreasing (negate >> float >> floor) i
let monotonicDecreasing f x y = List.contains <| compare (f x) (f y) <| [0; compare y x]

/// Checks whether a function is strictly monotonicDecreasing'.
/// x > y,  f(x) < f(y)
/// monotonicDecreasing' ((-) 1) x y
let monotonicDecreasing' f x y = compare (f x) (f y) = compare y x

/// Checks whether an binary operator is associative.
/// a + (b + c) = (a + b) + c
/// associative (+) x y z
let associative f x y z = f x (f y z) = f (f x y) z

/// Checks whether an operator is left-distributive over an other operator.
/// a * (b + c) = (a * b) + (a * c)
/// distributesLeftOver (*) (+) x y z
let distributesLeftOver f g x y z = f x (g y z) = g (f x y) (f x z)

/// Checks whether an operator is right-distributive over an other operator.
/// (b + c) / a = (b / a) + (c / a)
/// distributesRightOver (*) (+) x y z
let distributesRightOver f g x y z = f (g y z) x = g (f x y) (f x z)

/// Checks whether an operator is distributive over an other operator.
/// a * (b + c) = (a * b) + (a * c) = (b + c) * a
/// distributesOver (*) (+) x y z
let distributesOver f g x y z = (distributesLeftOver f g) x y z && (distributesRightOver f g) x y z

/// Checks whether a function increases the size of a list.
/// let append1 xs = 1 :: xs 
/// inflating' append1 i
let inflating f xs = Seq.length (f xs) >= Seq.length xs

/// Checks whether a function increases strictly the size of a list.
/// let append1 xs = 1 :: xs 
/// inflating' append1 i
let inflating' f xs = Seq.length (f xs) > Seq.length xs

/// Checks whether a function decreases the size of a list.
/// deflating List.tail
let deflating f xs = Seq.length (f xs) <= Seq.length xs

/// Checks whether a function decreases strictly the size of a list.
/// deflating List.tail
let defalting' f xs = Seq.isEmpty xs || Seq.length (f xs) < Seq.length xs

/// Checks whether a binary function is fixed by an argument.
/// f x y == f a y
/// fixedBy (*) 0 x y
let fixedBy f x y z = f x y = f x z
let (|~) x f = fixedBy f x

/// Checks whether a function is an endomorphism in relation to a unary operator.
/// f(g(x)) = g(f(x))
/// endomorphism ((*) 7) abs i
let endomorphism f g = g >> f <=> (f >> g)
let (<?>) = endomorphism

/// Checks whether a function is an endomorphism in relation to a binary operator.
/// f(g(x,y)) = g(f(x),f(y))
/// endomorphism2 (fun x -> pown x 2) (*) x y
let endomorphism2 f g x y = f (g x y) = g (f x) (f y)
let (<??>) = endomorphism2

/// Checks whether a function is an endomorphism in relation to a ternary operator.
/// let (++) x y z = x + y + z
/// endomorphism3 negate (++) x y z
let endomorphism3 f g x y z = f (g x y z) = g (f x) (f y) (f z)
let (<???>) = endomorphism3
