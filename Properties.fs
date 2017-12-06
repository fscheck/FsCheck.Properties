module Properties

open FsCheck
open FsCheck.Xunit

[<Property>] 
let ``Idepotent Property`` i = 
    idempotent abs i

[<Property>]
let ``PointSymmatric Property`` i = 
    pointSymmetric (fun x -> pown x 3) i

[<Property>]
let ``ReflectionSymmetric Property`` i = 
    reflectionSymmetric (fun x -> pown x 2) i

[<Property>]
let ``MonotonicIncreading Property`` i = 
    monotonicIncreasing floor i

[<Property>]
let ``MonotonicIncreading' Property`` i = 
    monotonicIncreasing' ((+) 1) i

[<Property>]
let ``MonotonicDecreasing Property`` i = 
    monotonicDecreasing (negate >> float >> floor) i

[<Property>]
let ``MonotonicDecreasing' Property`` x y = 
    monotonicDecreasing' ((-) 1) x y

[<Property>]
let ``Involutary Property`` i = 
    involutory negate i

[<Property>]
let ``Inverts Property`` i =
    inverts (fun x -> x - 2) ((+) 2) i

[<Property>]
let ``Commutative Property`` x y = 
    commutative' (+) x y

[<Property>]
let ``Map Reduce Commutative Property`` xs =
    xs <> [] ==> lazy
    commutative (List.take 1) (List.map ((+) 1)) xs

[<Property>]
let ``Associative Property`` x y z = 
    associative (+) x y z

[<Property>]
let ``DistributesLeftOver Property`` x y z = 
    distributesLeftOver (*) (+) x y z

[<Property>]
let ``DistributesRightOver Property`` x y z =
    distributesRightOver (*) (+) x y z

[<Property>]
let ``DistributesOver Property`` x y z = 
    distributesOver (*) (+) x y z

let append1 xs = 1 :: xs

[<Property>]
let ``Inflating Property`` i = 
    inflating append1 i

[<Property>]
let ``Inflating' Property`` i = 
    inflating' append1 i

[<Property>]
let ``Deflating Property`` (xs : int list) = 
    List.length xs > 1 ==> lazy 
    deflating List.tail xs

[<Property>]
let ``Deflating' Property`` (xs : string list) =
    List.length xs > 1 ==> lazy 
    deflating List.tail xs

[<Property>]
let ``Cycles Property`` x =
    let g x = 1 - x
    cycles g 2 x

[<Property>]
let ``InvariatesOver Property`` (xs : int list) = 
    List.rev |> invariates List.length <| xs

[<Property>]
let ``FixedBy Property`` x y = fixedBy (*) 0 x y

[<Property>]
let ``Endomorphism Property`` i = endomorphism ((*) 7) abs i

[<Property>]
let ``Endomorphism2 Property`` x y =
    endomorphism2 (fun x -> pown x 2) (*) x y

[<Property>]
let ``Endomorphism3 Property`` x y z =
    let (++) x y z = x + y + z
    endomorphism3 negate (++) x y z
