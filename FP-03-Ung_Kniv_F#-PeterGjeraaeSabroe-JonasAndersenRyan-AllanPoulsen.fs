module a2


// TEST COMMIT 


// Exercise 3.1 downTo + downTo2
let downTo n = 
    if n < 1
    then failwith "n must larger than 0"
    else [n .. -1 .. 1]


let rec downTo2 n = 
    match n with
    | 0 -> []
    | n -> n :: downTo2(n-1)


// Exercise 3.2 removeOddIdx
let rec removeOddIdx (xs: int list) = 
    match xs with
    |x0::x1::xs -> x0::removeOddIdx(xs)
    |[x0] -> [x0]
    |[] -> []

// Exercise 3.3 combinePair
let rec combinePair (xs :int list) = 
    match xs with
    |x0::x1::xs -> (x0, x1)::combinePair(xs)
    |[x0] -> []
    |[] -> []



// Exercise 3.4 - HR 3.2 - British currency

// Money tuple addition
let (^+^) (a, b, c) (d, e, f) = 
    let pence = (c + f) % 12
    let shilling = (b + e + (int (c + f) /12)) % 20
    let pound =  a + d + ((b + e + int (c + f) /12) / 20)
    (pound, shilling, pence)

// Money tuple subtraction
let (^-^) (a, b, c) (d, e, f) = 
    let pence1 = a*20*12 + b*12 + c
    let pence2 = d*20*12 + e*12 + f
    let penceDiff = pence1-pence2
    let pound = int (penceDiff /(20*12))
    let shilling = int ((penceDiff - pound*20*12)/12)
    let pence = int (penceDiff - pound*20*12 - shilling * 12)
    (pound, shilling, pence)

type Money = {pound : int; shilling : int; pence : int};;
// Money record addition
let (|+|) a b = failwith "not implemented"
// Money record subtraction
let (|-|) a b = failwith "not implemented"

// Exercise 3.5 - HR 3.3 - Complex numbers

//      1. Declare infix for addition and multiplication
let ( .+) (a:float,b:float) (c:float,d:float) = failwith "not implemented"

let ( .*) (a:float,b:float) (c:float,d:float) = failwith "not implemented"

//      2. Declare infix for subtraction and division
let ( .-) (a:float,b:float) (c:float,d:float) = failwith "not implemented"

let ( ./) (a,b) (c,d) = failwith "not implemented"
//      3. Use 'let' expressions in division to avoid repeated evals
let ( ../) (a:float,b:float) (c:float,d:float) = failwith "not implemented"


// Exercise 3.6 - HR 4.4 - altSum -> HR page 76
// function alternating between adding and subtracting the contents of a list.
let rec altsum = failwith "not implemented"