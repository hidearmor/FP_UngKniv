module a2
// Exercise 3.1 downTo + downTo2
let rec downTo n = failwith "not implemented"
let rec downTo2 = failwith "not implemented"
// Exercise 3.2 removeOddIdx
let rec removeOddIdx (xs: int list) = failwith "not implemented"

// Exercise 3.3 combinePair
let rec combinePair (xs :int list) = failwith "not implemented"

// Exercise 3.4 - HR 3.2 - British currency

// Money tuple addition
let (^+^) a b = failwith "not implemented"
// Money tuple subtraction
let (^-^) a b = failwith "not implemented"

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