module a2


// TEST COMMIT 


// Exercise 3.1 downTo + downTo2
(*Using if then else, we chose to use the simple list expressions for the 
downTo function. We could have also defined this recursively by eg. 
let downTo n = 
    if n < 1
    then failwith "n must larger than 0"
    else n::downTo(n-1)
*)

let downTo n = 
    if n < 1                                    //If n is smaller than one then (ie you try to produce a list of 0 or less elements)
    then failwith "n must larger than 0"        //provide a fail message
    else [n .. -1 .. 1]                         //otherwise use the simple llist expression to build the list, from n to 0 with steps of -1

(*for the downTo2 function we chose to use recursion. Similar to the downTo funciton this could have 
been defined with a simple list expression.
The recursive function calls itself with n-1 as argument until n is 0*)
let rec downTo2 n = 
    match n with                                
    | 0 -> []                                   //When n is 0 then return empty list
    | n -> n :: downTo2(n-1)                    //recursively call downTo2 (the function calls itself) with n-1


// Exercise 3.2 removeOddIdx
(*match a list of at least two elements x0 and x1. Keep the first element, x0, and discard the second, x1. 
Recursively call removeOddIdx with the remaining xs
*)
let rec removeOddIdx (xs: int list) = 
    match xs with
    |x0::x1::xs -> x0::removeOddIdx(xs)         //As described just above
    |[x0] -> [x0]                               //if/when xs has one element then return the element
    |[] -> []                                   //If/when xs is an empty list, then return an empty list        

// Exercise 3.3 combinePair
(*match a list of at least two elements x0 and x1 and combine them into a pair.
Recursively call combinePair with the remaining xs until there are 1 or less elements in xs.*)
let rec combinePair (xs :int list) = 
    match xs with
    |x0::x1::xs -> (x0, x1)::combinePair(xs)    //As described just above
    |[x0] -> []                                 //if/when xs has one element then return an empty list
    |[] -> []                                   //if/when xs is empty the return an empty list



// Exercise 3.4 - HR 3.2 - British currency

// Money tuple addition
(*Takes two  two triples (a, b, d) and (d, e, f) and multiplies them to 
get them amount of pounds, shilling and pence*)
let (^+^) (a, b, c) (d, e, f) = 
    let pence = (c + f) % 12                                //Calculates the number of pence. Cannot go beyond 11 as 12 pence = 1 shilling
    let shilling = (b + e + (int (c + f) /12)) % 20         //Calculates the number of shilling (incl. the pence contributions). Cannot go beyond 19 as 20 shilling = 1 pound
    let pound =  a + d + ((b + e + int (c + f) /12) / 20)   //Calculates the number of pounds (incl. the shilling contributions).
    (pound, shilling, pence)                                //Returns a triple with the multiplications

// Money tuple subtraction
(*Takes two triples, converts them into pence and subtract the two amounts in pence from each other
Finally calcalutes the number of pence into shillings and pounds.*)
let (^-^) (a, b, c) (d, e, f) = 
    let pence1 = a*20*12 + b*12 + c                         //number of pence in triple 1
    let pence2 = d*20*12 + e*12 + f                         //number of pence in triple 2
    let penceDiff = pence1-pence2                           //the difference in pence
    let pound = int (penceDiff /(20*12))                    //how many pounds does it add up to
    let shilling = int ((penceDiff - pound*20*12)/12)       // how many shilling does it add up to
    let pence = int (penceDiff - pound*20*12 - shilling * 12)  //how many pence
    (pound, shilling, pence)                                //returns a triple with the subtractions

// Money record addition
(*Here we basically want to be able to do the same as for the triples above, however by looking at 
pound, shilling, pence as a record of type Money*)
type Money = {pound : int; shilling : int; pence : int};;   //Define the type Money

let moneyA = {pound = 10; shilling = 4; pence = 7};;        //define a money amount (moneyA)
let moneyB = {pound = 4; shilling = 13; pence = 5};;        //define a money amount (moneyB)
                    
let (|+|) moneyA moneyB =  //use the basic same structure for calculating as for (^+^) only with moneyA and moneyB
    let pe = (moneyA.pence + moneyB.pence) % 12
    let sh = ((moneyA.shilling + moneyB.shilling) + (int ((moneyA.pence + moneyB.pence)) /12)) % 20
    let po =  moneyA.pound + moneyB.pound + ((moneyA.shilling + moneyB.shilling + int ((moneyA.pence + moneyB.pence)) /12) / 20)
    {pound = po; shilling = sh; pence = pe}                 //return a record of moneyA + moneyB

// Money record subtraction
type Money = {pound : int; shilling : int; pence : int};;

let moneyA = {pound = 10; shilling = 4; pence = 7};;
let moneyB = {pound = 4; shilling = 13; pence = 5};; 

let (|-|) moneyA moneyB =  //use the basic same structure for calculating as for (^-^) only with moneyA and moneyB
    let penceA = moneyA.pound*20*12 + moneyA.shilling*12 + moneyA.pence
    let penceB = moneyB.pound*20*12 + moneyB.shilling*12 + moneyB.pence
    let penceDiff = penceA - penceB
    let po = int (penceDiff /(20*12))
    let sh = int ((penceDiff - po*20*12)/12)
    let pe = int (penceDiff - po*20*12 - sh * 12)
    {pound = po; shilling = sh; pence = pe}               //return a record of moneA - moneyB

// Exercise 3.5 - HR 3.3 - Complex numbers

//      1. Declare infix for addition and multiplication
let ( .+) (a:float,b:float) (c:float,d:float) = (a + c, b + d) //calculate using the defintions in the assignment

let ( .*) (a:float,b:float) (c:float,d:float) = (a*c-b*d, b*c + a*d) //calculate using the defintions in the assignment

//      2. Declare infix for subtraction and division
let ( .-) (a:float,b:float) (c:float,d:float) = (a - c, b - d) //calculate using the defintions in the assignment

let ( ./) (a,b) (c,d) =  (a*(c/(c*c + d*d) - b*(-d/(c*c + d*d)) , b*(c/(c*c + d*d))+a*(-d/(c*c + d*d)))) //calculate using the defintions in the assignment

//      3. Use 'let' expressions in division to avoid repeated evals
(*The basic same as just above, only be simplified by using functions instead*)
let ( ./) (a,b) (c,d) = 
    let inv_c = c/(c*c + d*d)
    let inv_d = -d/(c*c + d*d)
    (a*inv_c-b*inv_d, b*inv_c+a*inv_d)


// Exercise 3.6 - HR 4.4 - altSum -> HR page 76
// function alternating between adding and subtracting the contents of a list.

// //Denne version gider ikke håndtere [_] (single-element list)
// let rec altsum (xs: int list) =
//     match xs with
//     |[] -> 0
//     |x0::(x1::xs as xs') -> if xs' = [] then x0 else (x0 - x1) + altsum xs

// //Denne version gider ikke håndtere [] (empty list)
// let rec altsum = function
//     |[x] -> x
//     |x0::x1::xs -> x0 - x1 + altsum xs


    //Allan: jeg har et skud her - jeg synes, den virker. Check it out 
    let rec altsum = function
    | [] | [_] as xs -> if xs = [] then 0 else List.head xs     //<-- matches to 0 or 1 element, binds the matched list to xs so that if xs is empty then return 0
    | x0::x1::xs -> x0 - x1 + altsum xs                         //<-- If the list has at least two elements, then subtract x1 from x0 and add the result to the recursive call

    // //Allan: Den originale 3 clauses fra bogen
    // let rec altsum = function
    // | [] -> 0
    // | [x] -> x
    // | x0::x1::xs -> x0-x1 + altsum xs
