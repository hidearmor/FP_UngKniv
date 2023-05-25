module a8

(* Assignment 7.1, HR 9.1 *)
(* Not covered by Code Judge *)
let xs = [1;2]

let rec g = function
    0 -> xs
  | n -> let ys = n::g(n-1)
         List.rev ys

g 2

(* Draw the stack  *)

(* Assignment 7.2, HR 9.3 *)


let sum(m,n) =
  let rec tail n acc = 
    match n with
    | 0 -> acc + m
    | n -> tail (n-1) (acc+m+n)
  tail n 0

(* Example *)
sum(10,10)

(* Assignment 7.3, HR 9.4 *)
let length xs = 
  let rec tail xs acc = 
    match xs with
    | [] -> acc
    | x::xs -> tail (xs) (acc+1)
  tail xs 0

(* Example *)
length [1]

(* Assignment 7.4, HR 9.6 *)
// let rec facC n c = failwith "Not implemented"
// Allan's attempt at 7.4/9.6 --------------------------------------
// Factorial function from section 9.4 in the book:
let rec factA = function
  | (0, m) -> m
  | (n, m) -> factA(n-1, n*m)

// recursive from section 9.4 as a continuation-based:
// A continuation based version will take a has an extra argument (the continuation function, "c", which we want to "feed" the results 
// from the recursive call) (cf. textbook section 9.6 + slides W07 "Imperative features and efficiency")
let rec facC n c =
  if n = 1 then c 1                              //<-- If n=1 then c is called with value = 1 (since 1! = 1)
  else facC (n-1) (fun res -> c(n * res))  //<-- Else recursively call facC with n-1 and the continuation function "fun res -> c(n * res)"
                                                //      the calls of c are then tail calls 

// Jonas' addition
// version with the identity function "hidden" so the interface is simpler
// because it feel redundant to me to have know, that adding an 
// identity function is nessecary to use the funtion
let factHiddenC n =
  let c = id
  let rec f n' g =
    if n' = 1 then g 1
    else f (n'-1) (fun x -> g(n' * x))
  f n c


// Running factA(5000000,1);; gives the results
// Real: 00:00:00.005, CPU: 00:00:00.000
// Running facC 5000000 id;; gives the result
// Real: 00:00:00.428, CPU: 00:00:00.406
                                        
(* Example *)
facC 5 id
  
(* Assignment 7.5, HR 8.6 *)
//let rec fib n = failwith "Not implemented"
//Allan's attempt -------------------------------------
//The Fibonacci numbers from 1.6
let rec fib16 = function
    | 0 -> 0
    | 1 -> 1
    | n -> fib16(n-1) + fib16(n-2)

//Solution to assignment 7.5 (not sure if this is what is asked for, but it's a Wdile-loop that does the job :-)):
let fib n = 
    let mutable a = 0 //First no in Fibunacci sequence
    let mutable b = 1 //Second no in Fibonacci sequence
    let mutable x = 0 //Counter
    while x < n do
      let c = a+b     //"Helper variable" calculates the fibunacci
      a <- b                //value of b is parsed into a (ie. moving a step forward in the fib seq)
      b <- c                //value of c is parsed into b (ie. moving a step forward in the fib seq)
      x <- x+1              
    a                       //return a as the value of the fib seq.
//-----------------------------------------------------

(* Example *)
fib 4

(* Assignment 7.6, HR 9.7 *)
//let rec fibA n n1 n2 = failwith "Not implemented"
// Allan's version-----------------------------------
// Jonas added behaviour for 1 since it is a fibbonacci number but did not compute properly
let rec fibA n n1 n2  = 
  match n with
  | 1 | 2 -> n1 + n2                  //<-- Stop at 2, cf below
  | _     -> fibA (n-1) n2 (n1+n2)    
  //^^^ Recursively counts down n and sets n1 to  previous in row (ie. n2) and n2 to n1 + n2.

// Why stop at 2?
// It corresponds to n2 (n2 = F(n-2)) being 1 as per:
// Starting value n2 = 1 -> F(n-2) = F(8) = 1 counting down:
// F(8) = 1 (n1 + n2)
// F(7) = 1
// F(6) = 3
// F(5) = 5
// F(4) = 8
// F(3) = 13
// F(2) = 21
// F(1) = 34
// ---> n1 = F(2) = 21 + n2 = F1(1) = 34 -> n1 + n2 = 55
//-------------------------------------

(* Example *)
fibA 10 0 1

// let rec fibC n c = failwith "Not implemented"
// Allan's version 1 --------------------------------
// cf. textbook example section 9.6 on BigList.
let rec fibC n c = 
  if n <= 2 then c 1
  else fibC (n-1) (fun n1 -> fibC (n-2) (fun n2 -> c (n1 + n2))) //<-- Cf. Patrick's lecture (FP 23 6.1) for ref as well

// version 2 (Jonas added 1 again)
let rec fibC2 n c =
  match n with
  | 2 | 1 -> c 1
  | _ -> fibC2 (n-1) (fun n1 -> fibC (n-2) (fun n2 -> c (n1 + n2)))
// Explanation to the last line:
// the element "...(fun n2 -> c (n1 + n2))" basically just uses the continuation function c to make n2 = n1 + n2 (same as assignment 9.7 above)
// the element "...(fun n1 -> fibC (n-2)..." sets n1 to be equal to whatever comes out of the function "(fun n2...)" (same as assignment 9.7 above)
// the element "fibC (n-1)..." is the counter 
//---------------------------------------------------------

(* Example *)
fibC 10 id
