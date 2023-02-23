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

let rec sum(m,n) = failwith "Not implemented"

(* Example *)
sum(10,10)

(* Assignment 7.3, HR 9.4 *)
let length xs = failwith "Not implemented"
(* Example *)
length [1]

(* Assignment 7.4, HR 9.6 *)
let rec facC n c = failwith "Not implemented"
(* Example *)
facC 5 id
  
(* Assignment 7.5, HR 8.6 *)
let rec fib n = failwith "Not implemented"
(* Example *)
fib 4

(* Assignment 7.6, HR 9.7 *)
let rec fibA n n1 n2 = failwith "Not implemented"
(* Example *)
fibA 10 0 1

let rec fibC n c = failwith "Not implemented"
(* Example *)
fibC 10 id
