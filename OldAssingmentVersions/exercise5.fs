module test
type 'a BinTree =
    Leaf
  | Node of 'a * 'a BinTree * 'a BinTree
// 5.1
// let inOrder...

// 5.2
// let mapInOrder...

// 5.3
// let foldInOrder...

//5.4 + 5.5
type aExp =                     (* Arithmetical expressions *)
    | N of int                  (* numbers *)
    | V of string               (* variables *)
    | Add of aExp * aExp        (* addition *)
    | Mul of aExp * aExp        (* multiplication *)
    | Sub of aExp * aExp        (* subtraction *)

type bExp =                     (* Boolean expressions *)
    | TT                        (* true *)
    | FF                        (* false *)
    | Eq of aExp * aExp         (* equality *)
    | Lt of aExp * aExp         (* less than *)
    | Neg of bExp               (* negation *)
    | Con of bExp * bExp        (* conjunction *)
(*
type stm =                      // statements
    | Ass of string * aExp      // assignment
    | Skip
    | Seq of stm * stm          // sequential composition
    | ITE of bExp * stm * stm   // if-then-else
    | While of bExp * stm       // while
    | IT of ...                 // if-then
    | RU of ...                 //  repeat until
*)
let rec A a s =
    match a with
    | N n -> n
    | V x -> Map.find x s
    | Add(a1, a2) -> A a1 s + A a2 s
    | Mul(a1, a2) -> A a1 s * A a2 s
    | Sub(a1, a2) -> A a1 s - A a2 s;;

(*
let rec B b s =
match b with
    | TT -> true
    | ....

let rec I stm s =
    match stm with
    | Ass(x,a) -> update x ( ... ) s
    | Skip -> ...
    | Seq(stm1, stm2) -> ...
    | ITE(b,stm1,stm2) -> ...
    | While(b, stm) -> ... ;;
    | IT...
    | RU...

// Example 0
let stmt0 = 
// Example 1
let stmt1 = ...
let state1 = ...
// Example 2
let stmt2 = ...
let state2= ...
// Example 3
let stmt3 = ...
let state3 = ...
// Example 4
let stmt4 = ...
let state4 = ...
// Example 5
let stmt5 = ...
let state5 = ...
*)

