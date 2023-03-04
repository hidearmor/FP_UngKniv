module test
type 'a BinTree =
    Leaf
  | Node of 'a * 'a BinTree * 'a BinTree
// 5.1
// let inOrder...
// The tree from the assignment
type Tree = Leaf
          | Node of int*Tree*Tree

//The  function from the slides
let intBinTree =
    Node(43, Node(25, Node(56,Leaf, Leaf), Leaf),
        Node(562, Leaf, Node(78, Leaf, Leaf)))

//inOrder function. 
let rec inOrder tree = 
    match tree with 
      Leaf -> []                            //<-- The input is a leaf (ie and empty tree), no values to return
    | Node(n, treeL, treeR) -> inOrder treeL @ [n] @ inOrder treeR; //<--First traverse the left tree, append (@) n to the list that comes 
inOrder intBinTree                                                  //out of this (effectively a new list). Then traverse the right tree 
                                                                    //Concatenate the lists uing @
// 5.2
// let mapInOrder...

//Allan's suggestions below -------------.
let rec mapInOrder f tree =         //<-- take a function f
    match tree with                 
    | Leaf -> Leaf                  //<-- if you are a leaf, then remain a leaf
    | Node(n, treeL, treeR) -> Node(f n, mapInOrder f treeL, mapInOrder f treeR) //<-- Apply the function to all the nodes.
//-----------------

// mapInOrder intBinTree



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

