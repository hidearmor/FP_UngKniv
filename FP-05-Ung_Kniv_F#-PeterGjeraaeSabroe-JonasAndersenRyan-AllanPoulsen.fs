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
      Leaf -> []                            //<-- The input is a leaf (ie an empty tree), no values to return
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
//REMEMBER TO PUT IN EXPLANATION OF DIFF BTW MAPINORDER AND MAPPOSTORDER!!


// 5.3
// let foldInOrder...
// Allan's suggestion based on exercise 4.2 and 4.3 ---------------
// 1. the "inOrder tree" element of the sentence produces a list, using the inORder function above
// 2. the "|>" pipes the list into the function
// 3. List.fold is a library function (we used it in exercise 4.2)
// 4. "(fun x xs -> f xs x) x" does the following (based on exercise 4.2): a) takes the arguments
//      x and xs, where x is the current element being piped into the function
//      and xs is the accumulated value is the list is being piped into
//      the function

let foldInOrder f xs tree = 
    inOrder tree |> List.fold (fun xs x -> f x xs) xs

//For some reason I cannot make it work with the floatTree, but it works with binTree (below)
//so likely I just made some mistake in the floatTree definition
foldInOrder (fun n a -> a + n) 0 intBinTree
//-------------



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

