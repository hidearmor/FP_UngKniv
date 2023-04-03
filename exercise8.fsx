(* Assignment 8.1, HR 9.8 *)
type BinTree<'a> = (* Page 133 *)
    Leaf
  | Node of BinTree<'a> * 'a * BinTree<'a>
// let rec countA t n = failwith "Not implemented"

// Allan's attempt at 8.1/9.8 BEGIN -------------------------------------------------------
// Based on Patrick's slides / video for FP 23 6.1 (BinTree accumulator and continuation structure)
// and Patrick's slides / video for FP 23 5.2 (Accumulators, factorial example)
// It seems to me that the example call below "coutnA t 0" has the format "BinTree<'a> -> int -> int",
// whereas the assignment asks for a function of format "int -> BinTree<'a> -> int".
// I have made two versions, cf. below

//version 1 BEGIN
let rec countA  (t: BinTree<'a>) (accN: int): int = 
  match t with
  | Leaf -> accN  //<-- Accumulate until  Leaf
  | Node (l , n, r) -> countA r (countA l (1 + accN)) 
// Above is more or less taken from Patrick's "sumA" example in video for lecture FP 23 6.1, with the 
// main difference that we count the leaves (1+accN) and nut sum them as in the video
// This works well with the example below 
(* Example *)
let t = Node(Node(Leaf,3,Node(Leaf,3,Leaf)),1,Node(Leaf,4,Leaf))
countA t 0 //<-- The original example call
// Version 1 END

// Version 2 BEGIN
let rec countA2 (accN: int) (t: BinTree<'a>) : int = 
  match t with
  | Leaf -> accN
  | Node (l , n, r) -> countA2 (countA2 (accN + 1) l) r
// Should basically do the same as version 1 - only in int -> BinTree<'a> -> int
// I basically just played around with the version 1 components to make it fit with int -> BinTree<'a> -> int

(* Example *)
let t = Node(Node(Leaf,3,Node(Leaf,3,Leaf)),1,Node(Leaf,4,Leaf))
countA2 0 t  //<-- This call works for a function int -> BinTree<'a> -> int
// Version 2 END
// Allan's attempt at 8.1/9.8 END -------------------------------------------------------

// another version
// we accumulate in a different way and WITHOUT tail recursion here.
// but we accumulate on the stack, not in the accumulator parameter
let rec countANoTail (accN: int) (t: BinTree<'a>) : int = 
  match t with
  | Leaf -> accN
  | Node (l, n, r) -> 1 + ((countANoTail accN l) + (countANoTail accN r))

// Allan's attempt at 8.2/9.9 BEGIN -----------------------------------------------------
(* Assignment 8.2, HR 9.9 *)
// Jonas corrected the last line to have the counter a instead of the node value n
let rec countAC t a c =
  match t with
  | Leaf -> c 0
  | Node (l, n, r) -> countAC l a (fun vl -> countAC r a (fun vr -> c (vl + vr  + a + 1)))
// Structure inspired by Patrick's video for FP 23 6.1. Traverse left, then traverse right, then sum and move through the nodes!

// Allan's attempt at 8.2/9.9 END -------------------------------------------------------

// ----- JONAS' VERSIONS 8.2/9.9 --- START ------------------------------------

// WORKS THE SAME COMPLETELY WITHOUT a ??
let rec countAC2 t c =
  match t with
  | Leaf -> c 0
  | Node (l, n, r) -> countAC2 l (fun vl -> countAC2 r (fun vr -> c (vl + vr + 1)))


// some stuff for testing monday
(*
let t2 = Node(Node(Leaf,3,Node(Leaf,3,Node(Leaf,4,Leaf))),1,Node(Leaf,4,Leaf))
let t3 = Node(Node(Leaf,3,Node(Leaf,3,Node(Leaf,4,Leaf))),1,Node(Leaf,4,Node(Node(Leaf,7,Leaf), 5, Leaf)))
countAC2 t3 id
*)


(* Example *)
countAC t 0 id

// ----- JONAS' VERSIONS 8.2/9.9 --- END ------------------------------------


(* Assignment 8.3, HR 9.10 *)
let rec bigListK n k =
  if n=0 then k []
  else bigListK (n-1) (fun res -> 1::k(res))

(* Assignment 8.4, HR 9.11 *)
let rec leftTreeC n c = failwith "Not implemented"
let leftTree n = failwith "leftTreeC ..."
(* Examples *)
leftTree 0
leftTree 1
leftTree 360000

let rec rightTreeC n c = failwith "Not implemented"
let rightTree n = failwith "rightTreeC ..."
(* Examples *)
rightTree 0
rightTree 1
rightTree 2
rightTree 360000

let rec count = function (* from page HR 214 *)
    Leaf -> 0
  | Node(tl,n,tr) -> count tl + count tr + 1

let rec countC t c = (* from page HR 215 *)
  match t with
    Leaf -> c 0
  | Node(tl,n,tr) -> countC tl (fun vl -> countC tr (fun vr -> c(vl+vr+1)))


(* Assignment 8.5, HR 11.1 *)
//Peters version
//Rather simple sequence build using Seq.initInfinite and the function for odd numbers (fun i - 2*i+1)
let oddNumbers = Seq.initInfinite (fun i -> 2*i+1)


(* Assignment 8.6, HR 11.2 *)
//Peters version
//Again quite simple - måske lidt for blåøjet. Jeg bruger en enkelt (ikke særlig smart - burde laves som continuation) fact
//funktion som en hjælper funktion til at genere sequences af alle factorials. 
let rec factA = function
  | (0, m) -> m
  | (n, m) -> factA(n-1, n*m)

(*
// Does NOT work with this fact function
let rec facC n c =
  if n = 1 then c 1
  else facC (n-1) (fun res -> c(n * res))
*)


let fac = Seq.initInfinite (fun i -> factA (i, 1))
(*Examples *)
Seq.take 0 fac
Seq.take 1 fac
Seq.take 2 fac
Seq.take 10 fac