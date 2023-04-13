(* Assignment 8.1, HR 9.8 *)
type BinTree<'a> = (* Page 133 *)
    Leaf
  | Node of BinTree<'a> * 'a * BinTree<'a>
// let rec countA t n = failwith "Not implemented"

// 8.1/9.8 BEGIN -------------------------------------------------------
// Based on Patrick's slides / video for FP 23 6.1 (BinTree accumulator and continuation structure)
// and Patrick's slides / video for FP 23 5.2 (Accumulators, factorial example)
// It seems to me that the example call below "coutnA t 0" has the format "BinTree<'a> -> int -> int",
// whereas the assignment asks for a function of format "int -> BinTree<'a> -> int".
// I have made two versions, cf. below

// Version 1 BEGIN
// let rec countA (accN: int) (t: BinTree<'a>) : int = 
//   match t with
//   | Leaf -> accN
//   | Node (l , n, r) -> countA (countA (accN + 1) l) r


// Should basically do the same as version 2 - only in int -> BinTree<'a> -> int
// Not tail recursive as both calls to countA in last sentence cannot both be tail recursive.

(* Example *)
let t = Node(Node(Leaf,3,Node(Leaf,3,Leaf)),1,Node(Leaf,4,Leaf))

(*
countA 0 t  //<-- This call works for a function int -> BinTree<'a> -> int
*)
// Version 1 END

//version 2 BEGIN----------------
let rec countA  (t: BinTree<'a>) (accN: int): int = 
  match t with
  | Leaf -> accN  //<-- Accumulate until  Leaf
  | Node (l , n, r) -> countA r (countA l (1 + accN)) 
// Above is more or less taken from Patrick's "sumA" example in video for lecture FP 23 6.1, with the 
// main difference that we count the leaves (1+accN) and nut sum them as in the video
// This works well with the example below 
(* Example *)
// let t1 = Node(Node(Leaf,3,Node(Leaf,3,Leaf)),1,Node(Leaf,4,Leaf))
// countA2 t 0 //<-- The original example call
// Version 2 END----------------


(* Assignment 8.2, HR 9.9 *)
// This count function takes a tree, an accumulator and a continuation function as parameter
// When t is matched on a Node, it calls itself on the left sub-tree and add 1 to the accumulator
// When t is matched on a Leaf, the value of the accumulator is carried on into the continuation function, 
//from where the traversal continues
 
let rec countAC t acc cont =
    match t with
    | Leaf -> cont acc
    | Node (left, _, right) -> countAC left (acc+1) (fun leftCount -> countAC right leftCount cont)

(* Example 
countAC t 0 id
*)
// ------------
// Addtional attempts:

// let rec countAC2 (t: BinTree<'a>) a c =
//   match t with
//   | Leaf -> c 0
//   | Node (l, n, r) -> countAC2 l a (fun vl -> countAC2 r a (fun vr -> c (vl + vr  + a + 1)))

// (* Example *)
// countAC2 t 0 id

// let rec countAC3 t c =
//   match t with
//   | Leaf -> c 0
//   | Node (l, n, r) -> countAC3 l (fun vl -> countAC3 r (fun vr -> c (vl + vr + 1)))


// some stuff for testing monday
(*
let t2 = Node(Node(Leaf,3,Node(Leaf,3,Node(Leaf,4,Leaf))),1,Node(Leaf,4,Leaf))
let t3 = Node(Node(Leaf,3,Node(Leaf,3,Node(Leaf,4,Leaf))),1,Node(Leaf,4,Node(Node(Leaf,7,Leaf), 5, Leaf)))
countAC2 t3 id
*)


(* Assignment 8.3, HR 9.10 *)
// ------ ALLAN's VERSION 8.3/9.10 BEGIN -------------
let rec bigListK n k =
  if n=0 then k []
  else bigListK (n-1) (fun res -> 1::k(res))

// This does not work because it is not tail recursive and therefore uses a lot of memory. 
// It has to remeber all the steps everytime it does the 1::k(res) part

// It could look something like this instead:
// Observe that if you put the cons operator inside the parameter k in the anonymous function,
// like below, it will not save the 1 on the stack each time, and instead save the function itself
// and compute it when the inntermost part of the recursion is reached

let rec bigListK3 n k = 
  if n = 0 then k []
  else bigListK3 (n-1) (fun res -> k(1::res))

// alternative version 1
// let bigListK2 n =
//   let rec foo acc n =
//     match n with
//     | 0 -> acc
//     | _ -> foo (1::acc) (n-1)
//   foo [] n


(* Assignment 8.4, HR 9.11 *)
//First we define the tail-recursive function leftTreeC using continuation
let rec leftTreeC n c = 
    match n with
    | n when n<0 ->  c Leaf
    | n -> leftTreeC (n-1) (fun vl -> c(Node(vl, n, Leaf)))

//Then we wrap this iterative function into the function leftTree, having id as the continuation function.
let leftTree n = leftTreeC n id 
(* Examples 
leftTree 0
leftTree 1
leftTree 2
leftTree 3600000 *)

//Same as above but for rightTreeC
let rec rightTreeC n c = 
    match n with
    | n when n<0 ->  c Leaf
    | n -> rightTreeC (n-1) (fun vr -> c(Node(Leaf, n, vr)))

//Same as above but for rightTree
let rightTree n = rightTreeC n id
(* Examples 
rightTree 0
rightTree 1
rightTree 2
rightTree 360000 *)


//-----8.4.1----
//First we test the generic count function as described below
let rec count = function (* from page HR 214 *)
    Leaf -> 0
  | Node(tl,n,tr) -> count tl + count tr + 1

(*
//Testing the count function on functions leftTree and rightTree
count (leftTree 260000) //Peters mac can handle this 
count (leftTree 270000) //This causes Stack overflow
count (rightTree 260000) //Peters mac can handle this
count (rightTree 270000) //This causes Stack overflow

//Then we test the tail-recursive function CountA, that uses accumulation.
countA 0 (leftTree 300000) //Mystisk! Peters mac kunne ikke køre denne function med leftTree.
countA 0 (rightTree 100000000) // Men havde ingen problem med at køre denne absurde function på righttree - omend det tog lidt tid at udregne...
*)

// -------  JONAS' COMMENT START ------- 
  // Det er fori den countA vi bruger har et indre recursive call på venstre side. 
  // når den prøver at køre det med rightTree kører den sit indre call præcis én gang
  // for hver ydre recursive lag, fordi alt til venstre er leafs. Derfor bliver den ikke 
  // helt så "dyb" i dens rekursionsmønster. Derimod vil leftTree k'øre hele vejen til bunden
  // i "indre" recursions, før den så skal køre hele lortet på alle leafs til højre
  // så den får mange flere lag i stacken.
// -------  JONAS' COMMENT END ------- 


//-----8.4.2----
//Now we test the computation time of the count functions using continuation.
//First we test the function countC from the book - this function has two continuation steps. (see function below)
let rec countC t c = (* from page HR 215 *)
  match t with
    Leaf -> c 0
  | Node(tl,n,tr) -> countC tl (fun vl -> countC tr (fun vr -> c(vl+vr+1)))

(*

// results
countC (leftTree 5000000) id // Realtime: 5 sec
countC (rightTree 5000000) id // Realtime: 6 sec
countC (leftTree 10000000) id // Realtime: 6 sec
countC (rightTree 10000000) id // Realtime: 9 sec
countC (leftTree 20000000) id // Realtime: 16 sec
countC (rightTree 20000000) id // Realtime: 18 sec
countC (leftTree 40000000) id // Realtime: 21 sec
countC (rightTree 40000000) id // Realtime: 21 sec

// Then we do the same tests, but now for the count function countAC we defined above, that uses both accumulation and continuation.
// We observe that this function is slightly faster than countC.
countAC (leftTree 5000000) 0 id // Realtime: 2,3 sec
countAC (rightTree 5000000) 0 id // Realtime: 1,2 sec
countAC (leftTree 10000000) 0 id // Realtime: 3,6 sec
countAC (rightTree 10000000) 0 id // Realtime: 2,9 sec
countAC (leftTree 20000000) 0 id // Realtime: 7 sec
countAC (rightTree 20000000) 0 id // Realtime: 6 sec
countAC (leftTree 40000000) 0 id // Realtime: 17,6 sec
countAC (rightTree 40000000) 0 id // Realtime: 14,6 sec
*)


(* Assignment 8.5, HR 11.1 *)
//Rather simple sequence build using Seq.initInfinite and the function for odd numbers (fun i - 2*i+1)
let oddNumbers = Seq.initInfinite (fun i -> 2*i+1)


(* Assignment 8.6, HR 11.2 *)
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
(*Examples 
Seq.take 0 fac
Seq.take 1 fac
Seq.take 2 fac
Seq.take 10 fac
*)