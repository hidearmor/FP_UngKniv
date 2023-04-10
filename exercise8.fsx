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

// ----------- PETERS COMMENT: Jeg har byttet rundt på version 1 og 2, så version 1 nu matcher den type opgaven efterspørger.

// Version 1 BEGIN
let rec countA (accN: int) (t: BinTree<'a>) : int = 
  match t with
  | Leaf -> accN
  | Node (l , n, r) -> countA (countA (accN + 1) l) r
// Should basically do the same as version 1 - only in int -> BinTree<'a> -> int
// I basically just played around with the version 1 components to make it fit with int -> BinTree<'a> -> int
// -----------PETER COMMENT: I think we should go with this solution. -------------

(* Example *)
let t = Node(Node(Leaf,3,Node(Leaf,3,Leaf)),1,Node(Leaf,4,Leaf))
countA 0 t  //<-- This call works for a function int -> BinTree<'a> -> int
// Version 1 END

//version 2 BEGIN
let rec countA2  (t: BinTree<'a>) (accN: int): int = 
  match t with
  | Leaf -> accN  //<-- Accumulate until  Leaf
  | Node (l , n, r) -> countA2 r (countA2 l (1 + accN)) 
// Above is more or less taken from Patrick's "sumA" example in video for lecture FP 23 6.1, with the 
// main difference that we count the leaves (1+accN) and nut sum them as in the video
// This works well with the example below 
(* Example *)
let t1 = Node(Node(Leaf,3,Node(Leaf,3,Leaf)),1,Node(Leaf,4,Leaf))
countA2 t 0 //<-- The original example call
// Version 2 END
// Allan's attempt at 8.1/9.8 END -------------------------------------------------------


// -------PETER COMMENT: This section we can skip, since the above IS NOT tail recursive---------------
// another version
// we accumulate in a different way and WITHOUT tail recursion here.
// but we accumulate on the stack, not in the accumulator parameter
let rec countANoTail (accN: int) (t: BinTree<'a>) : int = 
  match t with
  | Leaf -> accN
  | Node (l, n, r) -> 1 + ((countANoTail accN l) + (countANoTail accN r))







(* Assignment 8.2, HR 9.9 *)
// Allan's attempt at 8.2/9.9 BEGIN -----------------------------------------------------
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

// ---------- PETERS COMMENT:-----------
// Godt arbjede drenge! Jonas har en pointe i at akkumulatoren i CountAC ingen funktion har. 
// Fjerner vi den (som i CountAC2) får vi en funktion identisk med CountC i bogen side 214-215
// Opgaven går altså ud på at omskrive CountC (CountAC2), så man erstatter en continuation med en akkumulator. 
// Hvordan vi løser det, ved jeg ikke endnnu :) 


(* Example *)
countAC t 0 id

// ----- JONAS' VERSIONS 8.2/9.9 --- END ------------------------------------


//------------------------- PETERS VERSION 8.2/9.9 ---- BEGIN  ----------------
// This count function takes a tree, an accumulator and a continuation function as parameter
// When t is matched on a Node, it calls itself on the left sub-tree and add 1 to the accumulator
// When t is matched on a Leaf, the value of the accumulator is carried on into the continuation function, 
//from where the traversal continues
 
let rec countAC3 t acc cont =
    match t with
    | Leaf -> cont acc
    | Node (left, _, right) -> countAC3 left (acc+1) (fun leftCount -> countAC3 right leftCount cont)


(* Example *)
countAC3 t 0 id

//-------------------- PETERS VERSION 8.2/9.9----- END--------------------





(* Assignment 8.3, HR 9.10 *)
// ------ ALLAN's VERSION 8.3/9.10 BEGIN -------------
let rec bigListK n k =
  if n=0 then k []
  else bigListK (n-1) (fun res -> 1::k(res))

// This does not work because it is not tail recursive and therefore uses a lot of memory.

// -------  JONAS' COMMENT START ------- 
// It has to remeber all the steps everytime it does the 1::k(res) part
// -------  JONAS' COMMENT END ------- 

// It could look something like this instead:

let bigListK2 n =
  let rec foo acc n =
    match n with
    | 0 -> acc
    | _ -> foo (1::acc) (n-1)
  foo [] n

// I sort of feel like above bigListK2 should work as tail recursive, but cannot find my way through why it does not??

// -------  JONAS' COMMENT START ------- 
// Jonas comment: it's not tail recursive since it's not an anonymous function, but an actual list, i.e. variable. 
// When we say "1::acc" in the version below, however, there's an anonymous function, that says: 
// "when I'm evaluated, we'll do this" and then we pass the function on, not the list. 
// Functions are stored in the heap whereas a value like a list is stored in the stack
// -------  JONAS' COMMENT END ------- 

// ------- PETERS EXTENSION BEGIN --------------

//The bigListK2 could also look like this, which would make it tail recursive (... and identical to bigListC from p. 213)

let rec bigListK3 n k = 
  if n = 0 then k []
  else bigListK3 (n-1) (fun res -> k(1::res))

// ------- PETERS EXTENSION END --------

// ------ ALLAN's VERSION 8.3/9.10 END -------------







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

//Testing the count function on functions leftTree and rightTree
count (leftTree 260000) //Peters mac can handle this 
count (leftTree 270000) //This causes Stack overflow
count (rightTree 260000) //Peters mac can handle this
count (rightTree 270000) //This causes Stack overflow

//Then we test the tail-recursive function CountA, that uses accumulation.
countA 0 (leftTree 300000) //Mystisk! Peters mac kunne ikke køre denne function med leftTree.
countA 0 (rightTree 100000000) // Men havde ingen problem med at køre denne absurde function på righttree - omend det tog lidt tid at udregne...

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
countAC3 (leftTree 5000000) 0 id // Realtime: 2,3 sec
countAC3 (rightTree 5000000) 0 id // Realtime: 1,2 sec
countAC3 (leftTree 10000000) 0 id // Realtime: 3,6 sec
countAC3 (rightTree 10000000) 0 id // Realtime: 2,9 sec
countAC3 (leftTree 20000000) 0 id // Realtime: 7 sec
countAC3 (rightTree 20000000) 0 id // Realtime: 6 sec
countAC3 (leftTree 40000000) 0 id // Realtime: 17,6 sec
countAC3 (rightTree 40000000) 0 id // Realtime: 14,6 sec







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