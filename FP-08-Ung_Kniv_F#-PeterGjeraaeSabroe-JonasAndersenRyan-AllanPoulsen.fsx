(* Assignment 8.1, HR 9.8 *)
type BinTree<'a> = (* Page 133 *)
    Leaf
  | Node of BinTree<'a> * 'a * BinTree<'a>

let rec countA t n = failwith "Not implemented"

(* Example *)
let t = Node(Node(Leaf,3,Node(Leaf,3,Leaf)),1,Node(Leaf,4,Leaf))
countA t 0

(* Assignment 8.2, HR 9.9 *)
let rec countAC t n c = failwith "Not implemented"

(* Example *)
countAC t 0 id

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
let oddNumbers = failwith "Not implemented"

(* Assignment 8.6, HR 11.2 *)
let fac = failwith "Not implemented"
(*Examples *)
Seq.take 0 fac
Seq.take 1 fac
Seq.take 2 fac
Seq.take 10 fac
