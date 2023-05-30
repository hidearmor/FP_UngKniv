type Rope =
    | Leaf of string * int
    | Node of Rope * int * Rope

let rope1 = Node(Node(Leaf("I_lik",5),5,Leaf("e_functional_pr",15)),
                 20,
                 Node(Leaf("ogr",3),3,Leaf("amming",6)))

// Question 1.1

let rope2 = Node(Leaf("_and",4),
                 4,
                 Node(Leaf("_very_",6),6,Leaf("much_F#",7)))

// The type Rope is monomorphic since all its arguments have fixed types.

let rope3 = Node(Node(Leaf("example_",8), 8 ,Leaf("with_",5)),
                 13,
                 Leaf("5_nodes",7))

// Question 1.2

let rec length r = 
    match r with
    | Leaf (_, i) -> i
    | Node (_, i, rr) -> i + length rr

length rope1;; // answer is 29

let rec flatten r =
    match r with
    | Leaf (str, _) -> str
    | Node(rl,_,rr) -> (flatten rl) + (flatten rr);;

flatten rope1 // "I_like_functional_programming"
flatten rope2 // "_and_very_much_F#"
flatten rope3 // "example_with_5_nodes"

let rec maxDepth r = 
    match r with
    | Leaf (_,_) -> 1 
    | Node (rl, _, rr) -> 
            let x = maxDepth rl
            let y = maxDepth rr
            if x > y then 1 + x else 1 + y

maxDepth rope3 // 3

let rec index i r = 
    match r with
    | Leaf (str, j) -> if i > j then failwith "index out of bounds" else str.[i]
    | Node (rl, j, rr) -> if j > i then index i rl else index (i-j) rr

let test1 = index 5 rope1 // 'e'
let test2 = index 33 rope1 // "index out of bounds"
let test3 = index 29 rope1 //

// Question 1.3

let concat r1 r2 = Node(r1, length r1, r2)

let rec prettyPrint r = 
    match r with 
    | Leaf (str, i) -> sprintf "Leaf(%s,%d)" str i
    | Node (rl, i, rr) -> 
        sprintf "Node(
        %O 
        %d 
        %O" (prettyPrint rl) i (prettyPrint rr)

let prettyPrint1 (r: Rope) = string(r)

prettyPrint1 rope1


// Question 2.1

type Bucket<'a> = {
  sizeBucket : int;
  elems : List<'a>
}

type UList<'a> = Bucket<'a> list

let ulist01 = [ { sizeBucket = 4; elems = ['A';'B';'C';'D'] };
                { sizeBucket = 2; elems = ['E';'F'] };
                { sizeBucket = 6; elems = ['G';'H'; 'I'; 'J'; 'K'; 'L'] };
                { sizeBucket = 6; elems = ['M'; 'N'; 'O'; 'P'; 'Q'; 'R'] };
                { sizeBucket = 4; elems = ['S'; 'T'; 'U'; 'V'] };
                { sizeBucket = 4; elems = ['W'; 'X'; 'Y'; 'Z'] } ]

let ulist02 = [ { sizeBucket = 3; elems = ['A';'B';'C'] };
                { sizeBucket = 3; elems = ['D';'E';'F'] };
                { sizeBucket = 6; elems = ['G';'H'; 'I'; 'J'; 'K'; 'L'] };
                { sizeBucket = 6; elems = ['M'; 'N'; 'O'; 'P'; 'Q'; 'R'] };
                { sizeBucket = 4; elems = ['S'; 'T'; 'U'; 'V'] };
                { sizeBucket = 4; elems = ['W'; 'X'; 'Y'; 'Z'] } ]

ulist01 = ulist02

let emptyUL () : UList<'a> = []

let rec sizeUL ul =
    match ul with
    | [] -> 0
    | x::xs -> x.sizeBucket + sizeUL xs

sizeUL ulist01
sizeUL (emptyUL ())

let rec isEmptyUL ul =
    match ul with
    | [] -> true
    | x::xs -> x.sizeBucket = 0 && isEmptyUL xs

isEmptyUL ulist01
isEmptyUL (emptyUL())

let rec existsUL e ul = 
    match ul with
    | [] -> false
    | x::xs -> List.exists (fun x -> e = x) x.elems || existsUL e xs

existsUL 'A' ulist01
existsUL 'A' (emptyUL ())

let rec itemUL ul i = 
    match ul with 
    | [] -> failwith "Index out of bounds"
    | x::xs -> if (i+1) > x.sizeBucket 
               then itemUL xs (i-x.sizeBucket)
               else List.item i x.elems

itemUL ulist01 23

let rec filterUL p ul =
    match ul with
    | [] -> []
    | x::xs -> let elemsList = List.filter p x.elems 
               if elemsList = List.Empty then filterUL p xs else  {sizeBucket = (List.length elemsList); elems = elemsList} :: filterUL p xs

filterUL (fun e -> e < 'I') ulist01

let rec chkUL ul = 
    match ul with
    | [] -> true
    | x::xs -> x.sizeBucket = (List.length x.elems) && x.sizeBucket <> 0 && List.length x.elems < 4 && chkUL xs

chkUL ulist01

let rec map f ul = 
    match ul with
    | [] -> []
    | x::xs -> {sizeBucket = x.sizeBucket; elems = List.map f x.elems} :: map f xs

map (int) ulist01

let fold f a ul =
    let rec oneList ul' =
        match ul' with
        | [] -> []
        | x::xs -> x.elems @ oneList xs
    List.fold f a (oneList ul)


fold (fun a c -> a+((string)c)) "" ulist01

//Question 3.1

let rec G (m,n) = 
    match (m, n) with
    |(_, n) when n <= 0 -> m + n
    |(_, _) -> G(2*m,n-1) + m 

G(10,10)

let GA (m,n) = 
    let rec GARec (m, n, acc) =
        match (m, n, acc) with
        |(_, n, _) when n <= 0 -> m + n + acc
        |(_, _,_) -> GARec(2*m,n-1, acc+m) 
    GARec(m,n,0)

GA(10,10)

let mySeq = seq {for n in 1 .. 100 do
                                        for m in 1 .. 100 do
                                            yield (n,m)} 

Seq.take 4 mySeq

let gSeq = seq {for n in mySeq do
                                        yield GA n}

Seq.take 4 gSeq

// Question 4

type stack = int list

type inst =
    ADD
    | SUB
    | PUSH of int
    | LABEL of string
    | IFNZGOTO of string
    | EXIT

let insts01 =
  [PUSH 10;
   PUSH 12;
   ADD;
   EXIT]

let insts02 =
  [PUSH 10;
   LABEL "sub1";
   PUSH 1;
   SUB;
   IFNZGOTO "sub1";
   EXIT]

// Question 4.1
let execInsts insts =
        let rec exec insts s =
            match (insts,s) with
            | (SUB::is,v1::v2::s) -> exec is (v2-v1::s)
            | (PUSH v::is,s) -> exec is (v::s)
            | (ADD::is, v1::v2::s) -> exec is (v1+v2::s)
            | (EXIT::is, v1::s) -> v1
            | (LABEL lab::_,s) -> failwith "LABEL not implemented"
            | (IFNZGOTO lab::_,s) -> failwith "IFNZGOTO not implemented"
            | _ -> failwith "Missing stack values for instruction"
        exec insts []

execInsts insts01
execInsts insts02

// Question 4.2

type resolvedInst =
    | RADD
    | RSUB
    | RPUSH of int
    | RIFNZGOTO of int
    | REXIT

type prog = Map<int,resolvedInst>

let buildEnv insts =
    let rec build idx env = function
        | [] -> env
        | LABEL lab :: insts -> build idx (Map.add lab idx env) insts
        | _ :: insts -> build (idx+1) env insts
    build 0 Map.empty insts

buildEnv insts01
buildEnv insts02


type env = Map<string,int>
let lookup l m =
    match Map.tryFind l m with
    | None -> failwith "Value not in map"
    | Some v -> v

let resolveInsts insts env =
    let rec resolve idx = function
        [] -> Map.empty
        | LABEL lab :: insts -> resolve idx insts
        | ADD :: insts -> Map.add idx RADD (resolve (idx+1) insts)
        | SUB :: insts -> Map.add idx RSUB (resolve (idx+1) insts)
        | PUSH i :: insts -> Map.add idx (RPUSH i) (resolve (idx+1) insts)
        | EXIT :: insts -> Map.add idx REXIT (resolve (idx+1) insts)
        | IFNZGOTO lab :: insts -> Map.add idx (RIFNZGOTO (lookup lab env)) (resolve (idx+1) insts)
    resolve 0 insts