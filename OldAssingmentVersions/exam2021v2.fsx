// Allan's version

type Rope =
      Leaf of string * int
    | Node of Rope * int * Rope

let rope1 = Node(Node(Leaf("I_lik",5),5,Leaf("e_functional_pr",15)),
                        20,
                        Node(Leaf("ogr",3),3,Leaf("amming",6)))


// 1.1
let rope2 = Node(Leaf("_and",4),4 , Node(Leaf("_very", 6), 6, Leaf("much_F#", 7)))

let rope3 = Node(Node(Leaf("Example_", 8), 8, Leaf("with_", 5)), 13, Leaf("5_nodes", 7))

// 1.2
let rec length r = 
    match r with
    | Leaf (str, n) -> n
    | Node(rpL, _, rpR) -> length rpL + length rpR

length rope1

let rec flatten r =
    match r with
    | Leaf (str, n) -> str
    | Node(rpL, _, rpR) -> flatten rpL + flatten rpR
    
flatten rope1

let rec maxDepth r = 
    match r with
    | Leaf (str, n) -> 1
    | Node(rpL, _, rpR) -> 
        if maxDepth rpL > maxDepth rpR 
        then maxDepth rpL + 1 
        else maxDepth rpR + 1

maxDepth rope1
    
let index i r = 
    if i > length r then failwith "Fail, man!" else (flatten r)[i]
index 5 rope1 
index 2 rope1 
index 30 rope1 


// 1.3
let concat r1 r2 =
    let parentWeight = length r1
    Node(r1, parentWeight, r2)    

let prettyPrint (r:Rope) = r.ToString() |> printfn "%s"
prettyPrint rope1;;

// 2.1
let list01 = ['A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J'; 'K'; 'L'; 'M';
                        'N'; 'O'; 'P'; 'Q'; 'R'; 'S'; 'T'; 'U'; 'V'; 'W'; 'X'; 'Y'; 'Z']

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

let ulist02 = [ { sizeBucket = 2; elems = ['A'; 'B'] };
                                    { sizeBucket = 24; elems = ['C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J'; 'K'; 'L'; 'M'; 'N'; 'O'; 'P'; 'Q'; 'R'; 'S'; 'T'; 'U'; 'V'; 'W'; 'X'; 'Y'; 'Z'] } ]

ulist01 = ulist02

let emptyUL = [{sizeBucket = 0; elems =[]}]

let sizeUL ul  = ul |> List.map (fun x -> x.sizeBucket) |> List.sum
sizeUL ulist01
sizeUL emptyUL

let isEmpty ul = sizeUL ul = 0
isEmpty emptyUL
isEmpty ulist01

let existsUL e ul = 
    ul |> List.map (fun x -> x.elems) |> List.concat |> List.exists (fun y -> y = e)

existsUL 'A' emptyUL
existsUL 'A' ulist01

let itemUL ul i =
    ul |> List.map (fun x -> x.elems) |> List.concat |> List.item i

itemUL ulist01 5 

let filterUL (p : 'a -> bool)  ul =
    ul |> List.filter (fun x -> x.elems |> List.forall p) 

filterUL (fun e -> e < 'I') ulist01

// 2.3

let chkMaxBucketSize ul = 
    ul |> List.map (fun x -> (x.elems |> List.length) <= 4) |> List.filter (fun x -> x = false) |> List.length = 0
chkMaxBucketSize ulist01
chkMaxBucketSize emptyUL

let chkBkSizeElem ul =
    ul |> List.map (fun x -> x.sizeBucket = (x.elems |> List.length)) |> List.filter (fun x -> x = false) |> List.length = 0
chkBkSizeElem ulist01


let chkUL ul =
    if isEmpty ul = true || chkMaxBucketSize ul = false ||chkBkSizeElem ul = false
    then false
    else true
    
chkUL emptyUL
chkUL ulist01

let map f ul =
    ul |> List.map (fun x -> x.sizeBucket, x.elems |> List.map f)

map (int) ulist01

let fold f a ul =
    ul |> List.map (fun x -> x.elems) |> List.concat |> List.fold f a
fold (fun a c -> a+((string)c)) "" ulist01

// 3.1

// not tail recursive
let rec G (m, n) =
    match n with
    | 0 -> n + m
    | _ -> G (2*m, n-1) + m

G(10,10)

// tail recursive
let GA (m, n) =
    let rec loop acc m n =
        match n with
        | 0 -> acc + m
        | _ -> loop (acc + m) (2 * m) (n - 1)    
    loop 0 m n

GA(10,10)

// 3.2
let mySeq = 
    let myList1 = [1..100]
    let myList2 = [1..100]
    myList1 |> List.allPairs myList2 |> List.toSeq

Seq.take 4 mySeq

let gSeq =
    mySeq |> Seq.map (fun x -> G (fst x, snd x))

Seq.take 4 gSeq

// 4.1

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

let execInsts insts =
    let rec exec insts s =
        match (insts, s) with
        | (SUB::is, v1::v2::s) -> exec is (v2-v1::s)
        | (ADD::is, v1::v2::s) -> exec is (v1+v2::s)
        | (PUSH i::is, s) -> exec is (i::s)
        | (EXIT::_, [top]) -> top
        | (LABEL lab::_,s) -> failwith "LABEL not implemented"
        | (IFNZGOTO lab::_,s) -> failwith "IFNZGOTO not implemented"
        | _ -> failwith "Missing stack values for instruction"
    exec insts []

type resolvedInst =
      RADD
    | RSUB
    | RPUSH of int
    | RIFNZGOTO of int
    | REXIT
    
type prog = Map<int,resolvedInst>

// let buildEnv insts =
//     let rec build idx env = function
//         | [] -> env
//         | LABEL lab :: insts -> build idx (Map.add idx (LABEL lab) env) insts
//         | inst :: insts -> build (idx+1) (Map.add (idx+1) inst env) insts
//     build 0 Map.empty insts

// buildEnv insts02

let buildEnv insts = 
    insts 
    |> List.map (fun x -> 
        match x with 
            | LABEL lab -> lab
            | _ -> "")
    |> List.indexed
    |> List.filter (fun x -> snd x <> "")
    |> List.map (fun x -> snd x, fst x)
    |> Map

buildEnv insts02


type env = Map<string,int>
let lookup l m =
    match Map.tryFind l m with
      None -> failwith "Value not in map"
    | Some v -> v

// let resolveInsts insts env =
    // Jeg forstår ikke rigtigt, hvad de beder om her!