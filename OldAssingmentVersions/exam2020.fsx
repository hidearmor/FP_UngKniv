type mymap<'a,'b> = MyMap of list<'a*'b>

let ex1 = MyMap [('A',65);('B',66);('C',67)]
let ex1' = MyMap [('C',67);('A',65);('B',66)]

// Question 1.1
let dice1 = MyMap [(1,4);(2,2);(3,3);(4,2);(5,2);(6,2)]
let dice2 = MyMap [(1,4);(2,2);(3,3);(4,3);(5,5);(6,3)]

// The type of ex1 is mymap<char, int> and the type of dice 1 is type mymap<int, int>

let emptyMap() = MyMap []

let size m = 
    match m with
    |MyMap l -> List.length l

size dice1
size ex1
size (emptyMap ())

// Question 1.2

let isEmpty m = 
    match m with
    | MyMap l -> List.isEmpty l

isEmpty (emptyMap ())
isEmpty ex1

let tryFind k m =
    match m with
    |MyMap l -> 
        let rec tryFindRec = function
            | [] -> None
            | (a,b)::xs -> if a = k then Some (a,b) else tryFindRec xs
        tryFindRec l

tryFind 'B' ex1
tryFind 'D' ex1

// when 'a : equality is a restriction to the polymorphic type 'a, that it must implement a type that supports equality operations. 
// functions for instance doesn't do that.

let remove k m = 
    match m with 
    | MyMap l ->
        let rec removeRec x = function
            | [] -> l
            | (a,b)::xs -> if a = k then List.removeAt x l else removeRec (x+1) xs
        MyMap (removeRec 0 l)

remove 'D' ex1

let add k v m  = 
    match m with
    | MyMap l ->    
        let rec addRec x = function
            | [] -> (k,v)::l
            | (a,b)::xs -> if a <> k then addRec (x+1) xs else (k,v)::(List.removeAt x l)
        MyMap (addRec 0 l)

add 'D' 68 ex1
add 'A' 222 ex1


// Question 1.3

let upd f k v m = 
    match tryFind k m with
    | Some(k, v') -> add k (f v v') m
    | None -> add k v m


upd (+) 'A' 65 ex1
upd (+) 'D' 68 ex1

let map f m = 
    match m with
    | MyMap l ->
        let rec mapRec = function
            |[] -> []
            |(a,b)::xs -> (a,(f a b))::(mapRec xs)
        MyMap (mapRec l)

map (fun k v-> v+2) ex1


let fold f s m = 
    match m with
    | MyMap l -> List.fold f s l

fold (fun s (k, v) -> s+v) 0 dice1


// Question 2.1

let even n = if n % 2 = 0 then true else false
even 42
even 1

let collatz n = if (even n) then n/2 else 3*n+1

collatz 45

let collatz' n = if n <= 0 then raise (System.Exception "collatz': n is zero or less.") else collatz n

collatz' 0
collatz' 45

// Qustion 2.2
let rec applyN f n N =
    match N with
    | -1 -> []
    | N when N > -1 -> n::applyN f (f n) (N-1)

applyN collatz 42 8

let applyUntilOne f n =
    let rec applyUntilOnerec f n acc =
        match n with
        | 1 -> acc
        | n when n > 1 -> applyUntilOnerec f (f n) (acc+1)
    applyUntilOnerec f n 0

applyUntilOne collatz 42

// Question 2.3
let rec mySeq f x =
  seq { yield x
        yield! mySeq f (f x)}

mySeq collatz 42
Seq.item 100 (mySeq collatz 42)
// The sequence return by the example above is identical to the list returned in (applyN collatz 42 8)
// When building a list from a sequence expression as above, the sequence is lazy evaluated - 
// this means the full seqeunce is not created until a specific element in the sequence is called for:
// Seq.item 100 (mySey collatz 42) for instance yields a computation of the first 101 elements returning the one at
// index 100

let g x = x * 2

Seq.take 6 (mySeq g 1)


