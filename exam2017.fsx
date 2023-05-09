

type PrioritySet<'a when 'a: equality> = PrioritySet of List<'a>
let psEx = PrioritySet ["a";"b";"c"]


// Q1.1------------------------
// Consider the following elements and assume they are inserted in an empty priority set in this order: "a",
// "q", "a", "b", "b", "q", "d", "a".

// Declare a value priSetEx, being the result of inserting the elements above according to the
// definition of a priority set.
let priSetEx = PrioritySet ["a","q", "b", "d"]

// What is the type of the value priSetEx.
// PrioritySet of strings

// • Declare a value empty representing an empty priority set, i.e., priority set with no elements.
let empty = PrioritySet []

// Q1.2 ----------------------------------
// Declare a function
// isEmpty : PrioritySet<’a> -> bool when ’a : equality
// that returns true if a priority set is the empty set. For instance isEmpty(empty) returns true.
// The value empty is defined above.

let isEmpty s = s=empty 

let isEmpty2 (PrioritySet l)  = 
    match l with
    | [] -> true
    | _ -> false

isEmpty (priSetEx);;
isEmpty2 (psEx);;

// The size of a priority set is the number of elements in the set. Declare a function
// size : PrioritySet<’a> -> int when ’a : equality
// that returns the size of a priority set. For instance, size psEx returns 3.

let size (PrioritySet ps) = List.length ps
size psEx;;

// Declare a function contains e ps of type
// contains : ’a -> PrioritySet<’a> -> bool when ’a : equality
// that returns true if the priority set ps contains an element e. For instance contains "b" psEx
// returns true.
let contains elem (PrioritySet ps) = List.contains elem ps
contains "a" psEx;;

//  Declare a function getPN e ps of type
// getPN : ’a -> PrioritySet<’a> -> int when ’a : equality
// that returns the priority number of element e if exists in priority set ps. Otherwise raises an error
// exception (failwith). For instance getPN "a" psEx returns 1.
let getPN elem (PrioritySet ps) =
    if List.contains elem ps 
    then (List.findIndex (fun x -> x = elem) ps) + 1 
    else failwith "fail!"

getPN "a" psEx;;

// Question 1.3-----------------------------------------
//  Declare a function remove e ps of type
// remove : ’a -> PrioritySet<’a> -> PrioritySet<’a> when ’a : equality
// that removes element e from the priority set ps and returns a new priority set. Nothing changes if
// e does not exists in ps. For instance, remove "b" psEx returns the priority set PrioritySet
// ["a";"c"].

let remove elem (PrioritySet ps) = 
    let idx = (getPN elem (PrioritySet ps))-1
    if List.contains elem ps
    then List.removeAt idx ps
    else failwith "Nothing happened"

remove "b" psEx;;

// Declare a function
// add : ’a -> PrioritySet<’a> -> PrioritySet<’a> when ’a : equality
// where add e ps returns the priority set ps with the element e added with lowest priority (highest
// priority number) unless already in the set ps. Adding element h to priority set {a1, b2, c3} gives
// the priority set {a1, b2, c3, h4}. Adding element b to {a1, b2, c3} gives the unchanged priority set
// {a1, b2, c3}.

let add elem (PrioritySet ps) = 
    if List.contains elem ps
    then failwith "The element was already there!"
    else PrioritySet (List.append ps [elem])

add "h" psEx;;

// Question 1.4 ----------------------------
//  Declare a function map f ps of type
// map : (’a -> ’b) -> PrioritySet<’a> -> PrioritySet<’b>
// when ’a : equality and ’b : equality
// where map f ps returns the priority set where the function f has been applied on all elements
// in the priority set ps in order of priority number. For instance map (fun (c:string) ->
// c.ToUpper()) psEx returns the priority set value PrioritySet ["A";"B";"C"].
let map f (PrioritySet ps) = PrioritySet (List.map f ps)
map (fun (c:string) -> c.ToUpper()) psEx;;

// Declare a function cp of type 
// cp : PrioritySet<’a> -> PrioritySet<’b> -> PrioritySet<’a * ’b>
// when ’a : equality and ’b : equality
// where cp ps1 ps2 returns the cartesian product of ps1 and ps2. The result set is generated from
// ps1 and ps2 in order of priority number of ps1 first and then ps2. For instance the cartesian product
// of {A1, B2, C3} and {h1, i2} is {(A, h)1,(A, i)2,(B, h)3,(B, i)4,(C, h)5,(C, i)6}. A cartesian
// product involving an empty set is the empty set, eg. cp psEx empty is the empty set.
let cp (PrioritySet ps1) (PrioritySet ps2) = List.allPairs ps1 ps2

let ex5 = PrioritySet ["A";"B";"C"]
let ex6 = PrioritySet ["h";"i"]

cp ex5 ex6;;





// QUESTION 3.1-------------------------
// Consider the F# declaration:
let mySeq s1 s2 =
    seq { for e1 in s1 do
            for e2 in s2 do
                yield! [e1;e2] }
// of type seq<’a> -> seq<’a> -> seq<’a>.
// • Describe the sequence returned by mySeq when called with arbitrary sequences s1 and s2.

// It builds a sequence of lists as follows[e1(1), e2(1), e1(1), e2(2), ... , e1(1), e2(n), e1(2), e2(1), e1(2), e2(2), ... , e1(n), e2(n)]}
let ex31a = seq ["A";"B";"C"]
let ex31b=  seq ["h";"i"]

mySeq ex31a ex31b;;

// • Can you for any arguments to mySeq generate the following value 
// seq [’A’;’D’; ’A’;’E’; ’A’;’F’; ’B’;’D’; ’B’;’E’; ’B’;’F’]

// Yes
let ex31c = seq ["A";"B"]
let ex31d=  seq ["D"; "E"; "F"]
    
mySeq ex31c ex31d;;

// Question 3.2-------------------------
// Declare a function mySeq2 s1 s2 of type seq<’a> -> seq<’b> -> seq<’a * ’b> such that the
// cartesian product of s1 and s2 is returned. For instance mySeq2 [1;2] [’A’;’B’;’C’] gives the
// result seq [(1,’A’); (1,’B’); (1,’C’); (2,’A’); (2,’B’); (2,’C’)].
let mySeq2 s1 s2 = Seq.allPairs s1 s2
mySeq2 [1;2] ['A';'B';'C']

// Question 3.3-------------------------------------
// Declare a function mySeq3 of type int -> seq<int>, such that mySeq3 n produces the infinite
// sequence n
// 2 − n ∗ i for i >= 0. The identifier i is the index of the element in the sequence.
// Hint: Consider using Seq.initInfinite.

let mySeq3 n = Seq.initInfinite (fun i -> (n*n) - n*i)
mySeq3 3;;
