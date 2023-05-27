//  °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
//  °°°°°°°°°°°°°°   LISTS   °°°°°°°°°°°°°°°°°°°
//  °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°

// længere nede er en liste over brugbare list-funktioner, med eksempler hvor nødvendigt

// make some lists

let animals = ["Dog" ; "Cat" ; "Axolotle" ; "Penis-Frog"];;

let decreasingBy2 = [10..-2..0];;
// val decreasingBy2: int list = [10; 8; 6; 4; 2; 0]

//  °°°°°°
// APPEND
let list1 = [1;2;3]
let list2 = [4;5;6]

let appended = list1 @ list2        
// takes time proportional to list1

// the function for append:
let rec (@) xs ys =     // val (@) : ’a list -> ’a list -> ’a list
     match xs with
     | []     -> ys
     | x::xs' -> x::(xs' @ ys);;

//  °°°°°°
// How to reverse a list? - don't write a function that utilizes @ recursively
// use List.rev instead

let reverseList1 = List.rev [1;2;3]
// val reverseList1: int list = [3; 2; 1]

//  °°°°°°
//  check if member
let rec isMember x = function
    | [] -> false
    | y::ys -> x=y || isMember x ys;;

//  °°°°°°
//  SPLIT A LIST
let rec split = function
    | []       -> ([],[])
    | [x]      -> ([x],[])
    | x::y::xs ->   let (xs1,xs2) = split xs 
                    (x::xs1,y::xs2)


//  °°°°°°°°°°°° COLLECTION FUNCTIONS °°°°°°°°°°°°°°°°
// most of these are usable with both list and sequence

// dummy lists
let l1 = [1;2]
let l2 = [5;2]
let l3 = [10..-1..3]
let l4 = [200;0;3]

// concatenate a sequence odf lists to a new list
List.concat (seq{[1;2];[99;88]}) //val it: int list = [1; 2; 99; 88]

// appends ONE list to ONE other list
List.append [2;1] [66;55] //val it: int list = [2; 1; 66; 55] 

List.Empty

List.Cons // equal to: let cons elem list =  elem::list

List.map (fun x -> x+1) [1..4]  //val it: int list = [2; 3; 4; 5]

List.collect  // works like map but collects the resulting lists in the end
[1..4] |> List.collect (fun x -> [1..x]) 
(*
    val it: int list = 
    [1; 
    1; 2; 
    1; 2; 3; 
    1; 2; 3; 4]
*)

// List.find returns the FIRST MATCH
let testFunction x = if x = 3 then true else false 
List.find testFunction [1..5] 
// val testFunction: x: int -> bool
// val it: int = 3

List.tryFind    // safer version of find

List.forall     // return true if LL elements fulfill the predicate
List.filter     // returns list with elements fulfill a predicate defined by a function
List.choose     // a bit like filter, but the function returns an option type
                // and only returns those elements where the funciton results in Some('a)

List.exists //check if element exists in list

// FOLD, FOLD BACK
// are accumulative functions 
// that updates a state using "every" element in the list
// return a singe value/state, NOT list
List.fold
(*example making the sum of squares, witten in two ways*) 
(0, [1..5]) ||> List.fold (fun s v -> s + v * v) // val it: int = 55
let sqrL = [1..5]
let foldr = List.fold

List.foldBack // fold starting from the back
List.unfold // unfolds to "the next step" has t obe used recursively

// very close to fold, but does not update a state throughout, but rater 
// only uses the current and the next element in the list
// and a function to reduce them
List.reduce (fun x y -> x + y) [0..9] //val it: int = 45
// This one adds all numbers but one by one


List.skip       // returns list skipping n elements
List.singleton  // list with the one input item
List.head       // returns first eelement of list
List.init 5 (fun x -> x+2)   // val it: int list = [2; 3; 4; 5; 6] 
// generates a list of length 5 using a function on index x for each element

List.zip        // zips each element of two lists together
List.unzip      // creates two lists putting all odd indexes in one and even in the other
List.toSeq [1;2;3]      // val it: seq<int> = [1; 2; 3]
                        // creates sequence from list
List.toArray    // to array
List.rev        // reverses list efficiently


List.compareWith    // compares elements from two lists index-wise 
                    // returns if match
List.countBy id [4; 5 ; 1 ; 1 ; 6 ; 99; 4] // this counts the instances of each number
// val it: (int * int) list = [(4, 2); (5, 1); (1, 2); (6, 1); (99, 1)]

List.sort
List.sortBy 


//  °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
//  °°°°°°°°°°°°   Sequences   °°°°°°°°°°°°°°°°°
//  °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°

(*
    Sequences are lazy and thus the elements are only evaluated 
    when they are needed (roughly) - can be really effective
*)

// natural sequence
let nat = Seq.initInfinite (fun i -> i);; // val nat : seq<int>

//  °°°°°°°°°°°° SEQUENCE FUNCTIONS °°°°°°°°°°°°°°°°

// MOST LIBRARY-FUNCITONS for collections are the same
// i.e. most funcitons for Seq are the same as for List
let dummy = Seq.init 5 (fun i -> i) // val it: seq<int> = seq [0; 1; 2; 3; ...]


// A few examples:
Seq.average (Seq.init 5 (fun i -> (i:float))) // val it: float = 2.0
Seq.sum dummy //val it: int = 10
Seq.sumBy (fun x -> x+x) dummy //val it: int = 20