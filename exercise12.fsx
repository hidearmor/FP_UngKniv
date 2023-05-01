module a12

//Question 1.1

type Heap<'a when 'a: equality> =
    | EmptyHP
    | HP of 'a * Heap<'a> * Heap<'a>

let ex3 = HP(1, HP(2, HP(3, EmptyHP, EmptyHP), HP(5, EmptyHP, EmptyHP)), HP(4, EmptyHP, EmptyHP))

// The type of ex3 is Heap<int>, which is a monomorphic type. Type Heap is defined as polymorphic using the 'a type,
// but as soon as we declare an instance of type Heap, we have to define the type of 'a, which in this case is of type int.

let empty = EmptyHP

// above is a declaration of an empty Heap, which has the type Heap<'a when 'a: equality>

exception HeapError of string

// Since the above exception has type string, you can define the given error when raising the exception.


//Question 1.2

let isEmpty = function
    | EmptyHP -> true
    | _ -> false 

// The above function isEmpty mathces the input with the type Heap<'a>. If the input equals EmptyHP, then the funciton
// returns true, otherwise it returns false.

let size h =
    let rec sizeTR k acc cont =
        match k with
        | EmptyHP -> cont acc
        | HP(x, l, r) -> sizeTR l (1+acc) (fun leftCount -> sizeTR r (leftCount) cont)
    sizeTR h 0 id

let size2 h =
    match h with 
    | EmptyHP -> 0
    | HP (x, l, r) -> 1 + size l + size r

// The above function size h, traverses through the heap using tail-recursion, counting all the non-empty nodes.
// The function size2 h, is a quick 'n dirty, hit 'n run recursive function, that does the job, but is naïve.

let find h = 
    match h with
    | EmptyHP -> failwith "Dooh! The heap is empty"
    | HP (x, _, _) -> x



// find h returns the value of the root, which per definition is the minimum value in the heap. If the heap is empty, 
// it fails with an error message. Somehow the function doesn't work if h = EmptyHP ???


let rec chkHeapProperty h = 
    match h with
    | EmptyHP -> true
    | HP (x, l, r) ->
        let smallerThanLeft = match l with
                                    | EmptyHP -> true
                                    | HP (xl, _, _) -> x < xl
        let smallerThanRight = match r with
                                    | EmptyHP -> true
                                    | HP (xr, _, _) -> x < xr
        smallerThanLeft && smallerThanRight && chkHeapProperty l && chkHeapProperty r

// the function above traverses through the heap and compares all the nodes with the value of the children nodes


// Question 1.3

let rec map f h = 
    match h with
    | EmptyHP -> raise (HeapError "Error")
    | HP(x, l, r) -> HP(f x, map f l, map f r)

// the function above traverses through the tree, calling the function f on every Node value x, and returning 
// EmptyHP when the node is empty. 

let f x = x % 2

chkHeapProperty (map f ex3)

// Defining the function f x = x % 2, will in our (and most cases) make chkHeapProperty (map f ex3) return false


// Question 2.1
let random =
         let rnd = System.Random()
         fun () -> rnd.Next(1,10000)

let getRandoms n = Array.init n (fun _ -> random ())

let getRandomsP n= Array.Parallel.init n (fun _ -> random ())

// Above is the solution for getRandoms and getRandomsP (which is similar, but utilises parallel computation)
// However note that the parallel function is actually SLOWER than the non-parallel version. This can be due
// to the fact the creating randoms numbers is a very easy computational task, so in fact the parallel version
// is slowed down because it uses extra computational resources to keep track of different threads i.e. 

// Question 2.2
let split xs = 
    let lgt = List.length xs
    List.splitAt (lgt/2) xs

let splitTest1 = []
let splitTest2 = [1]
let splitTest3 = [1; 2]
let splitTest4 = [1; 2; 3]

// the functions splits the list xs at half its length. If the list contains an uneven amount of elements
// the "right" list will be one element larger than the "left" list. This also aplies if the list has exactly one
// element. NOTE that the split function doesn't work on the empty list. 


let indivisible xs = if List.length xs < 2 then true else false

// the function indivisible xs returns true if the list contains 0 or 1 element, else it returns false

let rec merge(xs, ys) = 
    match xs, ys with
    | xs, [] -> xs
    | [], ys -> ys
    | x::xs', y::ys' -> if x < y then x::merge(xs', ys) else y::merge(xs, ys')

let list1 = []
let list2 = [5]
let list3 = [2; 4; 7; 8]
let list4 = [1; 3; 6; 7; 23]

merge(list1, list2)
merge(list3, list1)
merge(list4, list3)

// the function merge(xs, ys) merges and sorts two lists, and the different test cases exhibits this.

//(’a -> ’a * ’a) -> (’a * ’a -> ’a) -> (’a -> bool) -> ’a -> ’a
let divideAndConquer split merge indivisible p = 
    let rec dc p = 
        if indivisible p
            then p
            else 
                let x,y = split p
                merge((dc y),(dc x))
    dc p

// divideAndConquer split merge indivisible [22;746;931;975;200];;
// val it: int list = [22; 200; 746; 931; 975]

// this version puts in a list, if it's divisible it splits it, then merges the result 
// of the recursive call of each half - when the list gets ti it's finest granularity
// it just returns the current number. Since each split is merged when the recursion,
// balance will always be maintained


//Question 3.1
let triNum = Seq.initInfinite (fun n -> (n*(n+1))/2)
// Seq.initInfinite takes a function that generates an item in the sequence 
// --- from a given index, in this case n

let triNumC = Seq.cache triNum

// triNumC utilises the Seq.cache function to establish a cached version of triNum.

//Question 3.2

let rec filterOddIndex s =
    Seq.append (Seq.singleton (Seq.item 0 s))
        (filterOddIndex (Seq.skip 2 s))

// Above function taken from exam paper

let rec myFilterOddIndex s = 
    Seq.delay (fun () -> Seq.append (Seq.singleton (Seq.item 0 s)) (myFilterOddIndex (Seq.skip 2 s)))

// The above function myFilterOddIndex s takes the structure from filterOddIndex s but wraps it into Seq.delay to avoid infinite looping.

//Question 3.3

let rec seqZip s1 s2 = 
    seq {
        let e1 = Seq.item 0 s1
        let e2 = Seq.item 0 s2
        yield (e1, e2)
        yield! seqZip (Seq.skip 1 s1) (Seq.skip 1 s2)}

// the function seqZip zips two seqeunces together, by using yield and yield!

//Question 4.1

exception FigError of string
type Point = P of double * double
type Fig =
      Circle of Point * double
    | Line of Point * Point
    | Move of double * double * Fig
    | Combine of Fig list
    | Label of string * Fig
    | Ref of string

let rectEx = Combine[
    Line(P(-1.0,-1.0),P(1.0,-1.0));
    Line(P(1.0,-1.0),P(1.0,1.0));
    Line(P(1.0,1.0),P(-1.0,1.0));
    Line(P(-1.0,1.0),P(-1.0,-1.0))]

//The above value rectEx of type Fig represents a rectangle in the two-dimensional space

let rect (x1,y1) (x2, y2) = Combine[
    Line(P(x1,y1),P(x2,y1));
    Line(P(x2,y1),P(x2,y2));
    Line(P(x2,y2),P(x1,y2));
    Line(P(x1,y2),P(x1,y1))]

// The above functions creates a rectangle based on the two pair of doubles (coordinates)

// Question 4.2

let figEx02 =
   Combine [Label("c",Circle(P(0.0,0.0),1.0));
            Move(1.0,1.0,Ref "c");
            Move(2.0,2.0,Ref "c")]

// example from exam paper


// let figEx03 =
//    Combine [Label("c",Circle(P(0.0,0.0),1.0));
//             Move(1.0,1.0,Ref "c");
//             Move(2.0,2.0,Ref "c");
//             Label("l", Line(P(1.0,-1.0),P(1.0,1.0)))
//             Line(P(-1.0,1.0),P(-1.0,-1.0))]
// // Anotheg Fig example



let rec buildEnv fig =
    match fig with 
    | Label(str, f) -> Map[(str, f)]
    | Combine(list) ->
        let rec buildEnvRec lis =
            match lis with 
            | f::lis' -> Map (Seq.concat [(Map.toSeq (buildEnv f)) ; (Map.toSeq (buildEnvRec lis'))])
            | [] -> Map []
        buildEnvRec list
    | _ -> Map[]

// 



