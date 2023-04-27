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
    | EmptyHP -> EmptyHP
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