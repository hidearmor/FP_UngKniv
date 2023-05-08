type OrderedList<'a when 'a : equality> =
    {front: 'a list; // <- the syntax with name (label) and value in brackets is a RECORD
     rear: 'a list}
    
let ex = {front = ['x']; rear = ['z';'y']}

// Question 1.1 

let ol1 = {front = ["Hans"; "Brian"; "Gudrun"]; rear = []}

let ol2 = {front = ["Hans"; "Brian"]; rear = ["Gudrun"]}
let ol3 = {front = ["Hans"]; rear = ["Gudrun"; "Brian"]}

// Declaration of ol2 and ol3 where the rear list is not empty
// In total there exist 4 representations of the list - the fourth being with an 
// empty front list as declared below:

let ol4 = {front = []; rear = ["Gudrun"; "Brian"; "Hans"]}


//Question 1.2

let canonical (ol: OrderedList<'a>) = {front = ol.front @ List.rev ol.rear; rear = []}

// the above function appends the front list of ol with the reverse rear list of ol, and sets the
// rear list to be the empty lis.

let toList (ol: OrderedList<'a>) = (canonical ol).front

// the function toList ol, uses canonical as a helper function to put all elements of ol in the front list
// and then returns only the front list of ol


// Question 1.3
let newOL () = {front = []; rear = []}

let isEmpty ol = if ol.front = [] && ol.rear = [] then true else false

// Question 1.4

let addFront x ol = {front = x::ol.front; rear = ol.rear}

let removeFront (ol: OrderedList<'a>) = 
    let oll = toList ol
    match oll with
    |[] -> failwith "list is empty"
    |x::xs -> (x, {front = xs; rear = []})

let peekFront ol = 
    if isEmpty ol then failwith "no elements" 
        else
        let x::xs = toList ol
        x

// Question 1.5

let append ol1 ol2 = {front = toList ol1; rear = List.rev (toList ol2)}

// Question 1.6

let map f ol = 
    let rec mapR = function
        |x::xs -> f x :: mapR xs
        |[] -> []
    {front = mapR (toList ol); rear = []}
    // {front = mapR (ol.front); rear = mapR ol.rear}   // <- returns the same order of the ordered list


// Question 1.7

let fold f start ol = 
    let rec foldR f' start' = function
        |x::xs -> foldR f' (f' start' x) xs 
        |[] -> start'
    foldR f start (toList ol)

// Question 1.8

let multiplicity ol = fold 
                        (fun e x -> 
                            if Map.containsKey x e then Map.add (x) ((Map.find x e)+1) (e) 
                            else Map.add x 1 e) 
                        Map.empty ol 


// Question 2.1

// f changes the values of the elements in the list, by some number related to i. 
// It returns a new list with all values changed. The value of i in the last recursive call
// is added to the end of the list, so the length of the output list is one greater than 
// the lenght of the input list.
// From the above follows that f, can never return an empty list. If you cal f with an empty list
// you will get a list containing one element (namely i) as output
// I don't see how f could go into an infinite loop, since we iterate over the list which by construction
// has a fintie number of elements. 

// Question 2.2

let rec fA acc i = function
    |[] -> List.rev (i::acc)
    |x::xs -> fA ((i+x)::acc) (i+1) xs

let fA2 j l = 
    let rec fArec acc i = function
        |[] -> List.rev (i::acc)
        |x::xs -> fArec ((i+x)::acc) (i+1) xs
    fArec List.Empty j l

let ex1 = fA List.Empty 10 [0;1;2;3]
let ex2 = fA2 10 [0;1;2;3]

// Question 2.3

let rec fC c i = function
    |[] -> c [i]
    |x::xs -> fC (fun l -> c ((i+x)::l)) (i+1) xs

let ex3 = fC id 10 [0;1;2;3]

// Question 3.1

let myFinSeq n M = Seq.map (fun m -> n+n*m) [0..M]

// the sequence resulting from this function when inputting two arbitrary integers n and M 
// is a sequence of length M+1, that has that value n as the first and then 2n as the next
// and so on until the M'th element, which is that last.
// I.E. the multiplication table for n for the first M+1 elements

// Question 3.2

let mySeq n = Seq.initInfinite (fun i -> n+n*i)

// Question 3.3

let mt N M = 
    let rec f m = function
        | [] -> Seq.empty
        | n::ns -> Seq.append (Seq.singleton (n, m, n*m)) (f (m) (ns))
    Seq.map 
        (fun m -> f (m) [0..N]) 
        [0..M] 
        |> Seq.concat

mt 3 2

Seq.item 11 (mt 3 2)
// to return the element triples (n,m,n*m) where n 2 [0,...,N] and m 2 [0,...,M].