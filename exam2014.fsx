type OrderedList<'a when 'a : equality> =
    {front: 'a list;
     rear: 'a list}
    
let ex = {front = ['x']; rear = ['z';'y']}
to
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
let newOL ()= {front = []; rear = []}

let isEmpty ol = if ol.front = [] && ol.rear = [] then true else false

// Question 1.4

let addFront x ol = {front = x::ol.front; rear = ol.rear}

let removeFront (ol: OrderedList<'a>) = 
    let x::xs = toList ol
    (x, {front = xs; rear = []})

let peekFront ol = 
    let x::xs = toList ol
    x

// Question 1.5

let append ol1 ol2 = {front = toList ol1; rear = List.rev (toList ol2)}

// Question 1.6

let rec map f ol = 
    let rec mapR = function
        |x::xs -> f x :: mapR xs
        |[] -> []
    {front = mapR (toList ol); rear = []}

// Question 1.7

let fold f start ol = 
    let rec foldR f' start' = function
        |x::xs -> foldR f' (f' start' x) xs 
        |[] -> start'
    foldR f start (toList ol)

    