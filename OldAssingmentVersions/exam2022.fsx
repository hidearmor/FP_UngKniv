// Question 1

type TrashItem<'a> =
    Paper of string
    | Glass of string
    | Other of 'a

let item1 = Other ("Shirt", "Clothes")
let item2 = Paper "Newspaper"
let item3 = Other ("Sneakers", "Shoe")
let item4 = Glass "Wine glass"
let item5 = Paper "Flyer"
let item6 = Other ("Loafers", "Shoe")

// Question 1.1

// item1 and item2 are both of type TrashItem<'a>. The two items use different constructors 
// of the type, i.e item1 uses the Other constructor and item2 uses the Paper constructor.

// the type TrashItem<'a> is polymorphic, indicated by the 'a. This is utilised in the 
// Other constructor

let items = [Paper "Magasine"; Glass "Bottle"; Glass "Jam"; Other("Beer can", "Aluminium"); Other("Bag", "Plastic")]

let fnPP (n,t) = sprintf "%O (%O)" t n 

let ppTrashItem fnPP = function
    | Paper s -> sprintf "Paper (%s)" s
    | Glass s -> sprintf "Glass (%s)" s
    | Other s -> fnPP s

// > ppTrashItem fnPP item1;;
// val it: string = "Clothes (Shirt)"

// > ppTrashItem fnPP item2;;
// val it: string = "Paper (Newspaper)"

// > ppTrashItem fnPP item4;;
// val it: string = "Glass (Wine glass)"

let isPaper item = 
    match item with
    | Paper s -> true
    | _ -> false


// Question 1.2

type TrashCan<'a> =
    Empty
  |TrashItems of TrashItem<'a> * TrashCan<'a>

let addItem item tc = TrashItems (item, tc)

let rec ofList ts = 
    match ts with
    |[] -> Empty
    |x::ts' -> addItem x (ofList ts')

let tcEx = ofList items

let rec forAll fnP tc = 
    match tc with
    | Empty -> true
    | TrashItems (ts, tc') -> if fnP ts then forAll fnP tc' else false


let isSameCategory item1 item2 = 
    match (item1, item2) with
    | (Paper s, Paper s') -> true
    | (Glass s, Glass s') -> true
    | (Other s, Other s') -> true
    | _ -> false

let isSorted tc =
    match tc with 
    | Empty -> true
    | TrashItems (ts, tc') -> forAll (isSameCategory ts) tc'

// Question 1.3

let rec fold f e tc = 
    match tc with 
        |TrashItems (ts, tc) -> fold f (f e ts) tc 
        |Empty -> e


let rec sort tc =
    match tc with
    | TrashItems (ts, tc') -> 
        match ts with 
        | Paper s -> let (paperTc, glassTc, otherTc) = sort tc'
                        (addItem ts paperTc, glassTc, otherTc)
        | Glass s -> let (paperTc, glassTc, otherTc) = sort tc'
                        (paperTc, addItem ts glassTc, otherTc)
        | Other s -> let (paperTc, glassTc, otherTc) = sort tc'
                        (paperTc, glassTc, addItem ts otherTc)
    | Empty -> (Empty, Empty, Empty)


    


// Question 2

type Node<'a> = Root of 'a
              | Link of 'a * Node<'a> ref


let mkRootElem a = ref (Root a)
let mkLinkElem a e =
    ref (Link(a,e))
let elemA = mkRootElem 'A'
let elemB =
    mkLinkElem 'B' elemA
let elemC =
    mkLinkElem 'C' elemB
let elemM = mkRootElem 'M'
let elemN =
    mkLinkElem 'N' elemM


// Question 2.1

let getValue e = 
    match e with 
    | Root a -> a
    | Link (a, e) -> a

let rec pathlength e = 
    match e with
    | Root a -> 0
    | Link (a, e') -> 1 + (pathlength !e')


// Question 2.2

let rec find (e: Node<'a> ref) =
    match !e with
    | Root a -> e
    | Link (a, e') -> find e'

find elemA = find elemB

find elemA = elemA

find elemB = elemA

find elemB = elemB

find elemC = find elemN

let union e1 e2 =
    let e1' = find e1
    let e2' = find e2
    match !e2' with
    | Root v -> Link (v, e1')
    | Link _ ->
        failwith "union: got Link after find."

// Question 3

let rec f x = function
    [] -> []
  | y::ys when x=y -> f x ys
  | y::ys when x <> y ->  
      match f x ys with
        ys' -> y::ys'

f 10 [10; 3; 4; 10; 5; 10; 6]