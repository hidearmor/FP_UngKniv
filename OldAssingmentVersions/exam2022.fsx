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

f 11[1..10] 

// Allan's
// Type 'a -> 'a list -> 'a list

// The function keyword indicates similar as match ... with. Int his case the ... is a list (see pattern
// matching). In other words, the function takes an int (x) and a list. Then it takes first element and compares to
// x. If they are equal., it calls itself with x and the rest of y.
// If first element is not equal to x, it matches again, this time using ys (the rest of list) and x as arguments
// The result of this match is bound to ys' and then the top of that list is consed onto the list.
// The function should return a a list, where x is removed (if it was in the list) otherwise it just returns the original list

// 3.3 - Allan's
// tail recursive

let rec fA x =
    let rec fA' acc = function
          [] -> List.rev acc
        | y::ys when x=y -> fA' acc ys
        | y::ys when x <> y -> fA' (y::acc) ys
    fA' []

fA 3 [1..10]

// 3.4 - Allan's
let fSeq x ys =
    seq { for n in ys do
                        if n<>x then yield n}
fSeq 2 {1..10}

// 4.1 - Allan's
type Field = ValField of int
type FieldMap = Map<string, Field>

let fieldMap = Map.ofList [("A", ValField 42);
                                                        ("B", ValField 43);
                                                        ("C", ValField 0)]
let fieldNames = List.map fst (Map.toList fieldMap)

let getField fM fN = fM |> Map.find fN 

getField fieldMap "A"
List.map (getField fieldMap) fieldNames

let getFieldValue (fi: Field) = 
    match fi with
    | ValField value -> value
getFieldValue (getField fieldMap "A")

let lookupFieldValue (fM : Map<'a, 'b>) fN = getField fM fN |> getFieldValue
List.map (lookupFieldValue fieldMap) fieldNames

// USing Map.change
let updateField2 fM (fN : string ) nVal = 
    fM |> Map.change fN (fun x -> 
        match x with 
        | Some v -> Some (ValField nVal)
        | None -> None
    )
updateField2 fieldMap "A" 32


// Using Map.add
let updateField fM fN nVal = 
    let myMap = fM |> Map.add fN (ValField nVal)
    myMap

updateField fieldMap "A" 32
    

// 4.2 - Allan's
type Function =
        FnOneToOne of string * string * (int -> int)
type FuncMap = Map<string, Function>
let fnAddTwoAC = ("fnAddTwoAC", FnOneToOne ("A", "C", fun a -> a + 2))
let fnNegateAB = ("fnNegateAB", FnOneToOne ("A", "B", fun a -> -a))
let funcMap = Map.ofList [fnAddTwoAC;fnNegateAB]


let evalFn fM (funcN : Function)  =  
    match funcN with
    | FnOneToOne (strA, strB, myFunction) -> 
        let valA = lookupFieldValue fM strA
        let valB = valA |> myFunction
        updateField fM strB valB
        
evalFn fieldMap (snd fnAddTwoAC)

let findFunctionsBySource sF (fMap : FuncMap) =
    fMap |> Map.toList |> List.filter (fun fx -> 
        match snd fx with
        | FnOneToOne (strA, _, _) -> strA = sF)

findFunctionsBySource "A" funcMap
     

//  Below is giving me a hard time / Allan
let evalField fieldName funcMap fieldMap=
    let myFunctions = findFunctionsBySource fieldName funcMap
    myFunctions 
        |> List.map (fun x  -> evalFn fieldMap (snd x))

evalField "A" funcMap fieldMap

// ^^Not quite there - but on the way...