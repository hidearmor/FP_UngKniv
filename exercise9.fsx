// 2013-04-18 * sestoft@itu.dk

// Similar to the development of monad-based interpreters from
// scala/ExpressionsMonads.scala

// Very simple expressions

// type expr =
//     | CstI of int
//     | Prim of string * expr * expr


// Exercise 1. ------------------------------------------------------------------------------------ 
// Extend the expression language and monadic evaluators with single-argument functions such as ABS(e1) 
// which evaluates e1 and produces its absolute value.  
// Do this by adding a new case Prim1 of string * expr to the expr datatype.  
// Create suitable variants of all the monadic evaluators; you should not have to change the 
// monad definitions (OptionBuilder, SetBuilder, TraceBuilder) at all.  Abstract out the action of 
// ABS on its argument in new auxiliary functions opEvalOpt1, opEvalSet1 and opEvalTrace1 similar to 
// the existing functions opEvalOpt, opEvalSet and opEvalTrace for two-argument primitives.  
// 
// Try the new evaluators on eg these expressions:
// let expr10 = Prim1("ABS", Prim("+", CstI(7), Prim("*", CstI(-9), CstI(10))))
// let expr11 = Prim1("ABS", Prim("+", CstI(7), Prim("/", CstI(9), CstI(0))))
// let expr12 = Prim("+", CstI(7), Prim("choose", Prim1("ABS", CstI(-9)), CstI(10)))
// --------------------------------------------------------------------------------------------------

type expr =
    | CstI of int
    | Prim of string * expr * expr
    | Prim1 of string * expr




// ------------------------------------------------------------

// Plain evaluator, return type int

let rec eval1 e : int =
    match e with
    | CstI i -> i
    | Prim(op, e1, e2) ->
        let v1 = eval1 e1
        let v2 = eval1 e2
        match op with
        | "+" -> v1 + v2
        | "*" -> v1 * v2
        | "/" -> v1 / v2
    | Prim1(op1, e1) ->       //<-- Allan's first shot EXERCISE1 
        let v = eval1 e1               //  Not quite theere, but feel it should be 
        match op1 with                       // something like this
        | "ABS" -> abs v                     // abs is a built-in Math Operator (https://www.dotnetperls.com/math-fs)


let opEval op v1 v2 : int =
    match op with
    | "+" -> v1 + v2
    | "*" -> v1 * v2
    | "/" -> v1 / v2
    //| "ABS" -> abs v1 ... for exercise 1 we could probably have used this and called OpEval with "opEval op v 0"

let EvalOpt1 op v:int =   //<--- Allan's EXERCISE 1
    match op with                                //to be used here and there for exercise 1
    | "ABS" -> if v<0 then -v else v

let rec eval2 e : int =
    match e with
    | CstI i -> i
    | Prim(op, e1, e2) ->
        let v1 = eval2 e1
        let v2 = eval2 e2
        opEval op v1 v2
    | Prim1(op1, e1) ->
        let v = eval2 e1
        match op1 with                  //<-- Allan's 1st shot EXERCISE 1
        | "ABS" -> abs v                // Same as for eval1, not quite there

type IdentityBuilder() =
    member this.Bind(x, f) = f x
    member this.Return x = x
    member this.ReturnFrom x = x

let identM = new IdentityBuilder();;

let rec eval3 e : int =
    match e with
    | CstI i -> identM { return i }
    | Prim(op, e1, e2) ->
        identM  { let! v1 = eval3 e1
                  let! v2 = eval3 e2
                  return! opEval op v1 v2 }
    | Prim1(op, e1) ->             //<-- Allan's 1st shot EXERCISE 1
        identM { let! v = eval3 e1            // not really sure about op here?
                 return! EvalOpt1 op v}          

// ------------------------------------------------------------

// Evaluator that may fail, return type: int option

let rec optionEval1 e : int option =
    match e with
    | CstI i -> Some i
    | Prim(op, e1, e2) ->
        match optionEval1 e1 with
        | None -> None
        | Some v1 ->
            match optionEval1 e2 with
            // | None -> None               //Original None
            | None ->                       //<-- Allan's EXERCISE 1 1st shot
                match op with               //if there is something in e1 and nothing in e2
                | "ABS" -> Some(abs v1)     // then return the absolute of e1
            | Some v2 ->
                match op with
                | "+" -> Some(v1 + v2)
                | "*" -> Some(v1 * v2)
                | "/" -> if v2 = 0 then None else Some(v1 / v2)

let opEvalOpt op v1 v2 : int option =
    match op with
    | "+" -> Some(v1 + v2)
    | "*" -> Some(v1 * v2)
    | "/" -> if v2 = 0 then None else Some(v1 / v2)

let opEvalOpt1 op v:int option =   //<--- Allan's EXERCISE 1
    match op with                                //to be used here and there for exercise 1
    | "ABS" -> Some(abs v)
                
let rec optionEval2 e : int option =
    match e with
    | CstI i -> Some i
    | Prim(op, e1, e2) ->
        match optionEval2 e1 with
        | None -> None
        | Some v1 ->
            match optionEval2 e2 with
            // | None -> None                   //<-- Allan's EXERCISE 1 
            | None -> opEvalOpt1 op v1          // Very similar to optionEval1 just uses the function opEvalOpt1 
            | Some v2 -> opEvalOpt op v1 v2

let optionFlatMap (f : 'a -> 'b option) (x : 'a option) : 'b option =
    match x with
    | None   -> None
    | Some v -> f v;;

type OptionBuilder() =
    member this.Bind(x, f) =
        match x with
        | None   -> None
        | Some v -> f v
    member this.Return x = Some x
    member this.ReturnFrom x = x
 
let optionM = OptionBuilder();;

let rec optionEval3 e : int option =
    match e with
    | CstI i -> optionM { return i }
    | Prim(op, e1, e2) ->
        optionM { let! v1 = optionEval3 e1
                  let! v2 = optionEval3 e2
                  return! opEvalOpt op v1 v2 }
    | Prim1(op, e1) ->              //<-- Allan's Exercise 1
        optionM { let! v1 = optionEval3 e1      //Similar to identM only
                  return! opEvalOpt1 op v1}         //usoing opEvalOpt1 instead

// ------------------------------------------------------------                

// Evaluator that returns a set of results, return type: int Set

let opEvalSet op v1 v2 : int Set =
    match op with
    | "+" -> Set [v1 + v2]
    | "*" -> Set [v1 * v2]
    | "/" -> if v2 = 0 then Set.empty else Set [v1 / v2]
    | "choose" -> Set [v1; v2]

let opEvalSet1 op v: int Set  = //<-- Allan's EXERCISE 1 
    match op with                           // we have a set of one element     
    | "ABS" -> Set [abs v]         //with an absolute value

let rec setEval1 e : int Set =
    match e with
    | CstI i -> Set [i]
    | Prim(op, e1, e2) ->
        let s1 = setEval1 e1
        let yss = Set.map (fun v1 ->
                        let s2 = setEval1 e2
                        let xss = Set.map (fun v2 -> opEvalSet op v1 v2) s2
                        Set.unionMany xss)
                        s1
        Set.unionMany yss
    | Prim1(op, e) ->           //<-- Allan's EXERCISE 1
        let s = setEval1 e           
        let zss = Set.map (fun v -> opEvalSet1 op v) s 
        Set.unionMany zss                 //----------
    

let setFlatMap (f : 'a -> 'b Set) (x : 'a Set) : 'b Set =
    Set.unionMany (Set.map f x);;

type SetBuilder() =
    member this.Bind(x, f) =
        Set.unionMany (Set.map f x)
    member this.Return x = Set [x]
    member this.ReturnFrom x = x
 
let setM = SetBuilder();;

let rec setEval3 e : int Set =
    match e with
    | CstI i -> setM { return i }
    | Prim(op, e1, e2) ->
        setM { let! v1 = setEval3 e1
               let! v2 = setEval3 e2
               return! opEvalSet op v1 v2 }
    | Prim1(op, e) ->           //<-- Allan's EXERCISE 1
        setM { let! v = setEval3 e
               return! opEvalSet1 op v}         //-------------------


// ------------------------------------------------------------

// Evaluator that records sequence of operators used,
// return type: int trace

let random = new System.Random()

type 'a trace = string list * 'a

let opEvalTrace op v1 v2 : int trace =
    match op with
    | "+" -> (["+"], v1 + v2)
    | "*" -> (["*"], v1 * v2)
    | "/" -> (["/"], v1 / v2)
    | "choose" -> (["choose"], if random.NextDouble() > 0.5 then v1 else v2)

let opEvalTrace1 op v : int trace =
    match op with
    | "ABS" -> (["ABS"], abs v)

let rec traceEval1 e : int trace =
    match e with
    | CstI i -> ([], i)
    | Prim(op, e1, e2) ->
        let (trace1, v1) = traceEval1 e1
        let (trace2, v2) = traceEval1 e2
        let (trace3, res) = opEvalTrace op v1 v2
        (trace1 @ trace2 @ trace3, res)
    | Prim1(op, e1) ->                      // <--- Allan's EXERCISE 1
        let (trace1, v) = traceEval1 e  // USes the same structure as above
        let (trace2, res) = opEvalTrace1 op v // not sure I completely understand it!
        (trace1 @ trace2, res)

let traceFlatMap (f : 'a -> 'b trace) (x : 'a trace) : 'b trace =
    let (trace1, v) = x
    let (trace2, res) = f v
    (trace1 @ trace2, res)

type TraceBuilder() =
    member this.Bind(x, f) =
        let (trace1, v) = x
        let (trace2, res) = f v
        (trace1 @ trace2, res)
    member this.Return x = ([], x)
    member this.ReturnFrom x = x
 
let traceM = TraceBuilder();;

let rec traceEval3 e : int trace =
    match e with
    | CstI i -> traceM { return i }
    | Prim(op, e1, e2) ->
        traceM { let! v1 = traceEval3 e1
                 let! v2 = traceEval3 e2
                 return! opEvalTrace op v1 v2 }
    | Prim1(op, e) ->       //<-- Allan's exercise 1
        traceM { let! v = traceEval3 e
                 return! opEvalTrace1 op v }//----------------

// ------------------------------------------------------------

let expr1 = Prim("+", CstI(7), Prim("*", CstI(9), CstI(10)))
let expr2 = Prim("+", CstI(7), Prim("/", CstI(9), CstI(0)))
let expr3 = Prim("+", CstI(7), Prim("choose", CstI(9), CstI(10)))
let expr4 = Prim("choose", CstI(7), Prim("choose", CstI(9), CstI(13)))
let expr5 = Prim("*", expr4, Prim("choose", CstI(2), CstI(3)))

