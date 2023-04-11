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

// Exercise 2. Extend the expression language and the monadic evaluators with a three-argument function such as 
// +(e1, e2, e3) that is basically two applications of "+", as in, +(+(e1,e2),e3).  
// Do this by adding a new constructor Prim3 of string * expr * expr * expr to the expr type.

// You may alternatively add a more general facility for functions with 
// n>=1 arguments, such as SUM(e1, ..., en), adding a suitable constructor to the expr type.

// Implement evaluation of such three-argument (or multi-argument) constructs in the monadic evaluators.
//-----------------------------------------------------------------------------------------------

// Exercise 3. Create a new family of evaluation functions optionTraceEval.  
// These evaluators should combine the effect of the original optional evaluator (optionEval) and the 
// original tracing evaluator (traceEval).

// This can be done in several ways, for instance corresponding to 
// (A) return type int trace option, 
//     for an evaluator that returns no trace if a computation fails; or 
// (B) the result type int option trace, 
//     for an evaluator that returns a partial trace up until some computation (eg division by zero) fails.

// 3.1: Make both a standard explicit version of (A) and a monadic version.  
// You need to create a new monad OptionTraceABuilder, among other things.

// 3.2: Make both a standard explicit version of (B) and a monadic version.  
// You need to create a new monad OptionTraceBBuilder, among other things.
// ------------------------------------------------------------------------------------------------------

type expr =
    | CstI of int
    | Prim of string * expr * expr
    | Prim1 of string * expr                //EXERCISE 1
    | Prim3 of string * expr * expr * expr // EXERCISE 2

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
        let v = eval1 e1               //  
        match op1 with                       // 
        | "ABS" -> if v<0 then -v else v
        // | "ABS" -> abs v                     // abs is a built-in Math Operator (https://www.dotnetperls.com/math-fs)
    | Prim3(op3, e1, e2, e3) -> //<-- For exercise 2
        let v1 = eval1 e1
        let v2 = eval1 e2
        let v3 = eval1 e3
        match op3 with
        | "+" -> v1 + v2 + v3                               //---------------



let opEval op v1 v2 : int =
    match op with
    | "+" -> v1 + v2
    | "*" -> v1 * v2
    | "/" -> v1 / v2
    //| "ABS" -> abs v1 ... for exercise 1 we could probably have used this and called OpEval with "opEval op v 0"

let EvalOpt1 op v:int =   //<--- EXERCISE 1
    match op with                        //
    | "ABS" -> if v<0 then -v else v

let EvalOpt3 op v1 v2 v3  =   //<--- EXERCISE 2
    match op with                        
    | "+" -> v1 + v2 + v3

let rec eval2 e : int =
    match e with
    | CstI i -> i
    | Prim(op, e1, e2) ->
        let v1 = eval2 e1
        let v2 = eval2 e2
        opEval op v1 v2
    | Prim1(op1, e1) -> //<-- EXERCISE 1
        let v = eval2 e1
        EvalOpt1 op1 v
    | Prim3(op3, e1, e2, e3) -> //<-- EXERCISE 2 
        let v1 = eval2 e1
        let v2 = eval2 e2
        let v3 = eval2 e3
        EvalOpt3 op3 v1 v2 v3         // ---------------

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
    | Prim1(op, e1) ->             //<-- EXERCISE 1
        identM { let! v = eval3 e1            // 
                 return! EvalOpt1 op v}      
    | Prim3(op, e1, e2, e3) -> //<-- exercise 2
        identM { let! v1 = eval3 e1
                 let! v2 = eval3 e2
                 let! v3 = eval3 e3
                 return! EvalOpt3 op v2 v2 v3}    

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
            | None -> None                    
            | Some v2 ->
                match op with
                | "+" -> Some(v1 + v2)
                | "*" -> Some(v1 * v2)
                | "/" -> if v2 = 0 then None else Some(v1 / v2)
    | Prim1(op, e1) ->      //<-- EXERCISE 1
        match optionEval1 e1 with
        | None -> None
        | Some v ->
            match op with 
            | "ABS" -> Some(abs v)
    | Prim3(op, e1, e2, e3) -> //<--- Exercise 2
        match optionEval1 e1 with
        | None -> None
        | Some v1 ->
            match optionEval1 e2 with
            | None -> None
            | Some v2 ->
                match optionEval1 e3 with
                | None -> None
                | Some v3 ->
                    match op with
                    | "+" -> Some(v1 + v2 + v3)


let opEvalOpt op v1 v2 : int option =
    match op with
    | "+" -> Some(v1 + v2)
    | "*" -> Some(v1 * v2)
    | "/" -> if v2 = 0 then None else Some(v1 / v2)

let opEvalOpt1 op v:int option =   //<--- Allan's EXERCISE 1
    match op with                                //to be used here and there for exercise 1
    | "ABS" -> Some(abs v)

let opEvalOpt3 op v1 v2 v3:int option =   //<--- Allan's EXERCISE 1
    match op with                                //to be used here and there for exercise 1
    | "+" -> Some(v1 + v2 + v3)
                
let rec optionEval2 e : int option =
    match e with
    | CstI i -> Some i
    | Prim(op, e1, e2) ->
        match optionEval2 e1 with
        | None -> None
        | Some v1 ->
            match optionEval2 e2 with
            | None -> None
            | Some v2 -> opEvalOpt op v1 v2
        | Prim1(op, e1) ->                      // Exercise 1
            match optionEval2 e1 with
            | None -> None
            | Some v -> opEvalOpt1 op v
    | Prim3(op, e1, e2, e3) -> //<-- EXERCISE 2
        match optionEval2 e1 with
        | None -> None
        | Some v1 ->
            match optionEval2 e2 with
            | None -> None                  //<-- Eg here (and in all similar sentences), we could add a structure to take into account the eg. 
            | Some v2 ->                // if v2 is null then add v1 and v3. However, I read the exercise as None should return None
                match optionEval2 e3 with
                | None -> None
                | Some v3 -> 
                    opEvalOpt3 op v1 v2 v3 //-------------------

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
    | Prim1(op, e1) ->              //<-- Exercise 1
        optionM { let! v1 = optionEval3 e1      //Similar to identM only
                  return! opEvalOpt1 op v1}         //usoing opEvalOpt1 instead
    | Prim3(op, e1, e2, e3) -> //<-- AEXERCISE 2
        optionM { let! v1 = optionEval3 e1
                  let! v2 = optionEval3 e2
                  let! v3 = optionEval3 e3
                  return! opEvalOpt3 op v1 v2 v3} //---------------------

// ------------------------------------------------------------                

// Evaluator that returns a set of results, return type: int Set

let opEvalSet op v1 v2 : int Set =
    match op with
    | "+" -> Set [v1 + v2]
    | "*" -> Set [v1 * v2]
    | "/" -> if v2 = 0 then Set.empty else Set [v1 / v2]
    | "choose" -> Set [v1; v2]

let opEvalSet1 op v: int Set  = //<-- EXERCISE 1 
    match op with                           // we have a set of one element     
    | "ABS" -> Set [abs v]         //with an absolute value

let opEvalSet3 op v1 v2 v3: int Set  = //<-- EXERCISE 2 
    match op with                            
    | "+" -> Set [v1 + v2 + v3]     //----------------  

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
    | Prim1(op, e) ->           //<-- EXERCISE 1
        let s = setEval1 e           
        let zss = Set.map (fun v -> opEvalSet1 op v) s 
        Set.unionMany zss                 //----------
    | Prim3(op, e1, e2, e3) ->  // <-- EXERCISE 2
        let s1 = setEval1 e1 
        let zss = Set.map (fun v1 -> 
                    let s2 = setEval1 e2
                    let yss = Set.map (fun v2 ->
                            let s3 = setEval1 e3
                            let xss = Set.map (fun v3 -> opEvalSet3 op v1 v2 v3) s3
                            Set.unionMany xss) s2
                    Set.unionMany yss) s1
        Set.unionMany zss //----------------------------------------------------



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
    | Prim3(op, e1, e2, e3) -> //<-- Allan's EXERCISE 2
        setM { let! v1 = setEval3 e1
               let! v2 = setEval3 e2
               let! v3 = setEval3 e3
               return! opEvalSet3 op v1 v2 v3} //---------------------


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

let opEvalTrace3 op v1 v2 v3 : int trace =
    match op with
    | "+" -> (["ABS"], v1 + v2 + v3)

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
    | Prim3(op, e1, e2, e3) -> //<-- Allan's EXERCISE 2
        let (trace1, v1) = traceEval1 e1
        let (trace2, v2) = traceEval1 e2
        let (trace3, v3) = traceEval1 e3
        let (trace4, res) = opEvalTrace3 op v1 v2 v3
        (trace1 @ trace2 @ trace3 @ trace4, res)  //--------------------------------

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
    | Prim3(op, e1, e2, e3) -> //Allan's eXERCISE 2
        traceM { let! v1 = traceEval3 e1
                 let! v2 = traceEval3 e2
                 let! v3 = traceEval3 e3
                 return! opEvalTrace3 op v1 v2 v3} //--------------------------

// ------------------------------------------------------------

let expr1 = Prim("+", CstI(7), Prim("*", CstI(9), CstI(10)))
let expr2 = Prim("+", CstI(7), Prim("/", CstI(9), CstI(0)))
let expr3 = Prim("+", CstI(7), Prim("choose", CstI(9), CstI(10)))
let expr4 = Prim("choose", CstI(7), Prim("choose", CstI(9), CstI(13)))
let expr5 = Prim("*", expr4, Prim("choose", CstI(2), CstI(3)))


//-------------------------------------
// Exercise 3. Create a new family of evaluation functions optionTraceEval.  
// These evaluators should combine the effect of the original optional evaluator (optionEval) and the 
// original tracing evaluator (traceEval).

// This can be done in several ways, for instance corresponding to 
// (A) return type int trace option, 
//     for an evaluator that returns no trace if a computation fails; or 
// (B) the result type int option trace, 
//     for an evaluator that returns a partial trace up until some computation (eg division by zero) fails.

// 3.1: Make both a standard explicit version of (A) and a monadic version.  
// You need to create a new monad OptionTraceABuilder, among other things.

// 3.2: Make both a standard explicit version of (B) and a monadic version.  
// You need to create a new monad OptionTraceBBuilder, among other things.
// ------------------------------------------------------------------------------------------------------


let random1 = new System.Random()

type 'a traceoption = (string list * 'a) option

let opTraceEval op v1 v2 : int traceoption =
    match op with
    | "+" -> Some(["+"], v1 + v2)
    | "*" -> Some(["*"], v1 * v2)
    | "/" -> if v2 = 0 then None else Some(["/"], v1 / v2)
    | "choose" -> Some(["choose"], if random1.NextDouble() > 0.5 then v1 else v2)

let rec optionTraceEval1 e : int traceoption =
    match e with
    | CstI i -> Some([], i)
    | Prim(op, e1, e2) ->
        match optionTraceEval1 e1 with
        | None -> None
        | Some (trace1, v1) ->
            match optionTraceEval1 e2 with
            | None -> None
            | Some (trace2, v2) ->
                match opTraceEval op v1 v2 with
                | None -> None
                | Some(trace3, res) -> 
                    Some(trace1 @ trace2 @ trace3, res)

type optionTraceBuilder() =
    member this.Bind(x, f) =
        match x with
        | None -> None
        | Some (trace1, v1) ->
            match f v1 with
            | None -> None
            | Some (trace2, res) -> 
                Some(trace1 @ trace2, res)
    member this.Return x = Some([], x)
    member this.ReturnFrom x = (x)
 
let optionTraceM = optionTraceBuilder();;

let rec optionTraceEval3 e : int traceoption =
    match e with
    | CstI i -> optionTraceM { return i }
    | Prim(op, e1, e2) ->
        optionTraceM {  let! v1 = optionTraceEval3 e1
                        let! v2 = optionTraceEval3 e2
                        return! opTraceEval op v1 v2 }
        

// type OptionTraceABuilder() =
//     member this.Bind()