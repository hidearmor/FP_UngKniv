module test
// 6.1 (HR 6.2)
type Fexpr = 
    | Const of float
    | X
    | Add of Fexpr * Fexpr
    | Sub of Fexpr * Fexpr
    | Mul of Fexpr * Fexpr
    | Div of Fexpr * Fexpr
    | Sin of Fexpr
    | Cos of Fexpr
    | Log of Fexpr
    | Exp of Fexpr;;

let rec fexprToString expr =
    match expr with
    | Const a -> string a
    | X     -> "x"
    | Add(a,b) -> (fexprToString a) + " " + (fexprToString b) + " +"
    | Sub(a,b) -> (fexprToString a) + " " + (fexprToString b) + " -"
    | Mul(a,b) -> (fexprToString a) + " " + (fexprToString b) + " *"
    | Div(a,b) -> (fexprToString a) + " " + (fexprToString b) + " /"
    | Sin a    -> (fexprToString a) + " sin"
    | Cos a    -> (fexprToString a) + " cos"
    | Log a    -> (fexprToString a) + " log"
    | Exp a    -> (fexprToString a) + " exp"

// fexprToString (Add(Const(1.0),Const(3.0)));;

// 6.2 (HR 6.8)

// stack is a list of floats
type Stack = S of float list

let stackExmpl = S[4.5;3.5]

type Instruction = | ADD | SUB | MULT | DIV | SIN
                   | COS | LOG | EXP | PUSH of float

// list operations & cons operator x::xs
// type be like: intpInstr: Stack -> Instruction -> Stack 

// open type System.Math
let intpInstr (S stack) (ins: Instruction) = 
    match ins with
    // stack -> x:s:xs (hvordan gør vi der her?)
    | ADD -> 
        match stack with
        | x0::x1::xs -> (x0 + x1)::xs |> S
        | _ -> stack |> S
    | SUB -> 
        match stack with
        | x0::x1::xs -> (x0 - x1)::xs |> S
        | _ -> stack |> S
    | MULT -> 
        match stack with
        | x0::x1::xs -> (x0 * x1)::xs |> S
        | _ -> stack |> S
    | DIV -> 
        match stack with
        | x0::x1::xs -> (x0 / x1)::xs |> S
        | _ -> stack |> S
    | SIN -> 
        match stack with
        | x0::xs -> (System.Math.Sin(x0))::xs |> S
        | _ -> stack |> S
    | COS -> 
        match stack with
        | x0::xs -> (System.Math.Cos(x0))::xs |> S
        | _ -> stack |> S
    | LOG -> 
        match stack with
        | x0::xs -> (System.Math.Log(x0))::xs |> S
        | _ -> stack |> S
    | EXP -> 
        match stack with
        | x0::xs -> (System.Math.Exp(x0))::xs |> S
        | _ -> stack |> S
    | PUSH r -> r::stack |> S
    

// let intpProg  = 0

// let trans = 0

// 6.3 (HR 7.2)
//type ComplexNumber