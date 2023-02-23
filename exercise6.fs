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

let fexprToString expr = failwith "not implemented"

// 6.2 (HR 6.8)
(*
type Stack = S of float list
*)
type Instruction = | ADD | SUB | MULT | DIV | SIN
                   | COS | LOG | EXP | PUSH of float


//let intpInstr ...

//let intpProg ...

//let trans ...

// 6.3 (HR 7.2)
//type ComplexNumber ...
