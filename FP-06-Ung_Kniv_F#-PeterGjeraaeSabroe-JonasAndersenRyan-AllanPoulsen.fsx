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

// let stackExmpl = S[4.5;3.5]

type Instruction = | ADD | SUB | MULT | DIV | SIN
                   | COS | LOG | EXP | PUSH of float

// list operations & cons operator x::xs
// type be like: intpInstr: Stack -> Instruction -> Stack 

// open type System.Math
let intpInstr (S stack) (ins: Instruction) = 
    match ins with
    // stack -> x:s:xs  How we do it: The type of parameter "stack" is S (whis is list of float)
    // then we do our operations and pipe it into S, be cause S takes a list of float
    // and that's exactly what we feed it
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

//intpInstr S[4.0; 4.5; 6.0] ADD;;  

// let intpProg  = 0
// Take list of instructions, returns float
let intpProg4 (insElem: Instruction list) : float = 
    let rec stackRec (stack: Stack) (prog: Instruction list) : float =  //<--recursively working our way through the stack and instruction list
        match prog with                                              //<--match on the instruction elements (ie. the isntruction list) one by one
        | [] -> match stack with                                        //<--if the instruction list is empty...
                | S [] -> failwith "Stakken er tom, maayn!"    //<--...then move on to the stack. if the stack is empty, the provide a message
                | S (x::_) -> x                                         //<-- if the stack is not empty, then return the first element in the stack
        | instr::rest -> let myStack = intpInstr stack instr    //<-- if the lidt of instructions are not empty, then define a stack (myStack) using the 
                         stackRec myStack rest                              //intpInstr function declared above with the stack and instruction found as arguments
    stackRec (S []) insElem                                               // Then run the stackRec inner function (loop) with the remaining stack (myStack) and the rest of the 
    //instruction list as arguments. Finally: Call the stackRec with an empty stach S[] to execute the instructions and get to the top element of the stack

(* SHORTER VERSION WITH DIFFERENT VARIABLE NAMES for pedagigical reasons
let intProg list  =
    let rec execute stack prog = 
        match prog with
        | [] -> match stack with 
                | S [] -> failwith "no input given at all"
                | S (x::_) -> x
        | instr::xs ->
            let newStack = intpInstr stack instr
            execute newStack xs
    
    execute (S []) list
*)

intpProg4 [PUSH 4.5; PUSH 3.0; ADD; PUSH 2.0; MULT; SIN];; 
//<-- This works!

// let trans = 0
//Uses type declaration from Fexpr from 6.1/6.2 above
let rec trans (fe: Fexpr * float) : Instruction List =
    match fe with
    | (Const c,_) -> [PUSH c] //<--lists and list appending as per slide 16/17 from week 3 (Records, tagged values and lists)    
    | (X, x) -> [PUSH x]
    | Add(a, b), x -> (trans (a, x)) @ (trans (b, x)) @ [ADD] //<-- Concatenation as per slide 16/17 from week 3 (Records, tagged values and lists)    
    | Sub(a, b), x -> (trans (a, x)) @ (trans (b, x)) @ [SUB] 
    | Mul(a, b), x -> (trans (a, x)) @ (trans (b, x)) @ [MULT] 
    | Div(a, b), x -> (trans (a, x)) @ (trans (b, x)) @ [DIV] 
    | Sin a, x -> (trans (a, x)) @ [SIN] 
    | Cos a, x -> (trans (a, x)) @ [COS] 
    | Log a, x -> (trans (a, x)) @ [LOG] 
    | Exp a, x -> (trans (a, x)) @ [SIN] 






// 6.3 (HR 7.2)
// Allan's 1st attempt below ---------------------------------
// Signature file
//Constructed as per table 7.1 (Signature file with type augmentation) in the textbook

module ComplexNumber
type ComplexNumber
    static member (.+) : ComplexNumber * ComplexNumber -> ComplexNumber
    static member (.-) : ComplexNumber * ComplexNumber -> ComplexNumber
    static member (.*) : ComplexNumber * ComplexNumber -> ComplexNumber
    static member (./) : ComplexNumber * ComplexNumber -> ComplexNumber
    val make : float * float -> ComplexNumber


// Implementation file
module ComplexNumber
type ComplexNumber = { RealNo: float; ImaginaryNo: float } //<-- record of two fields, similar logic as in exercise 3.2, where we made Money records
    static member (.+) (a: ComplexNumber) (b: ComplexNumber) : ComplexNumber =
        { RealNo = a.RealNo + b.RealNo; ImaginaryNo = a.ImaginaryNo + b.ImaginaryNo }

    static member (.-) (a: ComplexNumber) (b: ComplexNumber) : ComplexNumber =
        { RealNo = a.RealNo - b.RealNo; ImaginaryNo = a.ImaginaryNo - b.ImaginaryNo }

    static member (.*) (a: ComplexNumber) (b: ComplexNumber) : ComplexNumber =
        { RealNo = a.RealNo * b.RealNo - a.ImaginaryNo * b.ImaginaryNo; 
            ImaginaryNo = a.RealNo * b.ImaginaryNo + a.ImaginaryNo * b.RealNo }

    static member (./) (a: ComplexNumber) (b: ComplexNumber) : ComplexNumber =
        { RealNo = (a.RealNo *(a.ImaginaryNo/(a.ImaginaryNo * a.ImaginaryNo + b.ImaginaryNo*b.ImaginaryNo))
                      - (b.RealNo *(-b.ImaginaryNo/(a.ImaginaryNo*a.ImaginaryNo + b.ImaginaryNo * b.ImaginaryNo))));
          ImaginaryNo = (b.RealNo *(a.ImaginaryNo/(a.ImaginaryNo * a.ImaginaryNo + b.ImaginaryNo*b.ImaginaryNo))
                      - (a.RealNo *(-b.ImaginaryNo/(a.ImaginaryNo*a.ImaginaryNo + b.ImaginaryNo * b.ImaginaryNo)))) }
//Calculations are the exact same as in exercise 3.3, only with the note that in 3.3 we use a, b, c, d and in below
//we use a.RealNo, b.RealNo, a.ImaginarNo, b. ImaginaryNo instead. This is in order to use the record. Similar  to what we did in exercise 3.2

//FROM 3.3 BELOW --------------------------------
//      1. Declare infix for addition and multiplication

//let ( .+) (a:float,b:float) (c:float,d:float) = (a + c, b + d) //calculate using the defintions in the assignment

//let ( .*) (a:float,b:float) (c:float,d:float) = (a*c-b*d, b*c + a*d) //calculate using the defintions in the assignment

//      2. Declare infix for subtraction and division
// let ( .-) (a:float,b:float) (c:float,d:float) = (a - c, b - d) //calculate using the defintions in the assignment

// let ( ./) (a,b) (c,d) =  ((a*(c/(c*c + d*d)) - (b*(-d/(c*c + d*d))), (b*(c/(c*c + d*d)))+(a*(-d/(c*c + d*d))))) //calculate using the defintions in the assignment

//FROM 3.3 ABOVE-------------------------------------
