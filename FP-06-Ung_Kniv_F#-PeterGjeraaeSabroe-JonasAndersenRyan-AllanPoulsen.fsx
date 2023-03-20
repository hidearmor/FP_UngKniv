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
    // stack -> x:s:xs How we do it: The type of parameter "stack" is S (whis is list of float)
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

//Kan godt tage en instruktion og eksekvere instruktionen på de to første i listen 
let rec intpProg1 (insElem: Instruction list) (myList: float list) = 
    match insElem with
        | ins::insList ->
            match myList with 
                | x0 :: x1 :: xs -> intpInstr S[x0; x1], xs

//Virker på samme måde med Stack
let intpProg11 (insElem: Instruction list) (S stack)= 
    match insElem with 
        | ins::insList -> 
            match stack with
                | x0::x1::xs -> intpInstr S[x0; x1] ins, xs

// Jeg har en følelse af, at strukturen skal være nogenlunde sådan her. Det virker dog ikke, da
// den kune eksekverer instruktionen på de to første elementer, så rekursionen kører ikke 
// Jeg mangler også at få hevet det øverste element us af stacken, så der kun afleveres en float:
let rec intpProg2 (insElem: Instruction list) (myList: float list) = 
    match insElem with
        | ins::insList ->
            match myList with 
                | x0 :: x1 :: xs -> intpInstr S[x0; x1] ins :: intpProg2 insList xs
                | _ -> []
        | _ -> []

// let rec intpProg3 (insElem: Instruction list) : float = 
    // let rec stackLoop (stack: Stack) (insElem: Instruction List) : float =
    //     match insElem with
    //         | [] -> match stack with
    //                 | S [] -> failwith "Stakken er tom, maayn!"
    //                 | S (x::_) -> x
    //         | ins::insList -> let myStack = intpInstr stack ins 
    //                             stackLoop myStack insList
    //     stackLoop (S[]) insElem

         
        
// Take list of instructions, returns float
let intpProg4 (insElem: Instruction list) : float = 
    let rec stackRec (stack: Stack) (prog: Instruction list) : float =  //<--recursively working our way through the stack and instruction list
        match insElem with                                              //<--match on the instruction elements (ie. the isntruction list) one by one
        | [] -> match stack with                                        //<--if the instruction list is empty...
                | S [] -> failwith "Stakken er tom, maayn!"    //<--...then move on to the stack. if the stack is empty, the provide a message
                | S (x::_) -> x                                         //<-- if the stack is not empty, then return the first element in the stack
        | instr::rest -> let myStack = intpInstr stack instr    //<-- if the lidt of instructions are not empty, then define a stack (myStack) using the 
                         stackRec myStack rest                              //intpInstr function declared above with the stack and instruction found as arguments
    stackRec (S []) insElem                                               // Then run the stackRec inner function (loop) with the remaining stack (myStack) and the rest of the 
    //instruction list as arguments. Finally: Call the stackRec with an empty stach S[] to execute the instructions and get to the top element of the stack
    

intpProg4 [PUSH 4.5; PUSH 3.0; ADD; PUSH 2.0; MULT; SIN];;



  
// let trans = 0

// 6.3 (HR 7.2)
//type ComplexNumber