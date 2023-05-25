// Algebraic Data Types

// simple type with an unsoecified, an int, a record and a tuple

type Person = {name: string ; age: int ; weight: int}
type ADT_1 = 
    | Color
    | Number of int
    | Record of Person
    | Tuple of int * int

// type with eexpressions

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

// look in FP06 for more

// complex typex with records 

module ComplexNumber =
    type ComplexNumber = 
        { RealNo: float; ImaginaryNo: float } //<-- record of two fields, similar logic as in exercise 3.2, where we made Money records
        static member ( .+ ) (a: ComplexNumber) (b: ComplexNumber) : ComplexNumber =
            { RealNo = a.RealNo + b.RealNo; ImaginaryNo = a.ImaginaryNo + b.ImaginaryNo }

        static member ( .- ) (a: ComplexNumber) (b: ComplexNumber) : ComplexNumber =
            { RealNo = a.RealNo - b.RealNo; ImaginaryNo = a.ImaginaryNo - b.ImaginaryNo }

        static member ( .* ) (a: ComplexNumber) (b: ComplexNumber) : ComplexNumber =
            { RealNo = a.RealNo * b.RealNo - a.ImaginaryNo * b.ImaginaryNo; 
                ImaginaryNo = a.RealNo * b.ImaginaryNo + a.ImaginaryNo * b.RealNo }

        static member ( ./ ) (a: ComplexNumber) (b: ComplexNumber) : ComplexNumber =
            { RealNo = (a.RealNo *(a.ImaginaryNo/(a.ImaginaryNo * a.ImaginaryNo + b.ImaginaryNo*b.ImaginaryNo))
                        - (b.RealNo *(-b.ImaginaryNo/(a.ImaginaryNo*a.ImaginaryNo + b.ImaginaryNo * b.ImaginaryNo))));
            ImaginaryNo = (b.RealNo *(a.ImaginaryNo/(a.ImaginaryNo * a.ImaginaryNo + b.ImaginaryNo*b.ImaginaryNo))
                        - (a.RealNo *(-b.ImaginaryNo/(a.ImaginaryNo*a.ImaginaryNo + b.ImaginaryNo * b.ImaginaryNo)))) }