// namespace FP06Modules
module ComplexNumber
type ComplexNumber = 
    { RN: float; IN: float } //<-- record of two fields, similar logic as in exercise 3.2, where we made Money records
    static member (.+) a b =
        { RN = a.RN + b.RN; IN = a.IN + b.IN }

    static member (.-) a b =
        { RN = a.RN - b.RN; IN = a.IN - b.IN }

    static member (.*) a b =
        { RN = a.RN * b.RN - a.IN * b.IN; 
            IN = a.RN * b.IN + a.IN * b.RN }

    static member (./) a b =
        { RN = (a.RN *(a.IN/(a.IN * a.IN + b.IN*b.IN))
                    - (b.RN *(-b.IN/(a.IN*a.IN + b.IN * b.IN))));
        IN = (b.RN *(a.IN/(a.IN * a.IN + b.IN*b.IN))
                    - (a.RN *(-b.IN/(a.IN*a.IN + b.IN * b.IN)))) }

(* Calculations are the exact same as in exercise 3.3, only with the note that 
in 3.3 we use a, b, c, d and in below we use:
a.RealNo(RN), b.RealNo(RN), a.ImaginarNo(IN), b. ImaginaryNo(IN) instead. 
This is in order to use the record (type defined tuples). Similar  to what we did in exercise 3.2
*)
