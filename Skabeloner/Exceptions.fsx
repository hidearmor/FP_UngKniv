//  °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
//  °°°°°°°°°°°°°   EXCEPTIONS   °°°°°°°°°°°°°°°
//  °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°

// Exceptions are easily using the exception keyword (skal starte med stort): 
    exception TestError

// Exceptions can also carry data (usually a string):
    exception TestErrorString of string

// To use exceptions you have to raise them, here using both of the above exceptions: 
    let f a b = if b=0 then raise TestError else a / b

    let f' a b = if b=0 then raise (TestErrorString "Division by zero") else a / b

// Example from Assignment 12:
    type Heap<'a when 'a: equality> =
    | EmptyHP
    | HP of 'a * Heap<'a> * Heap<'a>

    exception HeapError of string

    let rec map f h = 
    match h with
    | EmptyHP -> raise (HeapError "Error")
    | HP(x, l, r) -> HP(f x, map f l, map f r)


// Another example from book (H.R 53)
    exception Solve;;
    
    let solve(a,b,c) =
     if b*b-4.0*a*c < 0.0 || a = 0.0 then raise Solve
     else ((-b + sqrt(b*b-4.0*a*c))/(2.0*a),
           (-b - sqrt(b*b-4.0*a*c))/(2.0*a));;

// Furthermore the solve function is used in a try .. with statement.
// Here the solve function is excecuted in the try-block, and if an error occurs
// the error/exception is matched on the with-block, in order to see execute further code (i.e. print an error statement):
    let solveText eq =
        try
            string(solve eq)
        with
        | Solve -> "No solutions";;

    solveText(1.0, 1.0, 1.2);; // prints Error-statement
    solveText(1.0, 9.0, 1.2);; // prints (result(s))



//  °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
//  °°°°°°°°°°°°°   FAILWITH    °°°°°°°°°°°°°°°°
//  °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°

// Another way of indicating an error is to use the built-in function failwith: string -> 'a
// It's polymorphic so it can be applied in any context. 
// See example from H.R page 54:
    let solve1(a,b,c) =
        if b*b-4.0*a*c < 0.0 || a = 0.0
        then failwith "discriminant is negative or a=0.0"
        else ((-b + sqrt(b*b-4.0*a*c))/(2.0*a),
                (-b - sqrt(b*b-4.0*a*c))/(2.0*a));;