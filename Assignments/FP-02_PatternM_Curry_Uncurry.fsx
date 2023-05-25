module a2

// 2.1 time difference:
let timediff (h0, m0) (h1, m1) = 
    (h1*60 + m1)-(h0*60 + m0) //virker ikke hvis den går over midnat, lol

// 2.2 function minutes
let minutes = timediff(0,0) // timediff ved den skal have 2 arguments, så dem behøver vi ikek skrive
    // this gcould just as well have been let minutes (h,m) = h*60+m

// 2.3 / HR 2.2  - we are using some type inference
let rec pow = function
    |(s, 0) -> ""
    |(s, n) -> s + pow(s, n-1)

// 2.4 / HR 2.8
let rec bin = function          
    | (_, 0) -> 1
    | (a, b) when a = b -> 1
    | (a, b) ->  bin(a-1, b-1) + bin(a-1, b)


// let rec bin (a,b) = 
//     match (a,b) with
//     | (_, 0) -> 1
//     | (a, b) when a = b -> 1
//     | (a, b) ->  bin(a-1, b-1) + bin(a-1, b)

// let rec bin = function 
//     |(_,0) -> 1
//     |(a,b) -> if a=b then 1 else bin(a-1, b-1) + bin(a-1, b)

// let rec fact = function 
//     | 1 -> 1
//     | n -> n * fact(n-1)let bin (n, k) = 
//         (fact(n-1)/(fact(k-1)*(fact((n-1)-(k-1))))) + (fact(n-1)/(fact(k)*fact(n-1-k)))

// 2.5 / HR 2.9

(*
let rec f = function
| (0,y) -> y
| (x,y) -> f(x-1, x*y);;

1. Type of f: int * int -> int
2. the evaluation of f terminates for the y argument
3. Evaluation for f(2,3).
    f(2,3)
        f(2-1, 2*3)
        f(1, 6)
            f(1-1, 1*6)
            f(0, 6)
            6
        6
    6

    f(3,3)
        f(3-1, 3*3)
        f(2, 9)
            f(2-1, 2*9)
            f(1, 18)
                f(0, 18)
                18
            18
        18
    18

4. The mathematical meaning of f(x,y): x! * y
*)

// 2.6 / HR 2.10

(*
    let test(c,e) = if c then e else 0;;

    1. type of test = boolean * int -> int
    2. test(false, fact(-1)) evaluates to an error
    3. Where "if false then fact -1 else 0" evaluates to 0
        The reason is that in 2. the arguments we put in get evaluated before the function is computed
        And you can't do fact(-1)
        But the 3. option tries to compute fact -1 only if false. So of true, it will never try fact -1

*)

// 2.7 / HR 2.13 Curry and Uncurry
let curry f x y = f (x,y)
// what we actually do is we take a curried form f x y and uncurry it into tupled form
// this means that we tell F# that istead of 
// "the funcition curry is: f, a function that returns a function x that returns a function y, that reaturns a value"
// now we say "curry is a function that takes the function f and puts the tuple "x,y) into it


let uncurry g (x,y) = g x y
// here we kinda do the opposite

