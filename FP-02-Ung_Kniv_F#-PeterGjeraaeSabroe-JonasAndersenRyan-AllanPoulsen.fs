module a2

// 2.1 time difference:
let timediff (h0, m0) (h1, m1) = 
    (h1*60 + m1)-(h0*60 + m0) //virker ikke hvis den går over midnat, lol

// 2.2 function minutes
let minutes = timediff(0,0) // timediff ved den skal have 2 arguments, så dem behøver vi ikek skrive
    // this gcould just as well have been let minutes (h,m) = h*60+m

// 2.3 / HR 2.2  - we are using some type inference
let rec pow =
    | ((text), 0) -> ""
    | ((text), n) -> text + dupn(text, n-1)

// 2.4 / HR 2.8
let rec bin (a,b) = failwith "not implemented"

// 2.5 / HR 2.9

// 2.6 / HR 2.10

// 2.7 / HR 2.13 Curry and Uncurry
let curry f x y = failwith "not implemented"

let uncurry g (x,y) = failwith "not implemented"

