
//From Exam 2022 1.3
let rec fold f e tc = 
    match tc with 
        |TrashItems (ts, tc) -> fold f (f e ts) tc 
        |Empty -> e

//From Exam 2014 1.7
let fold f start ol = 
    let rec foldR f' start' = function
        |x::xs -> foldR f' (f' start' x) xs 
        |[] -> start'
    foldR f start (toList ol)

// Solution fro 2014 1.7 using library List function (more clean)
// This makes good sense since we have a function toList, that transform our ol parameter
// into a list. 
// In the exam from 2022 such a function does not exist, hence the fold is written "manually"
let fold fn b ol = List.fold fn b (toList ol)