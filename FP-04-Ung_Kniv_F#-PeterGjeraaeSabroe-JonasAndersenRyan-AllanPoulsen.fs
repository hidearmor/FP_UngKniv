module a2
// Exercise 4.1 - explode - string to char list
let explode (s:string) = List.ofArray(s.ToCharArray())


let rec explode2 (s:string) = 
    match s with //does not work with "function" keyword, but does mit "match s with"
    | ""  -> []
    | _ -> s.Chars(0)::explode2(s.Remove(0,1))  //<-- s.Char(0) extracts first character, s.Remove(0,1) returns remaining string

//-----
 

// Exercise 4.2 - implode - char list to string
//Returns string build from list of char in same order as list, using foldback
let implode (list:list<char>) = 
    List.foldBack 
        (fun (c:char) s -> string c + s)
        list
        ""
//Returns string build from list of char in reverse order as list, using fold
let implodeRev (list:list<char>) =
    List.fold 
        (fun s (c:char) -> string c + s)
        "" 
        list


//-----
 

// Exercise 4.3 - toUpper
//Here we explode s, using the above function. This list is fed into fun c -> System.Char.Upper which maps  it to a new list, that is then imploded.
let toUpper (s:string) = implode(List.map (fun c -> System.Char.ToUpper c) (explode s))

// Alternative and more elaborate version:
// let toUpper (s:string) = 
//     let d = explode s
//     let g = List.map System.Char.ToUpper d
//     implode g



// let toUpper1 (s:string) = failwith "not implemented"
//Receive s forward function it into explode, then List.Map etc . and subsequently implode it.
let toUpper1 (s:string) = (explode >> List.map System.Char.ToUpper >> implode) s

//Pipe s into explode an "backwards" function forward through from explode to implode
let toUpper2 (s:string) = s |> (implode << List.map System.Char.ToUpper << explode)

// Alternative version
// let toUpper2 (s:string) = s |> (implode << (explode >> List.map System.Char.ToUpper))

// Alternative version 2
// let toUpper2 (s:string) =
//     explode s |> (implode << (List.map (fun c -> System.Char.ToUpper c)))

//-----
 

// Exercise 4.4 - palindrome - treating empty strings as palindromes too.

// Returns a boolean that is a comparison of the reesults of toUpper1 of s and the "backward" version of s
// that comes through implodeRev
let palindrome (s:string) = 
    let forward = toUpper1 s
    let backward = s |> (implodeRev << (explode >> List.map System.Char.ToUpper))
    if forward = backward 
    then true
    else false

// Alternative verrsion 1 (case sensitive)
// let palindrome (s:string) = (s = (explode s |> implodeRev))


// Alternative version 2 (case sensitive)
// let palindrome (s:string) =  s = (s |> (implode << List.rev << explode))

//-----
 

// Exercise 4.5 - ack

// recursive ackermann function as per assignment with the patterm guards (when) that n and m must be higher than zero
let rec ack t = 
    match t with
    |(0,n) when n>0 -> n+1
    |(m,0) when m>0 -> ack((m-1),1)
    |(m,n) when m>0 && n>0 -> ack(m-1,ack(m, n-1)) 
    | _ -> failwith "Fail, man!"

// Alternative version
//     match m, n with
//     | 0, _ -> n + 1
//     | _, 0 -> ack((m-1), 1)
//     | _, _ -> ack((m-1), ack(m, (n-1)))
  
// ack(3, 11) = 16381

//-----
 
// Exercise 4.6 - time

// Function as per assignment:
let time f =
    let start = System.DateTime.Now in
    let res = f () in
    let finish = System.DateTime.Now in
    (res, finish - start);
(*
    > time (fun () -> ack(3, 11));;
    Ovenstående expression giver 00:00:01.0542130
*)  

//Allows us to use "time" function above as requested in assignment
let timeArg1 f a = time (fun g -> f a)

//-----
 
// Exercise 4.7 - HR 5.4 - downTo2 f n e

//The factorial function from the assignment.
let rec fact4 = function
    | 0 -> 1
    | n when n > 0 -> n * fact4(n-1)
    | _ -> failwith "only works on positives, man!"

let downto1 g n e =
    match n with
    | n when n <=0 -> e
    | _ -> List.foldBack g [1..n] e

let buildList g n = downto1 (fun x xs -> g x :: xs) n []
//Above does the following
// 1. "buildList fact 5;;" applies a function (eg. fact) and a number (eg. 5) 
// 2. (fun x xs -> g x :: xs)  takes the integer x from xs and applies the function g 
    //(ie. eg. fact) to the integer
    // and then adds it to the empty list. this continues until xs is empty.
// 3. the downto1 function "unfolds" the list
// 4. as such the "buildList g n" takes a function and 5 value, applies the 
    //downto1 function to the function described in 2) above.

//---------------------------    

// ALTERNATIVE VERSIONS FOR STUDYING FOR EXAM

// let buildList g n = 
//     let l = [1..n]
//     let rec action xs =
//         match xs with
//         |[] -> []
//         |x::xs -> (g x)::action xs
//     if n < 0 then failwith "insert positive integer, bruh"
//     else action l

// ny version som er en kombi af det andet
// let buildlistYes g n =
//     let f x xs = (g x)::xs
//     downto1 f n []

// yet another version
// let buildList2 g n = List.rev (downto1 (fun x xs -> (g x)::xs) n [])
    // return a reverse version of:
        // the list returned from downto1
            // Taking arguments:
                // 1: a function taking two arguments (head & tail)
                    //applying the function g on the head
                // 2: n, the integer
                // 3: an empty list -> where we start
    // returns a list because we have put in the empty list in the end
    // can use downto1 because (fun x xs -> (g x) :: xs) takes two arguments