module a2
// Exercise 4.1 - explode - string to char list
let explode (s:string) = List.ofArray(s.ToCharArray())


let rec explode2 (s:string) = 
    match s with //does not work with "function" keyword, but does mit "match s with"
    | ""  -> []
    | _ -> s.Chars(0)::explode2(s.Remove(0,1))  //<-- s.Char(0) extracts first character, s.Remove(0,1) returns remaining string


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
//Peters suggestion - ikke helt korrekt. Jeg ved ikke hvordan man skal lave den recursive.
let rec downto1 f (n, e) = 
    match n with
    | n when n <= 0 -> e
    | n when n > 0 -> downto1(n-1 e)

//Fundet på nettet:
let downto1 f n e =
    if n > 0 then
        let items = [1..n]
        List.foldBack f items e
    else
        e


//-----------------------------------------------------------------

// Jonas 1: succinct version I made from the info in "fundet på nettet" 
let downto1 f n e = function
    | n when n <= 0 -> e
    | _ ->  List.fold f [n..1] e

// Jonas 2: version with an inner recursive call
// note that I first declare the inner function THEN I do the if the else
// because doing pattern matching on downto1 and then just having the manualFoldingBaby
// in the second clause will not actually return anything (see out-commented version below)
let downto1 f n e = 
    // we define an inner recursive function that runs the functionality on a list
    let rec manualFoldingBaby l =  
        match l with
        | [] -> e
        | x::xs -> f x (manualFoldingBaby xs)
    if n <= 0 then e else manualFoldingBaby [1..n] 
    // we run some actual code: e if n <= 0 and 
    // else we feed the desired list to our function manualFoldingBaby

// Jonas 2: the version not returning anything (had GPT help me with why)
// let downto1 f n e = function 
//     | n when n <= 0 -> e
//     | _ -> 
//         let l = [1..n]
//         let rec dt2 = 
//             match l with
//             | x::xs -> f x (dt2 xs)

//-----------------------------------------------------------------

// Også taget fra nettet:
let fact n =  downto1 (*) n 1


//Peters bud på den sidste - heller ikke helt korrekt. 
let buildList g n = downto1 g n []