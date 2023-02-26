module a2
// Exercise 4.1 - explode - string to char list
let explode (s:string) = List.ofArray(s.ToCharArray())


let rec explode2 (s:string) = 
    match s with //does not work with "function" keyword, but does mit "match s with"
    | ""  -> []
    | _ -> s.Chars(0)::explode2(s.Remove(0,1))






// Exercise 4.2 - implode - char list to string
let implode (list:list<char>) = 
    List.foldBack 
        (fun (c:char) s -> string c + s)
        list
        ""

let implodeRev (list:list<char>) =
    List.fold 
        (fun s (c:char) -> string c + s)
        "" 
        list



// Exercise 4.3 - toUpper
//Jonas' One-liner
let toUpper (s:string) = implode(List.map (fun c -> System.Char.ToUpper c) (explode s))

//Allan's suggestion:
let toUpper (s:string) = 
    let d = explode s
    let g = List.map System.Char.ToUpper d
    implode g



// let toUpper1 (s:string) = failwith "not implemented"
//Allan's suggestion
let toUpper1 (s:string) = (explode >> List.map System.Char.ToUpper >> implode) s

//Jonas' suggestion
let toUpper1 (s:string) = 
    // (List.map (fun c -> System.Char.ToUpper c) explode s) >> implode
    explode >> List.map (fun c -> System.Char.ToUpper c) >> implode 
    // but this one returns a function and not a value 
    // - weren't we supposed to make it do the same as toUpper?



// let toUpper2 (s:string) = failwith "not implemented"
// Allan's suggestion
let toUpper2 (s:string) = s |> (implode << (explode >> List.map System.Char.ToUpper))

//Peter's suggestion
let toUpper2 (s:string) = s |> (implode << List.map System.Char.ToUpper << explode)

//Jonas' suggestion
let toUpper2 (s:string) =
    explode s |> (implode << (List.map (fun c -> System.Char.ToUpper c)))





// Exercise 4.4 - palindrome - treating empty strings as palindromes too.
// let rec palindrome (s:string) = failwith "not implemented"
// Allan's suggestion
let palindrome (s:string) = 
    let forward = toUpper1 s
    let backward = s |> (implodeRev << (explode >> List.map System.Char.ToUpper))
    if forward = backward 
    then true
    else false

//Jonas' suggestion
// not a recursive implementation, but a simple and effective one
let palindrome (s:string) = (s = (explode s |> implodeRev))

//Peters suggestion
let palindrome (s:string) =  s = (s |> (implode << List.rev << explode))





// Exercise 4.5 - ack
// let rec ack t = failwith "not implemented"
// Allan's suggestion
let rec ack(m, n) = 
    match m, n with
    | 0, _ -> n + 1
    | _, 0 -> ack((m-1), 1)
    | _, _ -> ack((m-1), ack(m, (n-1)))

//Peters suggestion
let rec ack t = 
    match t with
    |(0,n) -> n+1
    |(m,0) -> ack((m-1),1)
    |(m,n) -> ack(m-1,ack(m, n-1)) 
    
// ack(3, 11) = 16381




// Exercise 4.6 - time
let time f =
    let start = System.DateTime.Now in
    let res = f () in
    let finish = System.DateTime.Now in
    (res, finish - start);
(*
    > time (fun () -> ack(3, 11));;
    Ovenstående expression giver 00:00:01.0542130
*)  

//Peters suggestion
let timeArg1 f a = time (fun g -> f a)





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

// Også taget fra nettet:
let fact n =  downto1 (*) n 1


//Peters bud på den sidste - heller ikke helt korrekt. 
let buildList g n = downto1 g n []