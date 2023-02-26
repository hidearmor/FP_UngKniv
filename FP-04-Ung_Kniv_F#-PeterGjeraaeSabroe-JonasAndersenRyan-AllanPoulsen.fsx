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
// let toUpper (s:string) = failwith "not implemented"

//Jonas' One-liner
let toUpper (s:string) = implode(List.map (fun c -> System.Char.ToUpper c) (explode s))

//Allan's suggestion:
let toUpper (s:string) = 
    let d = explode s
    let g = List.map System.Char.ToUpper d
    implode g


// explode >> map >> implode
// let toUpper1 (s:string) = failwith "not implemented"

//Allan's suggestion
let toUpper1 (s:string) = (explode >> List.map System.Char.ToUpper >> implode) s

// let toUpper2 (s:string) = failwith "not implemented"
// let toUpper2 (s:string) = implode (explode s |> List.map(fun x -> (Char.ToUpper(x))));;
// Allan's suggestion
let toUpper2 (s:string) = s |> (implode << (explode >> List.map System.Char.ToUpper))


//Peter's suggestion
let toUpper2 (s:string) = s |> (implode << List.map System.Char.ToUpper << explode)


// Exercise 4.4 - palindrome - treating empty strings as palindromes too.
// let rec palindrome (s:string) = failwith "not implemented"
// Allan's suggestion
let palindrome (s:string) = 
    let forward = toUpper1 s
    let backward = s |> (implodeRev << (explode >> List.map System.Char.ToUpper))
    if forward = backward 
    then true
    else false

// Exercise 4.5 - ack
// let rec ack t = failwith "not implemented"
// Allan's suggestion
let rec ack(m, n) = 
    match m, n with
    | 0, _ -> n + 1
    | _, 0 -> ack((m-1), 1)
    | _, _ -> ack((m-1), ack(m, (n-1)))
    // ack(3, 11) = 16381

// Exercise 4.6 - time
let time f = failwith "not implemented"
    
let timeArg1 f a = failwith "not implemented"

// Exercise 4.7 - HR 5.4 - downTo2 f n e
let rec downto1 f (n, e) = failwith "not implemented"
// factorial function using downto1 for recursion.
let fact n = failwith "not implemented"
let buildList g n = failwith "not implemented"