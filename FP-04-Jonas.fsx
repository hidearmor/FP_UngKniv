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
// let toUpper (s:string) = 
//     implode(explode s |> List.map (fun c -> System.Char.ToUpper c))

let toUpper (s:string) = 
    implode(List.map (fun c -> System.Char.ToUpper c) (explode s))

// explode >> map >> implode
let toUpper1 (s:string) = 
    // (List.map (fun c -> System.Char.ToUpper c) explode s) >> implode
    explode >> List.map (fun c -> System.Char.ToUpper c) >> implode 
    // but this one returns a function and not a value 
    // - weren't we supposed to make it do the same as toUpper?
    
let toUpper2 (s:string) =
    explode s |> (implode << (List.map (fun c -> System.Char.ToUpper c)))

// Exercise 4.4 - palindrome - treating empty strings as palindromes too.
// not a recursive implementation, but a simple and effective one
let palindrome (s:string) = (s = (explode s |> implodeRev))

// Exercise 4.5 - ack
let rec ack t = failwith "not implemented"
    // ack(3, 11) = 16381

// Exercise 4.6 - time
let time f = failwith "not implemented"
    
let timeArg1 f a = failwith "not implemented"

// Exercise 4.7 - HR 5.4 - downTo2 f n e
let rec downto1 f (n, e) = failwith "not implemented"
// factorial function using downto1 for recursion.
let fact n = failwith "not implemented"
let buildList g n = failwith "not implemented"