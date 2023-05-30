// Question 1.1 ------------------------
let infSeq3 = Seq.initInfinite (fun i -> i * 3)
infSeq3

let finSeq3 n = Seq.take(n) infSeq3
finSeq3 2

let sumSeq3 n =
    let mySeq = finSeq3 n
    Seq.sum (mySeq)

sumSeq3 100

// Questions 1.2 -----------------------------
let seqMap2 f s1 s2 =
    seq { for (x,y) in Seq.zip s1 s2 do
            yield f x y }


let seq1 = finSeq3 6
let seq2 = finSeq3 3

seqMap2 ( + ) seq1 seq2

let swap (x,y) = (y,x)

swap (3,4)

// seqMap2 swap [1;3;3] [4;5;2]
// does notwork because swap takes int whereas seqMap2 takes sequence

let seq3 = [1;3;3]
let seq4 =  [4;5;2]

let fixSwap s1 s2 =
    let zipped = Seq.zip s1 s2
    let swapped = Seq.map (fun (x,y) -> (y,x)) zipped 
    swapped

fixSwap seq3 seq4


// seqMap2 (fixSwap seq3 seq4) ( + )

// Quesiotn 2.1 ------------------------------
type TrieNode<'a when 'a : equality> = TN of 'a * bool * TrieNode<'a> list

let trie01 = TN('a',false,[TN('n',false,[TN('d',true,[])])])
trie01

let trie04 = TN('a', false, 
                    [ TN('n', true, [ TN('d', true, []) ])
                      TN('d', false, [ TN('d', true, []) ])
                      TN('t', true, [ TN('x', false, [])])]
                )

let trie03 = TN('a', false, 
                    [ TN('n', true, [ TN('d', true, []) ])
                      TN('d', false, [ TN('d', true, []) ])
                      TN('t', true, [])]
                )

// Type trie04: TrieNode<Char>, monomorphic

exception TrieError of string

// Question 2.2 -------------------------------
let rec numLetters tn =
    match tn with
    | TN (_, _, []) -> 1
    | TN (_, _, adjList) -> 1 + (List.sumBy numLetters adjList)

numLetters trie04


let rec numWords tn =
    match tn with
    | TN (_, bool, []) -> if bool then 1 else 0
    | TN (_, bool, adjList) ->
        if bool then
            1 + (List.sumBy numWords adjList)
        else
            List.sumBy numWords adjList

numWords trie04

// Question 2.3 -------------------------

let rec map f t  =
    match t with
    | (ch, bool, []) -> TN (f ch, bool, [])
    | (ch , bool, list) -> TN(f ch, bool, (List.map (map f)) list)



// Question 3.1----------------------
let rec F m i n k = 
    if k <=0 
    then m
    else F m i n (k-1) * (1.0 + i/n)

F 100.0 0.1 1.0 0
F 100.0 0.1 1.0 10

//It is not tail recursive

//Tail recursive version
let FA m i n k acc = 
    let rec FA' k acc =
        if k <=0 
        then acc
        else FA' (acc * (1.0 + i/n)) (k - 1)
    FA' m k 
    //Well....


let tabulate f start step stop = 
    let myList = [start .. step .. stop]
    List.map (fun x -> (x, f x)) myList

tabulate (F 100.0 0.1 1.0) 0 2 4


let prettyPrint (myList : list<int*float>) = 
    printfn " x | f(x) "
    printfn "---+-----"
    List.iter (fun (x, y) -> printfn " %i |  %f" x y) myList

prettyPrint [ (0, 100.0)
              (2, 121.0)
              (4, 146.41) ]

// Question 4.1-------------------------
let dt(d,m,y) = System.DateTime(y, m, d)

exception Error of string

type Position =
    | Stock of string
    | Cash of float

type Action =
    | Aquire of System.DateTime * Position
    | Give of System.DateTime * Position
    | Scale of int * Action
    | All of Action list

let ex1 =
    Scale(100,All[Aquire (dt(1,2,2018),Stock "APPLE");
                    Give (dt(1,2,2018),Cash 300.3)])

let sellApple = 
    Scale(100, All[Give (dt(1, 3, 2018), Stock "APPLE" ); 
                    Aquire(dt(1, 3, 2018), Cash 400.4)])


let stockDate =
    Map
        .empty
        .Add(("APPLE", dt (1, 2, 2018)), 300.3)
        .Add(("APPLE", dt (1, 3, 2018)), 400.4)
        .Add(("ISS", dt (1, 2, 2018)), 150.0)
        .Add(("ISS", dt (1, 3, 2018)), 200.2)
        .Add(("TIVOLI", dt (1, 2, 2018)), 212.0)
        .Add(("TIVOLI", dt (1, 3, 2018)), 215.2)


let price (stock, date) = 
    match stockDate.TryFind (stock, date) with
    | None -> failwith "Fail!"
    | Some p -> p

price ("ISS", dt(1, 3, 2018))
let price1 stdt = 
    match stockDate.TryFind (stdt) with
    | None -> failwith "Fail!"
    | Some p -> p

price1 ("ISS", dt(1, 3, 2018))

// Question 4.2 ---------------------------------------
let buyStock n stock date  =
    match stockDate.ContainsKey(stock, date) with   
    | false -> failwith "Price not known or stock not existing"
    | true -> 
        Scale(100,All[Aquire (date, Stock stock);
                    Give (date,Cash (price (stock, date)))])

buyStock 100 "APPLE" (dt(1,2,2018))

let buyStock2 n stock date  =
    let price = price1 (stock, date)
    Scale(n, All [Aquire (date, Stock stock);
                       Give (date, Cash price)])
buyStock2 100 "APPLE" (dt(1,2,2018))

let receiveCash cash da= 
    Aquire (da, Cash cash)

receiveCash 100000.0 (dt(1,2,2018))

let actions =
    let d1 = dt(1,2,2018)
    let d2 = dt(1,3,2018)
    All [receiveCash 100000.0 d1;
        buyStock 100 "APPLE" d1;
        buyStock 200 "ISS" d1;
        buyStock 50 "TIVOLI" d2]


type stockEnv = Map<string,int>
let updStock s n m =
    match Map.tryFind s m with
          None -> Map.add s n m
        | Some n1 -> Map.add s (n+n1) m

type env = float * stockEnv
let emptyEnv = (0.0,Map.empty)

let updEnv scaling (cash, stockEnv) pos =
    match pos with
    | Cash x -> env (cash + x * float scaling, stockEnv)
    | Stock y -> env (cash, (updStock y scaling stockEnv))

updEnv 100 emptyEnv (Cash 100.0)


//Hmm de sidste her de driller...

let execA a env = 
    match a with
    | [] -> []
    | 


// let execA action env =
//     let rec exec scaling env = function
//           Aquire(d,p) -> ...
//         | ...
//         exec 1 env action
