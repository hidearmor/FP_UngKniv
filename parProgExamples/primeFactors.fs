// F# parallel computing examples inspired by Hansen and Rischel
// chapter 13.6 * sestoft@itu.dk * 2013, 2017-03-20

let isPrime n =
    let rec testDiv a = a*a > n || n % a <> 0 && testDiv (a+1)
    n>=2 && testDiv 2;;

let factors n =
    let rec factorsIn d m =
        if m <= 1 then []
        else if m % d = 0 then d :: factorsIn d (m/d) else factorsIn (d+1) m
    factorsIn 2 n;;

let random n =
    let generator = new System.Random ()
    fun () -> generator.Next n;;

let r10000 = random 10000;; // 150x faster than creating a new System.Random

let rec ntimes (f : unit -> 'a) n =
    if n=0 then () else (ignore (f ()); ntimes f (n-1));;
    
let bigArray = Array.init 5000000 (fun _ -> r10000 ());;

#time;;

Array.map isPrime bigArray;;
// Real: 00:00:00.336, CPU: 00:00:00.336
Array.Parallel.map isPrime bigArray;;
// Real: 00:00:00.078, CPU: 00:00:00.609

// Better example: Prime factors of random numbers (more work)

Array.map factors bigArray;;
// Real: 00:00:00.078, CPU: 00:00:00.609
   
Array.Parallel.map factors bigArray;;
// Real: 00:00:19.062, CPU: 00:00:19.986
   
// Even better example: Prime factors of [1..200000]

Array.init 200000 factors;;
// Real: 00:00:10.337, CPU: 00:00:10.481

let factors200000 = Array.Parallel.init 200000 factors;;
// Real: 00:00:02.138, CPU: 00:00:16.414

let histogram = Array.init 200000 (fun i -> 0)
let incr i = histogram.[i] <- histogram.[i] + 1
Array.iter (fun fs -> List.iter incr fs) factors200000;;
// Real: 00:00:00.007, CPU: 00:00:00.007

histogram;;
