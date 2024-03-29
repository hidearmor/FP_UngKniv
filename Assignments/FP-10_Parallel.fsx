// Assignment 10

// These exercises concern parallel programming in F#.

// You should build on the files in the example code, found in folder
// "parProgExamples" in the code repository.

// --------------- EXERCISE 1 - DESCRIPTION --------------
// Run the slow Fibonacci computations from the lecture's
// examples on your own computer, file parallelfib.fs.  Use the #time
// directive to turn on timing, and see what is the best speed-up you can
// obtain for computing, say, the first 43 Fibonacci numbers using
// Async.Parallel.  This may be quite different on MS .NET than on Mono.

// -----    EXERCISE 1 BEGIN --------------------------
// slowfib from parallelfib.fs:
let rec slowfib n = if n<2 then 1.0 else slowfib(n-1) + slowfib(n-2);;
// Allan's laptop (slowfib 43;;): Real 00:00:01.857, CPU 00:00:01.812

// Calculating 43 Fibonacci numbers:
let fibs = [ for i in 0..42 do yield slowfib(i) ];;
// Allan's laptop: Real 00:00:03.021, CPU 00:00:02.875

// In parallel:
let parallelFibs =
    let tasks = 
        [ for i in 0..42 do yield async { return slowfib(i) } ]
    Async.RunSynchronously (Async.Parallel tasks);;
// Allan's laptop: Real 00:00:01.335, CPU 00:00:03.218
// Real time, more than twice as fast...
// --------    EXERCISE 1 END ----------------------------


// --------------- EXERCISE 2 ---------------
// Similarly, run the prime factorization example on your own
// computer, and see what speedup you get, file primeFactors.fs.

// --------    EXERCISE 2 BEGIN ------------------------
// code from primeFactors.fs:
let isPrime n =
    let rec testDiv a = a*a > n || n % a <> 0 && testDiv (a+1)
    n>=2 && testDiv 2;;

let rec factors n =
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
open System.Collections.Generic

Array.map isPrime bigArray;;
// Timing on Allan's laptop, Real: 00:00:00.136, CPU: 00:00:00.125
Array.Parallel.map isPrime bigArray;;
// Timing on Allan's laptop, Real: 00:00:00.146, CPU: 00:00:00.812

Array.map factors bigArray;;
// Timing on Allan's laptop, Real: 00:00:10.831, CPU: 00:00:10.437
   
Array.Parallel.map factors bigArray;;
// Timing on Allan's laptop, Real: 00:00:08.230, CPU: 00:00:13.484
   
Array.init 200000 factors;;
// Timing on Allan's laptop, Real: 00:00:04.422, CPU: 00:00:04.156

let factors200000 = Array.Parallel.init 200000 factors;;
// Timing on Allan's laptop, Real: 00:00:01.248, CPU: 00:00:08.421

// --------    EXERCISE 2 END ------------------------------

//---------------- EXERCISE 3 DESCRIPTION ---------------

        // The lecture's construction of a histogram (counting the
        // numbers of times that each prime factor 2, 3, 5, 7, 11 ... appears)
        // uses a side effect in the assignment

        //      histogram.[i] <- histogram.[i] + 1 

        // But side effects should be avoided.  Program the histogram
        // construction so that it does not use side effects but purely
        // functional programming.  There are several useful functions in the Seq
        // module.  The final result does not have to be an

        // int[] array,

        // but could be a

        // seq<int * int>

        // of pairs (p, n) where p is a prime factor and n is the number of times
        // p appears in the array of lists of prime factors.

// -----    EXERCISE 3 - START  --------

// THis version, which is not parrallel, is quicker to build than the ones below
// we think this can be because the Sequence library is highly optimized and
// and the map function is not as optimized. And finally the bottom one 
// they both take 00:00:00:000 to run

let hist2 =
    Seq.ofArray factors200000
        |> Seq.concat
        |> Seq.countBy id
// Real: 00:00:00.001, CPU: 00:00:00.001, GC gen0: 0, gen1: 0, gen2: 0
// val hist2: seq<int * int>

// ---   alternative versions   ---

// PARALLEL VERSION - THAT IS SLOWER
// let hist2Parallel =
//     factors200000
//     |> Array.Parallel.map (fun f -> Seq.ofList f)
//     |> Seq.concat
//     |> Seq.countBy id
// Real: 00:00:00.003, CPU: 00:00:00.018, GC gen0: 0, gen1: 0, gen2: 0
// val hist2Parallel: seq<int * int>


// A more manual version here:
// why is this significantly slower?

// let hist2Async =
//     let honk =
//         async {
//             let! factorSequences = 
//                 Async.Parallel (Array.map (fun f -> async { return Seq.ofList f }) factors200000)
//             let factors = 
//                 Seq.concat factorSequences
//             let result =
//                 Seq.countBy id factors
//             return result
//         }
//     Async.RunSynchronously(honk)
// Real: 00:00:00.280, CPU: 00:00:01.372, GC gen0: 41, gen1: 4, gen2: 1
// val hist2Async: seq<int * int>

// -----    EXERCISE 3 - END  --------


// --------------- EXERCISE 4 - DESCRIPTION ----------------------
    // Find the fastest way on your hardware to count the number
    // of prime numbers between 1 and 10 million (the correct count is
    // 664579).  Use this F# function to determine whether a given number n
    // is prime:

    // let isPrime n =
    //     let rec testDiv a = a*a > n || n % a <> 0 && testDiv (a+1)
    //     n>=2 && testDiv 2;;

    // or if you believe it would be faster in C, C# or Java, you may use this version:

    // private static boolean isPrime(int n) {
    //   int k = 2;
    //   while (k * k <= n && n % k != 0)
    //     k++;
    //   return n >= 2 && k * k > n;
    // }

    // Remember to use parallel programming to the extent possible.

// ----------   EXERCISE 4  BEGIN  --------------

// Calculating prime as per exercise description
let isPrime4 n =
    let rec testDiv a = a*a > n || n % a <> 0 && testDiv (a+1)
    n>=2 && testDiv 2;;

// the whole idea of this one is to skip the step of first going through all the numers
// and then going through all the numbers filtering again, but instead utilizing the
// option type to filter out all non-None, since it requires very little performance

let parallelPrimes2  =
    let tasks = 
        async { return seq {for i in 0..10000000 do if isPrime4 i then i}}
    Seq.length(Async.RunSynchronously (tasks))
// Real: 00:00:07.412, CPU: 00:00:11.018, GC gen0: 0, gen1: 0, gen2: 0
// val parallelPrimes2: int = 664579

// ----------   EXERCISE 4  END  --------------



// --------  ALTERNATIVE VERSION -  EXERCISE 4 BEGIN ----------------

// A WAY NON-PARALLEL TO CALCULATE THE NUMBER OF PRIMES ---------------
// Returns each prime in a list where everything else is 0
let primes = [ for i in 0..10000000 do 
                            if isPrime4(i) then i else 0 ];;

// Filters, so that we only have the primes and not the zeros
let onlyPrimes = List.filter (fun x -> isPrime4 x) primes

// Takes the length of the list
let noOfPrimes = onlyPrimes.Length

// Above is inspired by https://www.youtube.com/watch?v=pHDzgp7zDR0 

// These could be consolidated into this, which takes a long time
let noOfPrimes2 = primes |> List.filter isPrime4 |> List.length


// A PARALLEL WAY TO CALCULATE THE NUMBER OF PRIMES -------------
// well, this is almost definitely not what we are looking for. It works though :-)
let parallelPrimes  =
    let tasks = 
        [ for i in 0..10000000 do yield async { return if isPrime4(i) then i else 0 } ]
    Async.RunSynchronously (Async.Parallel tasks);;

// Same idea as above
let parallelOnlyPrimes = Array.filter (fun x -> isPrime4 x) parallelPrimes

// Same idea as above
let parallelNoOfPrimes = parallelOnlyPrimes.Length

// Same idea as above
let parallelNoOfPrimes2 = parallelPrimes |> Array.filter isPrime4 |> Array.length

//Let's try to parallelise the shit of this:
// I think this is more along the line of what we are looking for
// Roughly four times faster in Real time on ALlan's laptop.
Array.Parallel.map isPrime4 parallelPrimes |> Array.filter (fun x -> x = true) |> Array.length

// --------  ALTERNATIVE VERSION  EXERCISE 4 END -----------------------------------
