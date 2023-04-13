// Computing slowfib(42) is CPU-intensive, ca 2 sec. Mono Mac 

let rec slowfib n = if n<2 then 1.0 else slowfib(n-1) + slowfib(n-2);;

// Don Syme examples, adapted
// Times are Mono 6.12.0 on MacOS 12.6.4 2,5 GHz Quad-Core Intel Core i7

let fib42 = slowfib(42);;
// Real: 00:00:02.129, CPU: 00:00:02.127, GC gen0: 0, gen1: 0

let fibs = [ slowfib(41); slowfib(42) ];;
// Real: 00:00:03.448, CPU: 00:00:03.446, GC gen0: 0, gen1: 0

let fibs =
  let tasks = [ async { return slowfib(41) };
                async { return slowfib(42) } ]
  Async.RunSynchronously (Async.Parallel tasks);;
// Real: 00:00:02.157, CPU: 00:00:03.490, GC gen0: 0, gen1: 0

let fibs = [ for i in 0..42 do yield slowfib(i) ];;
// Real: 00:00:05.509, CPU: 00:00:05.507, GC gen0: 0, gen1: 0

let fibs =
    let tasks = [ for i in 0..42 do yield async { return slowfib(i) } ]
    Async.RunSynchronously (Async.Parallel tasks);;
// Real: 00:00:02.332, CPU: 00:00:06.653, GC gen0: 0, gen1: 0

// Dissection of this:

// async { return slowfib(i) }
// has type: Async<float>
// an asynchronous tasks that, when run, will produce a float

// let tasks = [ for i in 0..42 do yield async { return slowfib(i) } ]
// has type: Async<float> list
// a list of asynchronous tasks, each of which, when run, will produce a float

// Async.Parallel tasks
// has type: Async<float []>
// an asynchronous tasks that, when run, will produce a list of floats

// Async.RunSynchronously (Async.Parallel tasks)
// has type: float []
// a list of floating-point numbers
