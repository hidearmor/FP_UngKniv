// module QueueWithMistake

// Lock-based queue with memory management mistake
// sestoft@itu.dk * 2013-10-20
// nh@itu.dk - 2019-03-31:
//   Concurrency removed from example.
//   Now just a simple array with queues uesed.
//   Text below edited accordingly.
//   Translated from Java to F#.

// The SentinelLockQueue is a simple first-in first-out buffer,
// implemented as a linked list of Nodes.  Method queue.put(x) inserts
// item x and method queue.get() removes and returns an item provided
// the queue is not empty.

// The queue implementation in class SentinelLockQueue has an extra
// "sentinel" Node so that the head and tail fields always have a Node
// to point to, even when the queue is empty.  This means that the
// first integer item in the queue is not head.item but head.next.item.

// The queue implementation contains a programming mistake that
// usually causes the test program to run out of memory quite soon
// although this should not happen.

// The mistake actually appears in an example in Goetz et al: Java
// Concurrency in Practice, 2006, page 334 (ch 15) and the book's
// errata and online source code do not correct it.  But in all other
// respects it is an extremely useful and recommendable book!

type Node(item:int, next:Node option) =
  let mutable _item = item
  let mutable _next = next

  member this.Item
    with get() = _item
    and  set(v) = _item <- v
  member this.Next
    with get() = _next    
    and  set(v) = _next <- v

// --------------------------------------------------
// Locking queue, with sentinel (dummy) node

type SentinelLockQueue() =
  // With sentinel (dummy) node.
  // Invariants:
  //  * The node referred by tail is reachable from head.
  //  * If non-empty then head != tail, 
  //     and tail points to last item, and head.next to first item.
  //  * If empty then head == tail.

  let dummy = Node(-444,None)
  let mutable head = dummy
  let mutable tail = dummy

  member this.put(item) =
    let node = Node(item, None)
    tail.Next <- Some node;
    tail <- node

  member this.get() = // this method seems legit, besides the -999 thing
    if (head.Next = None)
      then -999  // the list is empty. this should be an option that returns None here ?

      // else  // ORIGINAL VERSION
      //   let first = head
      //   head <- first.Next.Value // this is a mutable structure
      //   head.Item

        // numbers with 20000000
        // Real: 00:00:24.751, CPU: 00:00:25.250, GC gen0: 7143, gen1: 2382, gen2: 1
        // numbers with 100000000
        // Real: 00:01:59.079, CPU: 00:01:59.948, GC gen0: 35564, gen1: 11857, gen2: 1

      else    // NEW VERSION
        let first = head.Next.Value // get the node, not the Option<Node>
        head.Next <- first.Next    // we keep the dummy as the head
        first.Item
        // numbers with 20000000
        // Real: 00:00:14.974, CPU: 00:00:15.517, GC gen0: 6705, gen1: 439, gen2: 1
        // numbers with 100000000
        // Real: 00:01:49.516, CPU: 00:01:53.002, GC gen0: 26350, gen1: 8791, gen2: 4

let time f =
  let start = System.DateTime.Now in
  let res = f () in
  let finish = System.DateTime.Now in
  (res, finish - start)

let run() =
  // let iterations = 20000000
  let iterations = 100000000
  let noQueues = 20
  let queues = Array.init noQueues (fun _ -> SentinelLockQueue())
  let doIter j =  // a guess could be that it should be a tail-recursive thing
    queues.[j].put(42);
    for i in 0 .. iterations-1 do
      queues.[j].put(i);
      queues.[j].get()
    |> ignore
  for j in 0 .. noQueues-1 do
    let (_,t) = time (fun () -> doIter j)
    printfn "Qno. %2d\t %10d %A\n" j (queues.[j].get()) t

run();;
