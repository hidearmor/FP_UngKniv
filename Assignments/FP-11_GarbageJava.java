// Lock-based queue with memory management mistake
// sestoft@itu.dk * 2013-10-20
// nh@itu.dk - 2019-03-31:
//   Concurrency removed from example.
//   Now just a simple array with queues uesed.
//   Text below edited accordingly.

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

class QueueWithMistake {
    public static void main(String[] args) {
        run ();
    }
  
    private static void run() {
      final int iterations = 20000000; // Increase this constant if program does not run out of memory.
      final int noQueues = 20; // Number of queues.
      final Timer timer = new Timer();
      Queue[] queues = new SentinelLockQueue[noQueues];
      for (int j=0; j<noQueues; j++) {
          queues[j] = new SentinelLockQueue();
          queues[j].put(42);
          for (int i=0; i<iterations; i++) {
              queues[j].put(i);
              queues[j].get();
          }
          System.out.printf("%-20s\tQno. %2d\t%7.2f\t%s%n", 
                            queues[j].getClass().getName(), j,
                            timer.Check(), queues[j].get());
      }
    }
  }
  
  interface Queue {
    boolean put(int item);
    int get();
  }
  
  // --------------------------------------------------
  // Locking queue, with sentinel (dummy) node
  
  class SentinelLockQueue implements Queue {  
    // With sentinel (dummy) node.
    // Invariants:
    //  * The node referred by tail is reachable from head.
    //  * If non-empty then head != tail, 
    //     and tail points to last item, and head.next to first item.
    //  * If empty then head == tail.
  
    private static class Node {
      final int item;
      volatile Node next;
      
      public Node(int item, Node next) {
        this.item = item;
        this.next = next;
      }
    }
  
    
  // OLD CODE: 
  // private final Node dummy = new Node(-444, null);
  // private Node head = dummy, tail = dummy;

  // NEW CODE:
      private Node head = new Node(-444, null);
      private Node tail = head;

  // Creating the "dummy" Node, makes the dummy variable sit on the stack
  // pointing at the initial Node all the time, so when as the we the Sentinel Lock Queue
  // grows and grows never allowing the garbage collector to collect anything
  // since every Node on the heap is being pointed at, cause to the dummy variable.

  
    
    public boolean put(int item) {
      Node node = new Node(item, null);
      tail.next = node;
      tail = node;
      return true;
    }
  
    public int get() {
      if (head.next == null) 
        return -999;
      Node first = head;
      head = first.next;
      return head.item;
    }
  }
  
  // Crude timing utility ----------------------------------------
     
  class Timer {
    private long start, spent = 0;
    public Timer() { Play(); }
    public double Check() { return (System.nanoTime()-start+spent)/1E9; }
    public void Pause() { spent += System.nanoTime()-start; }
    public void Play() { start = System.nanoTime(); }
  }