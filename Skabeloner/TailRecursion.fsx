//--------------------------------------------------------
//------------------- ACCUMULATION -----------------------
//--------------------------------------------------------

// Function to generate a list of size n, using accumulation (H.R. 204)
// Here xs is the accumulating parameter which is returned when n = 0
let rec bigListA n xs = if n=0 then xs else bigListA (n-1) (1::xs)


// Factorial function made with accumulation (H.R. 206-207)
// The function is called with m = 1 and n = "the number you want to faculty"
// m is the accumulating parameter.
let rec factA = function
  | (0,m) -> m
  | (n,m) -> factA (n-1, n*m)

//Same as above, but with match-with instead of the function keyword
let rec factA (n,m) = 
    match (n,m) with
    | (0,m) -> m
    | (n,m) -> factA (n-1, n*m)


// Reverse list funcion made with accumulation (H.R. 208-209)
// The function is called with ys = [] and xs = "the list you want to reverse"
// ys is the accumulating parameter.
let rec revA = function
    ([], ys) -> ys
  | (x::xs, ys) -> revA(xs, x::ys)


// Inner function is tail recursive, with acc is the accumulating parameter (Assignment 7)
// This functions sums the parameters in the following way:
// (m+n) + (m+(n-1)) + (m+(n-2)) + ... + (m+1) + (m+0)
let sum2(m,n) =
  let rec tail n acc = 
    match n with
    | 0 -> acc + m
    | n -> tail (n-1) (acc+m+n)
  tail n 0

// Same structure as above, acc is accumulating parameter. (Assignment 7)
// This function returns the length of a list
let length xs = 
  let rec tail xs acc = 
    match xs with
    | [] -> acc
    | x::xs -> tail (xs) (acc+1)
  tail xs 0


// A little more advanced, here the accumulating parameter is both n1 and n2 (Assignment 7)
// When calling fibA n indicates the nth Fibonacci number, which is what the functions returns (0-indexed)
// n1 and n2 is the first to Fibonacci numbers, 0 and 1 respectively.
// So fibA 10 0 1 returns the 10th Fibonacci number
let rec fibA n n1 n2  = 
  match n with
  | n when n < 0 -> failwith "nope"
  | 0 -> 0
  | 1 | 2 -> n1 + n2                  
  | _     -> fibA (n-1) n2 (n1+n2)


// Another example of a tail-recursive functions that alters a list (Exam 2014, 2.2)
// The accumulating parameter in this case is to be instantiated as List.Empty
let rec fA acc i = function
    |[] -> List.rev (i::acc)
    |x::xs -> fA ((i+x)::acc) (i+1) xs



//--------------------------------------------------------
//------------------- CONTINUATION -----------------------
//--------------------------------------------------------

// NOTE! Hence c always has one of two meanings:
// When calling a continuation function almost always we use id as the c-parameter.
// Bear in mind this id function is only called at the very, very end of our tail-recursion
// namely to retrieve the result of the function.
// In all other tail-recursive calls, c is the anonymous function defined within 
// the function-body, the (fun x -> ...) part.
//
// REMEMBER that c MUST be in the anonymous function as below (fun res -> c(1::res)), in order
// to trace back the result when the function arives at the last recursion.
// 
// ALSO REMEMBER c MUST appear in the base case, that being when n=0 or [] or whatever 
// criteria is set in the pattern matching.


// The function below computes a list of size n (H.R 213)
// In the last recursive call the empty list is put into the continuation function
// of the second last recursive call. This function takes the empty list "res" and 
// cons 1 to, before propagating it into the continuation function of the third last 
// recursive call, and so on, until we reach the first recursive call where the list 
// is put into the id function and returned.
let rec bigListC n c =
  if n=0 then c []
  else bigListC (n-1) (fun res -> c(1::res))


// A slightly more advanced example, counting the nodes in a tree (H.R. 214-215)
// Important to note the c appears both in the anonymous contiunation function
// and the base case (Leaf).
// The key to understand this function is to appreciate that "vl" is the total count
// of nodes in the left subtree and "vr" is the total count of nodes in the right
// right subtree. These values haven't yet been calculated yet, but it makes 
// sense that when we visit the first node (root) the total count of nodes in the tree 
// is vl+vr+1 (the root), which is what we have in the continuation function in the end
// of the line. 
let rec countC t c =
 match t with
   | Leaf -> c 0
   | Node(tl,n,tr) -> countC tl (fun vl -> countC tr (fun vr -> c(vl+vr+1)))


// A Fibonacci contiunation function. Think of res1 as the (n-1)th Fib number
// and res2 as the (n-2)th Fib number then clearly the nth Fib number is res1+res2
// as stated in the continuation function. 
// I still get dizzy when trying to fully grasp this function, and as I recall there was a better 
// solution to the Fib problem, since this function takes quadratic time.
let rec fibC n c =
  match n with
    0 -> c 0
  | 1 -> c 1
  | n -> fibC (n-1) (fun res1 -> fibC (n-2) (fun res2 -> c(res1 + res2)))

// Heres the non-quadratic version from our Assignment 7.
// Still quite advanced.
let fibC n =
  let f = (fun a _ -> a) // hide away the continuation function
  let rec inner m (c : int -> int -> int) =  // make the continuation funtion take TWO arguments
    match m with
    | m when m < 0 -> failwith "nope"
    | 0 -> c 0 1
    | 1 -> c 1 1
    | _ -> inner (m-1) (fun a b -> c b (a + b)) // accumulate in param b and set param a to be previous b
  inner n f //start the recursion


// Another tail-recursive function creating a Tree, only with right subtrees. (Assignment 8)
// When we get to the base case (bottom of tree) we put a leave to the right, and then builds the 
// tree from the bottom up, putting a Leaf at every left subtree, and the tree that we are building
// as the right subtree, until we reaches the root (c is id) and returns. 
let rec rightTreeC n c = 
    match n with
    | n when n<0 ->  c Leaf
    | n -> rightTreeC (n-1) (fun vr -> c(Node(Leaf, n, vr)))


// Doing the same thing as function fA above in the ACCUMULATION section, but now using continuation (Exam 2014, 2.3)
// This code might be easier to understand when reading the exam question, but again notice the c appears
// both in the base case and the continuation function.
let rec fC c i = function
    |[] -> c [i]
    |x::xs -> fC (fun l -> c ((i+x)::l)) (i+1) xs

//--- EXAM NOTE ---
// In the solution for the Exam 2014 both function fA and fC above are wrapped into an outer function
// thus hiding away the accumulation/continuation parameter and making it bullet proof from a users
// point of view (that is, the user cannot accidentally put a malicious value for 'c' or 'acc' into the function
// which would lead to false results). See Exam solution version of fC below:
let fC i xs =
  let rec fC' i xs c =
    match xs with
      [] -> c [i]
    | x::xs -> fC' (i+1) xs (fun acc -> c(i+x::acc))
  fC' i xs id
