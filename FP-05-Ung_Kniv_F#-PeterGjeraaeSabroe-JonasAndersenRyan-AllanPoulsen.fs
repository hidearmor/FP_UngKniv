module test
type 'a BinTree =
    Leaf
  | Node of 'a * 'a BinTree * 'a BinTree
// 5.1
// generic version of tree for float compability
type Tree<'a> = Leaf
                | Node of 'a*Tree<'a>*Tree<'a>

// type Tree = Leaf
//           | Node of int*Tree*Tree

//The  function from the slides
let intBinTree =
    Node(43, Node(25, Node(56,Leaf, Leaf), Leaf),
        Node(562, Leaf, Node(78, Leaf, Leaf)))

//inOrder function. 
let rec inOrder tree = 
    match tree with 
      Leaf -> []                            //<-- The input is a leaf (ie an empty tree), no values to return
    | Node(n, treeL, treeR) -> inOrder treeL @ [n] @ inOrder treeR; //<--First traverse the left tree, append (@) n to the list that comes 
inOrder intBinTree                                                  //out of this (effectively a new list). Then traverse the right tree 
                                                                    //Concatenate the lists uing @

// -----------------

// 5.2
let rec mapInOrder f tree =         //<-- take a function f
    match tree with                 
    | Leaf -> Leaf                  //<-- if you are a leaf, then remain a leaf
    // evaluer individuelt og put ind i node bagefter, så vi antid går med ventre træ FØR vi når til vores øverste node
    | Node(n, treeL, treeR) -> 
        let tl = mapInOrder f treeL
        Node(f n, tl, mapInOrder f treeR)
     //<-- Apply the function to all the nodes.

// let print tree = mapInOrder (fun elem -> printfn "%A" elem; elem) tree 

//mapinorder vs mappost explanation:
// mapinorder traverses the binary tree from left to right and applies the function to each node in that order (ie. first left subtree, then current node, then right subtree). 
// The structure of the tree does not change as such but the nodes will have the function applied to them
// The mappostorder will apply the function in the order of left subtree, then right subtree then current node. Again, no changes to structure of the tree, but the nodes will have the 
// but the funciton will be applied.

//-----------------

// 5.3
// Based on exercise 4.2 and 4.3:
// 1. the "inOrder tree" element of the sentence produces a list, using the inORder function above
// 2. the "|>" pipes the list into the function
// 3. List.fold is a library function (we used it in exercise 4.2)
// 4. "(fun x xs -> f xs x) x" does the following (based on exercise 4.2): a) takes the arguments
//      x and xs, where x is the current element being piped into the function
//      and xs is the accumulated value is the list is being piped into
//      the function

let foldInOrder f xs tree = 
    inOrder tree |> List.fold (fun xs x -> f x xs) xs

foldInOrder (fun n a -> a + n) 0 intBinTree;;

let floatBinTree = Node(43.0,Node(25.0, 
    Node(56.0,Leaf, Leaf), Leaf), Node(562.0, Leaf, Node(78.0, Leaf,Leaf)))

foldInOrder (fun n a -> a + n) 0.0 floatBinTree;;

//------------------

//5.4 + 5.5
type aExp =                     (* Arithmetical expressions *)
    | N of int                  (* numbers *)
    | V of string               (* variables *)
    | Add of aExp * aExp        (* addition *)
    | Mul of aExp * aExp        (* multiplication *)
    | Sub of aExp * aExp        (* subtraction *)

type bExp =                     (* Boolean expressions *)
    | TT                        (* true *)
    | FF                        (* false *)
    | Eq of aExp * aExp         (* equality *)
    | Lt of aExp * aExp         (* less than *)
    | Neg of bExp               (* negation *)
    | Con of bExp * bExp        (* conjunction *)

type stm =                      // statements
    | Ass of string * aExp      // assignment
    | Skip
    | Seq of stm * stm          // sequential composition
    | ITE of bExp * stm * stm   // if-then-else
    | While of bExp * stm       // while
    | IT of bExp * stm          // if-then
    | RU of stm * bExp          //  repeat until


let rec A a s =
    match a with
    | N n           -> n
    | V x           -> Map.find x s
    | Add(a1, a2)   -> A a1 s + A a2 s
    | Mul(a1, a2)   -> A a1 s * A a2 s
    | Sub(a1, a2)   -> A a1 s - A a2 s;;

let rec B b s =
    match b with
    | TT            -> true
    | FF            -> false
    | Eq(a1, a2)    -> A a1 s = A a2 s
    | Lt(a1, a2)    -> A a1 s < A a2 s
    | Neg(b1)       -> if B b1 s = true then false else true 
    | Con(b1, b2)   -> 
        if B b1 s = true && B b2 s = true then true else false

let rec I stm s =
    match stm with
    | Ass(x,a)          -> Map.add x (A a s) s      // vi lægger en ny value ind med en string og et tal
    | Skip              -> s        // den her bruger vi når vi skal returnere det vi allerede har, som: I Skip s
    | Seq(stm1, stm2)   -> I stm1 s |> I stm2 
        // fordi det første returner et state kan vi pipe det der er ikke s på nr. 2 fordi det er et env vi piper ind i nr. 2
    | ITE(b,stm1,stm2)  -> if B b s then I stm1 s else I stm2 s
    | While(b, stm)     -> if B b s then ((I stm s) |> I (While(b, stm))) else (I Skip s)
                                    //"if B b s then ((I stm s) |>"  --> If b is true then pipe the stm from s into the following 
                                    //"|> I (While(b, stm))"         --> take the statement that is being piped into you and recursively call 
                                    // itself with stm and the original environment s (as long as b is  true)
                                    // "else (I Skip s)" otherwise Skip

    | IT(b, stm1)       -> if B b s then I stm1 s else s
    | RU(stm1, b)       -> if (B (Neg(b)) s) then ((I stm s) |> I (RU(stm, b))) else (I Skip s)
                                    //"if B Neg(b) s then ((I stm s) |>"  --> If b is negative then pipe the stm from s into the following 
                                    //"|> I (While(b, stm))"  --> take the statement that is being piped into you and recursively call 
                                    // itself with stm and the original evironment s (as long as b is true)
                                    // "else (I Skip s)" otherwise Skip

// Example 0
let stmt0 = While((Eq((N 2), (N 3))), Skip)
let state0 = Map.empty
I stmt0 state0 
// Example 1
let stmt1 = ITE(FF, Ass(("yes"), Mul(N(1),N(3))), Skip)
let state1 = Map.empty |> Map.add "yo" (A (Mul(N 4, N 6)) Map.empty)
I stmt1 state1
// Example 2
let stmt2 = Ass("your mom", Mul(N 3, N 5))
let state2 = state1 |> Map.add "state2" (A (N 3) state0)
I stmt2 state2
// Example 3d
let stmt3 = IT(Lt(N 7, N 8), Seq(Ass("Young knife", Sub(N 10, N 1)), Ass("Even younger knife", Sub(N 10, N 4))))
let state3 = state2 |> Map.add "state3" (A (N 9) state1)
I stmt3 state3
// Example 4
let stmt4 = Seq(IT(Con(TT, TT), Ass("k", N 1)), IT(Con(TT, TT), Ass("to-the nife", N 4)))
let state4 = state3 |> Map.add "state4" (A (N 11) state3)
I stmt4 state4
// Example 5
let stmt5 = RU(Ass("YKF#", Add(V "YKF#", N 1)), Lt(V "YKF#", N 5))
let state5 = state4 |> Map.add "YKF#" 0
I stmt5 state5

//Exercise 5.6
// Explaining the inc(x), we would take the following actions:
// 1. We would add inc(x) to aExp as a type aExp
// 2. We would then add inc(x) to the function A a s as inc(x) -> A (Add((x), (N 1))) s
//    this way it should be possible to use inc(x) in correspondance with the general structure,
//    as it returns an aExp.
//    Specifically Inc(x) would be applied to Ass in the interpreter function to ensure the incrementation in the state.
