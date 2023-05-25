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
