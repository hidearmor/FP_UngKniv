// tree that gives no fuck about types
type Tree<'a> =
    Leaf
  | Node of Tree<'a>*'a*Tree<'a>

let ex =
  Node(Node(Node(Leaf,2,Leaf),7,Leaf),
       9,
       Node(Node(Leaf,13,Leaf),21,Node(Leaf,25,Leaf)))

let ex2 =
  Node(Node(Node(Leaf,"2",Leaf),"7",Leaf),
       "9",
       Node(Node(Leaf,"13",Leaf),"21",Node(Leaf,"25",Leaf)))


// ---- insert at index function ----

let rec insert i = function
    Leaf                ->  Node(Leaf,i,Leaf)
  | Node(t1,j,t2) as tr ->
      match compare i j with
      | 0           -> tr
      | n when n<0  -> Node(insert i t1 , j, t2)
      | _           -> Node(t1,j, insert i t2)

let t1 = Node(Leaf, 3, Node(Leaf, 5, Leaf))
let t2 = insert 4 t1


// ---- member of function ----

let rec memberOf  i = function
    Leaf          -> false
  | Node(t1,j,t2) ->
    match compare i j with
      | 0   -> true
      | n when n<0 -> memberOf i t1
      | _          -> memberOf i t2


// °°°°°°°°  TREE TRAVERSAL  °°°°°°°°
// °°°°°°°°  TREE TRAVERSAL  °°°°°°°°
// ---       PRODUCES A LIST

// ---  traverse tree - in order  ---
let rec inOrder = function
    | Leaf          -> []
    | Node(tl,x,tr) -> (inOrder tl) @ [x] @ (inOrder tr);;

// --- traverse tree in reverse order
let rec inRevOrder = function
    Leaf            -> []
  | Node(t1,j,t2)   -> inRevOrder t2 @ [j] @ inRevOrder t1

// ---  Pre and Post order works a bit differently

// Post-order traversal
// First traverse the left sub-tree in post-order, then traverse 
// the right sub-tree in post-order and finally visit the root node.
let rec postOrder = function
    | Leaf          -> []
    | Node(tl,x,tr) -> (postOrder tl) @ (postOrder tr) @ [x];;

//Pre-order traversal
//First visit the root node, then traverse the left sub-tree 
//in pre-order and finally traverse the right sub-tree in pre-order.
let rec preOrder = function
    | Leaf          -> []
    | Node(tl,x,tr) -> x :: (preOrder tl) @ (preOrder tr);;


let TestTree = Node(Node(Node(Leaf, 1, Leaf),2,Node(Leaf,3,Leaf)), 4, Node(Node(Leaf,5,Leaf), 6, Node(Leaf,7,Leaf)))
(*  TestTree representation
         4
      2     6
    1  3   5  7
*)
let ex3_1 = inOrder(TestTree)       //[1; 2; 3; 4; 5; 6; 7]
let ex3_2 = inRevOrder(TestTree)    //[7; 6; 5; 4; 3; 2; 1]
let ex3_3 = postOrder(TestTree)     //[1; 3; 2; 5; 7; 6; 4]
let ex3_4 = preOrder(TestTree)      //[4; 2; 1; 3; 6; 5; 7]

// --- FOLD BACK
// --- traverse the tree in REVERSE ORDER and foldBack
// --- produces a value (like an int)
let rec inFoldBack f t e =
  match t with
      Leaf          -> e
    | Node(t1,x,t2) -> let er = inFoldBack f t2 e
                       inFoldBack f t1 (f x er)

let ta = Node(Node(Node(Leaf,-3,Leaf),0,Node(Leaf,2,Leaf)),5,Node(Leaf,7,Leaf))

// examples 
let ex5 = List.foldBack (-) (inOrder ta) 0 // make tree into a list 
let ex6 = inFoldBack (-) ta 0
(* -3-(0-(2-(5-(7-0)))) *)


let rec postFoldBack f t e =
    match t with
    | Leaf          -> e
    | Node(tl,x,tr) ->
          let ex = f x e
          let er = postFoldBack f tr ex
          postFoldBack f tl er;;



// ---  delete minimum  ---
let rec delMin = function
  | Node(Leaf,i,t2) -> (i,t2)    
  | Node(t1,i,t2) -> let (m,t1') = delMin t1
                     (m, Node(t1',i,t2))

let rec delMin2 = function
    Leaf -> (None, Leaf)
  | Node(Leaf,i,t2) -> (Some i,t2)
  | Node(t1,i,t2) -> match delMin2 t1 with
                         (None,t) -> (None,t)
                       | (Some m,t1') -> (Some m, Node(t1',i,t2))


// ---  delete at index i  ---
let rec delete j = function
    Leaf          -> Leaf
  | Node(t1,i,t2) ->
       match compare i j with
         n when n<0 -> Node(t1,i,delete j t2)
       | n when n>0 -> Node(delete j t1,i,t2)
       | _          ->
            match t2 with
              Leaf -> t1
            | _  -> let (m,t2') = delMin t2
                    Node(t1,m,t2')



// ----  Count Nodes in tree

let rec countNodes tree =
  match tree with
    Leaf -> (1,0)
  | Node(treeL,_,treeR) ->
      let (ll,lr) = countNodes treeL in
      let (rl,rr) = countNodes treeR in
      (ll+rl,1+lr+rr);

/// ----  Add t at index x
let rec add x t =
    match t with
    | Leaf                   -> Node(Leaf,x,Leaf)
    | Node(tl,a,tr) when x<a -> Node(add x tl,a,tr)
    | Node(tl,a,tr) when x>a -> Node(tl,a,add x tr)
    | _                      -> t;;

// ----  Check if contains x
let rec contains x = function
    | Leaf                  -> false
    | Node(tl,a,_) when x<a -> contains x tl
    | Node(_,a,tr) when x>a -> contains x tr
    | _                     -> true;;
