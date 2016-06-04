module  FSet

// we use binary search trees as an alternative implementation of sets

type 'a FSet = Leaf | Node of 'a * 'a FSet * 'a FSet

// First, a technicality
let (.<) small big = 
      match (Unchecked.compare small big) with
      | -1 ->  true 
      | _ ->   false;;

(* why do we redefine comparison? In general this is *not* a good idea,
as it will allow to compare things that we should not
compare such as functions. However, here we would need 'a to be both 

'a : comparison and 
'a : equality

for this implementation, and this mean multiple inheritance 

So we suppress the static check using Uncheched.equals
*)


let empty =   Leaf

let isEmpty t = (t = Leaf)

let rec contains  x  btree =
    match btree with
        |  Leaf -> false
        |  Node (r, left, right) ->
            ( x = r ) || 
            ( (x .< r) && contains  x left ) ||
            ( not (x .< r) && contains  x right );;  

let rec add  x  btree  =
    match btree with
        | Leaf -> Node(x, Leaf, Leaf)  
        | Node(r, left, right) when x = r ->  btree 
        | Node(r, left, right) when x .< r ->  Node(r,  (add  x left) , right )
        | Node(r, left, right)  ->  Node(r , left, (add x  right) ) 

let ofList list = List.fold (fun t x -> add x t ) Leaf list

// foldback :  f_node:('a -> 'b -> 'b -> 'b) -> tree:'a FSet -> f_leaf:'b -> 'b
let rec fold_treeBack f_node tree f_leaf  = 
  match tree with
    | Leaf -> f_leaf
    | Node (x, left, right) -> f_node x (fold_treeBack f_node left f_leaf ) (fold_treeBack f_node right f_leaf )

let rec foldB f l acc=
    match l with
    | []->acc
    | h::ls-> f h (foldB f ls acc)

let rec fold f l acc=
    match l with
        | []->acc
        | h::ls -> fold f ls (f acc h);;

List.fold (-) 0 [1 .. 3];; 


let rec foldTree f a t=
    match t with
        | Leaf->a
        | Node(x,l,r)->foldTree f (f a x (foldTree f a l)) r;; 



foldT (fun a b c->a+b+c) 0 (Node(1,Node(2,Leaf,Leaf),Node(4,Leaf,Leaf)));;

let rec union  s1 s2 = 
    match s1 with
     | Leaf -> s2
     | Node(x,ltr,rtr) -> let ts = add x s2
                          let tsl = union  ltr ts
                          union  rtr tsl;;


let foldT f a t =
    let rec foldT' f a t k =
        match t with
        | Leaf -> k a
        | Node(x,r,l) -> foldT' f a r ( fun res -> foldT' f a l ( fun res1 -> k (f a res res1)))
    in foldT' f a t id;;


// preorder here
let toList tree  = fold_treeBack (fun x l r -> x :: l @ r) tree [] 

let count tree= fold_treeBack (fun _ l r -> l+r+1) tree 0;;

let map f tree= ofList (List.map f (toList tree));;

let isSubset t1 t2=List.forall (fun x -> contains x t2) (toList t1);; 




(* not quite a fold on sets, type too specific
let rec foldBackr f tree seed =
  match tree with
    | Leaf -> seed
    | Node(x,le,ri) ->
      let fle = f x (foldBackr f le seed)
      let fre = f x (foldBackr f ri seed)
      f fle fre

    *)


