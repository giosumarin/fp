module Tree

type Tree<'a when 'a:comparison>=
    Node of 'a * 'a Tree * 'a Tree | Leaf;;

let empty=Leaf;;

let add x t=
    let rec add' x t k=
        match t with
            | Leaf -> k (Node(x, Leaf, Leaf))
            | Node(a, s, d) when x<a -> add' x s (fun res -> k(Node(a,res,d))) 
            | Node(a, s, d) -> add' x d (fun res -> k(Node(a,s,res)))
    in add' x t id;;

let count t=
    let rec count' t k=
        match t with
            | Leaf -> k 0
            | Node(a, s, d) -> count' s (fun r1 -> count' d (fun r2->k (1+r1+r2)))
    in count' t id;;

let inorder t=
    let rec inorder' t k=
        match t with
            | Leaf -> k []
            | Node(a, s, d) -> inorder' s (fun r1 -> inorder' d (fun r2-> k (r1@(a::r2))))
    in inorder' t id;;

let ofList l=
    let rec ofList' l k=
        match l with
            | [] -> k empty
            | h::tl-> ofList' tl (fun r ->k (add h r))
    in ofList' (List.rev l) id;;          