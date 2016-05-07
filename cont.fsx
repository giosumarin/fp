let length l=
    let rec lA l acc=
        match l with
            | [] -> acc
            | h::tl -> lA tl (acc+1)
        in lA l 0;;

let length2 l=List.foldBack (fun x y->y+1) l 0;;

let length3 l=
    let rec aL l f=
        match l with
            | []->f 0
            | h::tl-> aL tl (fun res -> f (res+1))
    in aL l (fun x -> x);;

let map f l=
    let rec map1 f acc=function
        | [] -> acc
        | h::tl -> map1 f ((f h)::acc) tl
    in map1 f [] l;;


let map2 f l=
    let rec map3 f f1=function
        | [] -> f1 []
        | h::tl -> map3 f (fun res -> f1 ((f h)::res)) tl
    in map3 f (fun x->x) l;; 

type 'a Tree= Null | Node of 'a * ('a Tree) * ('a Tree);;
(*
let preorder t=
    let pA acc=function
        | Null -> acc
        | Node(r,s,d)->acc
*)

let id x=x;;
let rec preorder t f=
    match t with
        | Null->f []
        | Node(r,s,d) -> preorder s (fun resS -> preorder d (fun resR -> f(r::resS@resR))) ;;  

preorder (Node(1, (Node(2, Null, Null)), (Node( 3 ,Null, Null)))) id;;


let rec inorder t f=
    match t with
        | Null->f []
        | Node(r,s,d) -> inorder s (fun resS -> inorder d (fun resR -> f(resS@r::resR))) ;;  


inorder (Node(1, (Node(2, Null, Null)), (Node( 3 ,Null, Null)))) id;;

let rec postorder t f=
    match t with
        | Null->f []
        | Node(r,s,d) -> postorder s (fun resS -> postorder d (fun resR -> f(resS@resR@[r]))) ;;  


postorder (Node(1, (Node(2, Null, Null)), (Node( 3 ,Null, Null)))) id;;
