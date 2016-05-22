let concat l=List.foldBack (@) l [];;
concat [[1 .. 5];[6 .. 10]]

let filter p l=List.foldBack(fun x y-> if (p x) then x::y else y) l [];
filter (fun x->x>5) [1 .. 10];

let rec reduceB f l=
    match l with
        | []->failwith"woo"
        | [x]->x
        | h::ls->f h (reduceB f ls)

let last l=reduceB (fun x y->y) l
last [1 .. 10]

#r "FsCheck.dll"
open FsCheck

let prop_reduce f l=
    if (List.length l=0) then true else reduceB f l=List.reduceBack f l

Check.Quick prop_reduce

let reduceBF f l=List.foldBack f (l |> List.rev |> List.tail |> List.rev) (l |> List.rev |> List.head)

let last1 l=reduceBF (fun x y->y) l
last1 [1 .. 10]


let prop_reduce1 f l=
    if (List.length l=0) then true else reduceBF f l=List.reduceBack f l

Check.Quick prop_reduce