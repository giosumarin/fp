module FarlocNum

exception Negative;;

type FN =
    | C of FN
    | Zero;;

let rec add fn1 fn2=
    match fn1 with
        | Zero -> fn2
        | C x -> C (add x fn2);;

let rec (.<) fn1 fn2=
    match (fn1, fn2) with
        | Zero, C x -> true
        | _ -> false;;

let sub fn1 fn2=
    if (fn1 .< fn2) then raise Negative 
    let rec sub' fn1 fn2=
        match (fn1, fn2) with
            | Zero, Zero -> Zero
            | C x, Zero -> C x
            | C x, C y -> sub' x y
            | _ -> raise Negative 
    in sub' fn1 fn2;;

let rec toFarloc n=
    match n with
        | 0-> Zero
        | n when n>0 -> C (toFarloc (n - 1))
        | _ -> raise Negative;;

let rec fromFarloc fn=
    match fn with
        | Zero -> 0
        | C x -> 1+(fromFarloc x);;

let rec mult fn1 fn2 =
    match (fn1, fn2) with
        | _, Zero -> Zero
        | fn1, C Zero->fn1
        | fn1, C n2->mult (add fn1 fn1) n2;;

let div fn1 fn2 =
    let rec div' fn1 fn2 acc=
        match (fn1, fn2) with
            | fn1, fn2 when fn1 .< fn2 -> acc
            | fn1, fn2 -> div' (sub fn1 fn2) fn2 (C acc)
    in div' fn1 fn2 Zero;; 
        


