type boolex = Const of bool | Var of char | Neg of boolex | And of boolex *  boolex;;

type environment = char list;;

let rec eval b e =
    match b with
        | Const x -> x
        | Var x -> List.exists (fun y->x=y) e 
        | Neg x -> not (eval x e)
        | And (x, y) -> (eval x e)&&(eval y e);;

type ifex= K of bool | X of char | If of ifex*ifex*ifex;;

let ie=If ( K(3 = 4), K(1<2) , If (K(true), X 'x',X 'y') );;

let rec ifeval i e =
    match i with
        | K x -> x
        | X x -> List.exists (fun y->x=y) e 
        | If (a, b, c) -> if (ifeval a e) then (ifeval b e) else (ifeval c e);;

let rec bool2if b=
    match b with
        | Const x -> K x
        | Var x -> X x
        | Neg x -> If(bool2if x, K false, K true)
        | And (x, y) -> If(bool2if x, bool2if y, K false);;

#r "FsCheck.dll";;
open FsCheck;;

let propIf b e=
    (eval b e)=ifeval(bool2if b) e;;

Check.Quick propIf;;

let nat =Seq.initInfinite (fun x->x);;
let seq1=seq{
    yield! [0 ; 1 ; 2 ; 0 ; 3 ; 4 ; 4 ;  3 ; 1]
    yield! Seq.initInfinite(fun x->x+5)
};;

let seq2=
    let rec aux n=seq{
        yield n
        yield n
        yield! aux (n+1)
    }
    aux 0;;

let seq3=
    let rec aux n=seq{
        let s=Seq.initInfinite (fun x->x)
        yield! Seq.take n s
        yield! aux (n + 1)
    }
    aux 2;;

let rec duplicate s=seq{
    let f=Seq.head s
    yield f   
    yield! duplicate (Seq.filter (fun x -> x<>f) s)
}