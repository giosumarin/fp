type t = INT | LSTINT;;

type exp =
  K of int
  | Plus of exp * exp
  | Nil
  | Cons of exp * exp
  | Hd of exp
  | Tl of exp;;

let rec tc e=
    match e with
        | K (_) -> Some INT
        | Plus(e1, e2) -> 
            match (tc e1, tc e2) with
                | (Some INT, Some INT) -> Some INT
                | _ -> None
        | Nil -> Some LSTINT
        | Cons(e1, e2) ->
            match (tc e1, tc e2) with
                | (Some INT, Some LSTINT) -> Some LSTINT
                | _ -> None
        | Hd (e)  ->
            match (tc e) with
                | Some LSTINT -> Some INT
                | _ -> None
        | Tl (e) ->
             match (tc e) with
                | Some LSTINT -> Some LSTINT
                | _ -> None;;



#r "FsCheck"
open FsCheck

let test size len =
  let exps = (Gen.sample size len Arb.generate<exp>)   //size prof, len num esempi
  List.map2 (fun x y -> printf "%A has type %A\n" x y) exps (List.map tc exps)

do test 10 5;;



let test1 size len =
  let exps = (Gen.sample size len Arb.generate<exp>)   //size prof, len num esempi
  List.iter2 (fun x y -> match y with 
                            | None -> printf "%A non tipabile\n" x
                            | Some h -> printf "%A has type %A\n" x h
                            ) exps (List.map tc exps);;

do test1 10 5;;

exception OpERR of (exp * t) ;;
exception HdERR of (exp * t) ;;
exception TlERR of (exp * t) ;;

let rec tce e=
    match e with
        | K (_) -> INT
        | Plus(e1, e2) -> 
            match (tce e1, tce e2) with
                | (INT, INT) -> INT
                | _ -> raise (OpERR (Plus(e1, e2), INT))
        | Nil -> LSTINT
        | Cons(e1, e2) ->
            match (tce e1, tce e2) with
                | (INT, LSTINT) -> LSTINT
                | (INT,_)-> raise (OpERR(Cons(e1, e2), LSTINT))
                | (_,_)->raise (OpERR(Cons(e1, e2), INT))
        | Hd (e)  ->
            match (tce e) with
                | LSTINT -> INT
                | _ -> raise (HdERR( Hd (e), LSTINT))
        | Tl (e) ->
             match (tce e) with
                | LSTINT -> LSTINT
                | _ -> raise (TlERR( Hd (e), LSTINT));;
(*
let main exp=
    try 
        printf "%A has type %A" exp (tce exp)
    with
        | OpERR (a,t)->"expected types %A";;
*)

let rec value exp=
    match exp with 
        | K (_) -> true
        | Nil -> true
        | Cons(e1, e2) -> value e1 && value e2
        | _ -> false;;

type t1=N of int | L of int list

let rec eval exp=
    match exp with
        | K n-> N n
        | Plus(e1, e2) -> 
            match (eval e1, eval e2) with
                | (N e1, N e2)-> N (e1+e2)
                | _ -> failwith"caio"
        | Nil -> L []
        | Cons (e1, e2) ->
            match (eval e1, eval e2) with
                | (N e1, L e2)-> L (e1::e2)
                | _ -> failwith"caio "
        | Hd e ->  
            match (eval e) with
                | L e-> N (List.head e)
                | _ -> failwith"caio "
        | Tl e ->  
            match (eval e) with
                | L e-> L (List.tail e)
                | _ -> failwith"caio ";;

