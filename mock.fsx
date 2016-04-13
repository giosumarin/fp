let rec dotProduct l1 l2=
    match l1,l2 with
        | [],[]->0
        | h1::ls1,h2::ls2->(h1*h2)+(dotProduct ls1 ls2)
        | _,_->failwith"lunghezza diversa";;

let dotProduct1 l1 l2=
    List.foldBack2 (fun x y a -> (x*y)+a) l1 l2 0

#r "FsCheck.dll"
open FsCheck

let propDot l1 l2=
    List.length l1=List.length l2 ==> lazy(dotProduct l1 l2=dotProduct1 l1 l2)

Check.Quick propDot;;

let rec takeWhile f l=
    match l with
        | []->[]
        | h::tl when f h-> h::(takeWhile f tl)
        | _ ->  [];;

 let rec dropWhile f l=
    match l with
        | []->[]
        | h::tl when f h -> dropWhile f tl
        | h::tl -> h::tl;; 

let propL f l=
    l=(takeWhile f l)@(dropWhile f l)

Check.Quick propL;;

let safeDiv a b=
    match a,b with
        | Some a, Some b -> Some (a/b)
        | (None,_)
        | (_,None)-> None;;

let optMapBinary f a b=
    match a,b with
        | Some a, Some b -> Some(f a b)
        | _ -> None;;

let optPlus a b=optMapBinary ( + ) a b;;

let optTimes a b=optMapBinary ( * ) a b;;

type Form= Const of bool | Neg of Form | And of (Form*Form) | Or of (Form*Form);;
 
let rec eval f=
    match f with
        | Const(x)->x
        | Neg(x)->not(eval x)
        | And(x, y)->(eval x)&&(eval y)
        | Or(x, y)->(eval x)||(eval y);;

eval (And(Const(true), Neg(Const(3=4))));;

let rec toString f=
    match f with
        | Const(x)->string x
        | Neg(x)->"- "+toString x
        | And(x, y)-> "("+(toString x)+" /\ "+(toString y)+")"
        | Or(x, y)-> "("+(toString x)+" \/ "+(toString y)+")"

let main f=
    (toString f)+" ritorna "+(string (eval f));;