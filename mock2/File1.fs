module FSet

type FSet<'a when 'a : equality> = S of ('a -> bool);;

let empty=S(fun _->false);;
     

let contains a (S s)=s a;;

let singleton a = S(fun x->x=a);;

let add a (S s)=S(fun x->(x=a || s x));;
     
let union (S s1) (S s2)= S (fun x -> (s1 x || s2 x)) ;; 

let rec ofList l=
    match l with
        | [] -> empty 
        | h::tl -> add h (ofList tl);;

let rec toList s n=
    match n with
        | 0 -> if(contains 0 s) then [0] else []
        | x -> if(contains x s) then x::(toList s (x - 1)) else (toList s (x - 1));;

 