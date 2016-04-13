/////////////////////////////////LISTE-ES
let rec rmEven l=
    match l with    
        | [] -> []
        | h::tl when (h%2)=0 -> rmEven tl
        | h::tl -> h::rmEven tl;;

let rec rmOddPos l=
    match l with
        | [] -> []
        | h1::h2::tl -> h1::(rmOddPos tl)
        | h1::[] -> [h1];;

let rec split l=
    match l with
        | [] -> ([],[])
        | h1::h2::tl -> 
            let (a,b)=split tl
            (h1::a,h2::b)
        | h1::[] -> ([h1],[]);;

let rec cmpLength l1 l2=
    match l1,l2 with
        | [],[] -> 0
        | _,[] -> 1
        | [],_ -> -1
        | h1::tl1,h2::tl2 -> cmpLength tl1 tl2;;

let rec remove a l=
    match l with
        | [] -> []
        | h::tl when a=h -> (remove a tl)
        | h::tl -> h::(remove a tl);;

let rec rmDup l=
    match l with
        | [] -> []
        | h::tl -> 
            let l1=remove h tl
            h::rmDup l1;;
////////////////////////////////BINTREE
type 'a binTree= 
    | Null
    | Node of 'a * 'a binTree * 'a binTree;;

let rec intToFloatTree t=
    match t with
        | Null -> Null
        | Node(r,s,d)->Node((float)r, intToFloatTree s, intToFloatTree d);;

let rec inorder t=
    match t with    
        | Null -> []
        | Node(r,s,d) -> (inorder s)@(r::inorder d);;

let rec search x t=
    match t with 
        | Null -> false
        | Node(r,s,d) when x=r -> true
        | Node(r,s,d) -> (search x s)||(search x d);; 

let rec count t=
    match t with
        | Null -> (0,0)
        | Node(x, Null, Null) -> (1,1)
        | Node(x,s,d) ->
            let (a,b)=count s
            let (c,d)=count d
            (a+c+1,b+d);;

let rec depth p t=
    match t with
        | Null -> []
        | Node(r,_,_) when p=0 -> [r]
        | Node(r,s,d) -> (depth (p-1) s)@(depth (p-1) d);;

type fig=
    | Quad of float option
    | Rett of float option*float option;;

let area f=
    match f with
        | Quad  (Some x) -> "area= "+(string)(x*x)
        | Rett (Some x, Some y) -> "area= "+(string)(x*y)
        | _ -> "Errore";;

type valV={stud1:string;voto:int};;
type valG={stud2:string;giud:string};;

let valuta v=
    match v.voto with
        | x when x<18 -> {stud2=v.stud1; giud="insufficente"}
        | x when x<22 -> {stud2=v.stud1; giud="sufficente"}
        | x when x<27 -> {stud2=v.stud1; giud="buono"}
        | x -> {stud2=v.stud1; giud="ottimo"};;

let rec valList l=
    match l with
        | [] -> []
        | h::ls -> (valuta h)::valList ls;; 

let concat l=List.foldBack (fun x y -> x@y) l [];;
let filter p l=List.foldBack (fun a b->if(p a) then a::b else b) l [];;

let rec rB f l=
    match l with
        | [] -> failwith"errore"
        | [x] -> x
        | h::tl -> f h (rB f tl);;

let rec last l=rB (fun x y->y) l;

#r "FsCheck.dll";;
open FsCheck;;

let propRB f l=
    (List.length l>0) ==> lazy((rB f l)=(List.reduceBack f l));;

Check.Quick propRB;;

let rec scal l1 l2=
    match l1,l2 with
        | [],[] -> 0
        | h1::tl1,h2::tl2 -> h1*h2+(scal tl1 tl2)
        | _ -> failwith"lunghezze diverse";;

let rec scal2 l1 l2=List.foldBack2 (fun x y acc->x*y+acc) l1 l2 0;;

let propScal l1 l2=
    ((List.length l1)=(List.length l2)) ==> lazy((scal l1 l2)=(scal2 l1 l2));;

Check.Quick propScal;;

let rec tW f l=
    match l with    
        | [] -> []
        | h::tl when f h -> h::(tW f tl)
        | h::tl -> [];;
     
let rec dW f l=
    match l with
        | [] -> []
        | h::tl when f h-> dW f tl
        | h::tl -> h::tl;;

let safeDiv a b=
    match a,b with
        | _,Some(0) -> None
        | Some(x),Some(y) -> Some (x/y)
        | _ -> None;;

let optM f a b=
    match a,b with
        | Some(x),Some(y) -> Some(f x y)
        | _ ->  None;;

type f=
    | Cost of bool
    | Neg of f
    | And of f*f
    | Or of f*f;;

let rec eval a=
    match a with
        | Cost(x) -> x
        | Neg(x) -> not(eval x)
        | And(x,y) -> (eval x)&&(eval y)
        | Or(x,y) -> (eval x)||(eval y)

let rec toString  a=
    match a with
        | Cost(x) -> (string)x
        | Neg(x) -> "- "+toString x
        | And(x,y) -> "("+(toString x)+" /\ "+(toString y)+")"
        | Or(x,y) -> "("+(toString x)+" \/ "+(toString y)+")";;

//toString (And((Neg(Cost(true)),Cost(false))));;
//toString (Or((Neg(Cost(true)),(Neg(Cost(false))))));;