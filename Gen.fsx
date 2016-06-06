#r "FsCheck.dll";;

open FsCheck;;

let rec isOrdered ls=
    match ls with
        | [] | [_] -> true
        | h1::h2::tl -> if h1 <= h2 then isOrdered (h2::tl) else false;;

let rec insert x l=
    match l with
        | [] -> [x]
        | h::tl -> if x < h then x::h::tl else h::(insert x tl);;



let rec remove x l=
    match l with
        | [] -> []
        | h::tl -> if x=h then (remove x tl) else h::(remove x tl);;



let genSet genL n m=
    Arb.mapFilter (List.foldBack (fun x acc->remove x acc) []) (fun x->isOrdered x && List.length x>=m && List.length x<=n) genL;;


(genSet Arb.from<int list> 3 5 ) |> Arb.toGen |> Gen.sample 50 5;;

let genZip g=
    Arb.mapFilter id (fun (x,y)->List.length x=List.length y) g;;

 
genZip Arb.from<int list * int list> |> Arb.toGen |> Gen.sample 50 2;;

let propZip x=
    Prop.forAll (genZip (Arb.from<int list * int list>)) (fun (x,y)->(x,y)=List.unzip(List.zip x y) );

Check.Quick propZip;;

type 'a Tree=
    | Node of 'a * 'a Tree * 'a Tree
    | Leaf;;

Arb.from<int Tree> |> Arb.toGen |> Gen.sample 50 2;;

type Color= | Blue | Red | Black;;
type Block = | Jolly | C of Color;;

//generatore con probabilità 1 di generare joker

let isPositive l=
    List.foldBack (fun x acc-> (x>=0)&&acc) l true;;
   // List.forall (f)

let sortDup l=
    (List.foldBack (fun x acc->x::remove x acc) l l) |> List.sort;; 

sortDup [6;6;1;1;1]

let genPos genL=
    Arb.mapFilter sortDup (fun x->isPositive x && List.length x> 3) genL;;

genPos Arb.from<int list> |> Arb.toGen |> Gen.sample 50 10;;

let freq = Gen.frequency [(6,Gen.map(fun _ -> Jolly) Arb.generate<int>);(4,Gen.map(fun x -> C x ) Arb.generate<Color>)]

Gen.sample 5 10 freq

Arb.generate<Block> |> Gen.sample 5 5


Gen.frequency [(6,Arb.generate<Color>)]

 