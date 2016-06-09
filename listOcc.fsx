
let es=['a';'a';'a';'b';'a';'a'];;


let y l=
    let rec y' l elem=
        match elem with
            | []->[]
            | h::tl->(h,(List.foldBack (fun x acc->if(x=h)then acc+1 else acc) l 0))::(y' l tl)
    y' l (Set.toList (Set.ofList l));;


let x l=
    let rec x' l last acc=
        match l with
            | []-> [(last,acc)]
            | h::tl when h=last -> x' tl h (acc +  1)
            | h::tl -> (last,acc)::x' tl h 1
    in x' (List.tail l) (List.head l) 1;;

let perFold (x,occ) acc=
    let rec aux (x,occ)=
        match occ with
            | 0->[]
            | n->x::aux (x,(occ - 1))
    aux(x,occ)::acc;;

let xConv lc=List.foldBack (perFold) lc [];;


let divide l =
    let rec divide l acc =
        match l with
        | x::y::tl when x = y -> divide (y::tl) (x::acc)
        | x::y::tl -> (x::acc,y::tl) 
        | [x] -> ([x],[])
        | [] -> ([],[]) 
    in divide l [];;

let rec count l =
    match divide l with
    | (x,[]) -> [x];
    | (x,y) -> x::(count y);;

