let rec map f l=
    match l with
        | []->[]
        | h::ls->f h::map f ls

       // val map : f:('a -> 'b) -> l:'a list -> 'b list???

map (fun x->x*x) [1 .. 10];

map (fun x->match x%2 with | 0 ->(string)x+", pari" | _ -> (string)x+", dispari") [1 .. 10];;

map (fun (x,y)->"Dott. "+(string)x+" "+(string)y) [ ("Mario", "Rossi") ; ("Anna Maria", "Verdi") ; ("Giuseppe", "Di Gennaro")];;

#r "FsCheck.dll";;
open FsCheck;;

let prop_map f (ls : int list)=
    List.map f ls=map f ls;;

Check.Quick prop_map;;//????

let prop_map_pres_len f (ls :int list)=
    List.length(List.map f ls)=List.length(map f ls);;

Check.Quick prop_map_pres_len;;

let rec filter p l=
    match l with
        | []->[]
        | h::ls when p h->h::filter p ls
        | h::ls->filter p ls

let mult3 n=
    filter (fun x->x%3=0) [1 .. n];;

let rec filter1 p l=
    match l with
        | []->([],[])
        | h::ls when p h->
            let (x,y)=filter1 p ls
            (h::x,y)
        | h::ls->
            let (x,y)=filter1 p ls
            (x,h::y)

let mult3a n=
    filter1 (fun x->x%3=0) [1 .. n];;

let divisori n=
    filter (fun x->n%x=0) [1 .. n];;

let isPrime n=
    List.length(divisori n)=2;;

