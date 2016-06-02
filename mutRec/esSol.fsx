//////////////////////// file system

type FileSys = Element list
and Element  = | File of string
               | Dir of string * FileSys;;

let rec namesFileSys fileSys =
    match fileSys with
        | [] -> []
        | h::tl -> (namesElement h)@(namesFileSys tl)    
and namesElement el =
    match el with
        | File s -> [s]
        | Dir (s, f) -> s::(namesFileSys f)

//////////////////////// es

let isuml l=
    let rec aux l acc=
        match l, acc with
            | [], acc -> acc
            | h::tl, acc ->aux tl (acc + h)
    aux l 0;;
    

