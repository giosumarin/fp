
let nat=Seq.initInfinite id;;

let seq1=seq{
    yield! [0 ; 1 ; 2 ; 0 ; 3 ; 4 ; 4 ;  3 ; 1 ;]
    yield! Seq.initInfinite (fun x->x+5)
};;

let seq2=Seq.collect (fun x->seq{yield! [x;x]}) nat;; 

let seq3=Seq.collect (fun x->seq{yield! [0..x]}) (Seq.initInfinite (fun x->x+1));;

let distinct s=
    let rec distinct' s acc=seq{
        let h=Seq.head s
        match List.contains h acc with
        | true->yield! (distinct' (Seq.tail s) acc)
        | _ -> 
            yield h
            yield! (distinct' (Seq.tail s) (h::acc))
    } 
    distinct' s [];;

distinct seq1 |> Seq.take 500 |> Seq.toList;;


let distinct1 s=
    let rec distinct' s acc=seq{
        let h=Seq.head s
        if Seq.contains h acc then yield! (distinct' (Seq.tail s) acc)
        else yield h ; yield! (distinct' (Seq.tail s) (seq{yield h; yield! acc}))
    } 
    distinct' s Seq.empty;;

let isEqual n s1 s2= (Seq.toList (Seq.take n s1)=Seq.toList(Seq.take n s2));;

isEqual 20 nat (distinct seq1);;




