type rel<'a, 'b when 'a : comparison and 'b : comparison > = Set<'a * 'b>;;

let perFold acc r2 (x,y) =
    let filtered=Set.fold (fun acc2 (a,b)->if(y=a) then (Set.add(a,b) acc2) else acc2) Set.empty r2
    let s1=Set.map (fun (a,b)->(x,b)) filtered
    Set.union s1 acc;;

let comp r1 r2=
    Set.fold (fun acc x-> perFold acc r2 x) Set.empty r1;;
 
let set1 = Set.add (1,"ciao") (Set.add (2,"hello") (Set.empty));;
let set2 = Set.add ("hello",3) (Set.add ("x",5) (Set.empty));;

comp set1 set2;;




