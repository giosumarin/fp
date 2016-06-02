///////////////////// CRIVELLO DI ERATOSTENE

let nat=Seq.initInfinite id;

let sift n s=Seq.filter (fun x -> x%n<>0) s;;

sift 2 nat |> Seq.take 10 |> Seq.toList;;
sift 3 nat |> Seq.take 10 |> Seq.toList;;

let rec sieve s=
    seq{
        let head=Seq.head s
        yield head
        yield! sieve (sift head (Seq.tail s)) 
    }

let nat2=Seq.initInfinite (fun x->x+2);;

sieve nat2 |> Seq.take 10 |> Seq.toList;;

///////////////////// DIRECTORY

open System.IO;;

Directory.GetFiles "/";;
Directory.GetDirectories "/";;

let allFiles s=seq{
    yield! (Directory.GetFiles s)
    yield! Seq.collect (Directory.GetFiles) (Directory.GetDirectories s)
}


allFiles "/Dev-Cpp" |> Seq.take 30 |> Seq.toList;;

///////////////////// SEQ OF SUM OF SEQ




