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

let nat1=Seq.initInfinite (fun x->(float) x);

let sumSeq s:seq<float>=
    let rec aux s (acc:float)=
        seq{
            let acc2=(Seq.head s) + acc
            yield acc2
            yield! (aux (Seq.tail s) acc2)   
       }
    aux s 0.;;

sumSeq nat1 |> Seq.take 15 |> Seq.toList;;

///////////////////// TAYLOR

let rec t (x:float) (k:int)=
    match k with
        | x when x=0 -> float 1
        | k -> (t x (k-1)) * (x / (float k)) ;;

t 3. 2;;

   
let gent x=Seq.initInfinite (t x);;

gent 3. |> Seq.take 3 |> Seq.toList;;

let apprTaylor x=sumSeq (Seq.initInfinite (t x));;

apprTaylor 1.0;;

///////////////////// LAZY PROD

(fun ()->3) ();;

let rec lazyProd a b=
    let x=a ()
    match x with
        | 0-> (fun () -> 0)
        | _->
            let c= a()
            let d= b()
            fun () -> c*d;;
