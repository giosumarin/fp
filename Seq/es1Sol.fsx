/////////////////////// MAP

let rec map f s=
    seq{
        yield f (Seq.head s)
        yield! map f (Seq.tail s)
    }

let nat=Seq.initInfinite id;;

let square= map (fun n->n*n) nat;;

square |> Seq.take 20 |> Seq.toList;;

let rec map2 f s=
    seq{
        if not(Seq.isEmpty s) then
            yield f (Seq.head s)
            yield! map f (Seq.tail s)
    }

let square1= map (fun n->n*n) nat;;

square1 |> Seq.take 20 |> Seq.toList;;

/////////////////////// FILTER

let rec filter f s=
    seq{
        let head=Seq.head s
        if(f head) then yield head
        yield! filter f (Seq.tail s)
    }

let triple=filter (fun x->x%3=0) nat;;

triple |> Seq.take 20 |> Seq.toList;;

/////////////////////// FIBO

let rec fibFrom a b=
    seq{
        yield a
        yield! fibFrom b (a + b)
    };;

fibFrom 0 1 |> Seq.take 10 |> Seq.toList;;

let fib n=Seq.item n (Seq.tail (fibFrom 0 1));;

fib 0 ;;   // 1
fib 1 ;;   // 1
fib 2 ;;   // 2
fib 3 ;;   // 3
fib 4 ;;   // 5
fib 10 ;;  // 89
