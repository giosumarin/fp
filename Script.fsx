let sift n sq=
    Seq.filter (fun x -> x%n<>0) sq;;

let rec sift1 n sq=seq{
    if ((Seq.head sq) % n = 0) then
        yield (Seq.head sq)
    yield! sift1 n (Seq.tail sq)
}

sift 3 (List.toSeq [1 .. 10]);;

let rec genNat n=seq{
    yield n
    yield! genNat (n + 1)
}

Seq.take 10 nat |> Seq.toList;;

let rec sieve sq=seq{
    let x0=Seq.head sq
    yield x0
    let sq1=sift x0 (Seq.tail sq)
    yield! sieve sq1
}
let sq1=Seq.skip 2 nat;;
let x=sieve sq1;;
sq1 |> Seq.toList;;

sieve (Seq.skip 2 (Seq.initInfinite id)) |> Seq.take 200 |> Seq.toList;;

let siftC a sq = Seq.cache( sift a sq );;

let rec sieve1 sq=seq{
    let x0=Seq.head sq
    yield x0
    let sq1=Seq.cache (sift x0 (Seq.tail sq))
    yield! sieve sq1
}

sieve1 (Seq.skip 2 (Seq.initInfinite id)) |> Seq.take 500 |> Seq.toList;;

open System.IO ;;

let rec allFiles path=
    seq{
        yield! Directory.GetFiles path
        yield! Directory.GetDirectories path |> Seq.collect allFiles
    }

allFiles "/home/gio/Scrivania" |> Seq.take 10 |> Seq.toList

let sum sq=
    seq{
        yield ((Seq.item 0 sq) + (Seq.item 1 sq))
        yield! Seq.skip 2 sq
    }


let nat=Seq.initInfinite id

let rec sumSeq sq f=
    seq{
        let first=(Seq.head sq)+f
        yield first
        yield! sumSeq (Seq.tail sq) first  
    }

