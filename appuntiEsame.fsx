type FileSys = Element list and Element  = | File of string | Dir of string * FileSys
let rec namesFileSys fileSys =  match fileSys with | []    -> [] | el::els -> (namesElement el) @ (namesFileSys els)
and namesElement el =match el with | File s    -> [s] | Dir(s,fileSys) -> s :: (namesFileSys fileSys)

let add x t=let rec add' x t k=match t with | Leaf -> k (Node(x, Leaf, Leaf)) 
            | Node(a, s, d) when x<a -> add' x s (fun res -> k(Node(a,res,d))) 
            | Node(a, s, d) -> add' x d (fun res -> k(Node(a,s,res))) in add' x t id
let count t=let rec count' t k=match t with| Leaf -> k 0
            | Node(a, s, d) -> count' s (fun r1 -> count' d (fun r2->k (1+r1+r2))) in count' t id
let inorder t=let rec inorder' t k=match t with| Leaf -> k []
            | Node(a, s, d) -> inorder' s (fun r1 -> inorder' d (fun r2-> k (r1@(a::r2))))in inorder' t id

let fatt n=let rec fatt' n k=match n with | 0 | 1  -> k 1 | n -> fatt' (n - 1) (fun r->k (n * r)) in fatt' n id
let rec fibFrom a b= seq{yield a ; yield! fibFrom b (a + b)}

let genZip g=Arb.mapFilter id (fun (x,y)->List.length x=List.length y) g
genZip Arb.from<int list * int list> |> Arb.toGen |> Gen.sample 50 2;;
let propZip x=Prop.forAll (genZip (Arb.from<int list * int list>)) (fun (x,y)->(x,y)=List.unzip(List.zip x y) )
let isPositive l=List.forall (fun x -> x>0) l
let genPos genL=Arb.mapFilter List.sort (fun x->isPositive x && List.length x> 3) genL
let freq = Gen.frequency [(6,Gen.map(fun _ -> Jolly) Arb.generate<int>);(4,Gen.map(fun x -> C x ) Arb.generate<Color>)]
Arb.generate<Block> |> Gen.sample 5 5 == Arb.from<Block> |> Arb.toGen |> Gen.sample 5 5;;