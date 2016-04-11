#r "FsCheck";;
open FsCheck;;
    
type 'a binTree =
    | Null    // empty tree
    | Node of 'a  * 'a binTree * 'a binTree ;;   // Node(root, left, right)

let t =  Node( 1 , Node (2, Null , Node(4,Null,Null)) ,  Node(3, Null,Null)  );;

let t2 = Node (2, Null, Node ( 4 , Null, Null ) );; 
let t7 = Node (7, Null, Node (10, Null, Node ( 13 , Null, Null ))) ;; 
let t8 = Node ( 8, Node ( 11, Null, Null), Null ) ;; 
let t5 = Node ( 5, t7, t8 ) ;;
let t9 = Node ( 9, Null,   Node (12, Null, Null) );
let t6 = Node( 6, t9, Null) ;;
let t3 = Node(3, t5, t6 ) ;;
let t1 = Node (1, t2, t3 );;

let rec intToFloatTree t =  
    match t with
        | Null -> Null
        | Node(x, t1, t2) -> Node((float x),intToFloatTree t1, intToFloatTree t2)

let bt7 = intToFloatTree t7 ;;

let rec inorderToList t =
    match t with
        | Null -> []
        | Node(a, s, d) -> inorderToList s@a::inorderToList d

let t1inord = inorderToList t1 ;;


let rec preorderToList t =
    match t with
        | Null -> []
        | Node(a, s, d) ->  (a::preorderToList s)@preorderToList s

let t1pre = preorderToList t1 ;;

let prop_visit (t:int binTree)= //controllare anche l'inverso
    let x1=inorderToList t
    let x2=preorderToList t
    let rec visit l= 
        match l with
            | [] -> true;
            | h::ls when List.contains h x2 -> visit ls
            | _ -> false
    visit x1;;

do Check.Quick prop_visit;;

let rec search (x,t)=
    match t with
        | Null -> false
        | Node(h,s,d) when h=x -> true
        | Node(h,s,d) -> search (x,s) || search (x,d);;

search(2,t1) ;; // true
search(3,t1) ;; // true
search(4,t1) ;; // true
search(5,t1) ;; // true
search(100,t1) ;; // false
//RIGA 370
        
let rec mem (x , ls) =  
    match ls with 
    | [] -> false 
    | y::ys -> x=y || mem (x, ys) ;;

let prop_search ( x , btree : int binTree) =
  let l1 = inorderToList btree
  mem(x,l1) ==> search(x,btree) ;;  // se x appartiene a l1, valuta  search(x,btree)

Check.Verbose prop_search ;;

let filterToList pred btree=
    let l1=inorderToList btree
    let rec filter l=
        match l with
            | [] ->[]
            | h::tl when pred h -> h::filter tl
            | h::tl -> filter tl
    filter l1;;

let isEven x= (x%2=0) ;;

let isSmall x= (x<5);;

let t1even = filterToList isEven t1 ;;

let t1small = filterToList isSmall t1 ;;

//RIGA 431

let rec count t=
    match t with
        | Null -> (0,0)
        | Node (x, Null, Null) -> (1,1)
        | Node (x, d, s) -> 
            let (a, b) = count d
            let (c, d) = count s
            (a+c+1,b+d)

let rec depthToList(p,t)=
    match t with
        | Null -> []
        | Node (x, s, d) when p=0 -> [x]
        | Node (x, s, d) -> depthToList(p-1,d)@depthToList(p-1,s)


type direction = L | R ;; // Left, Right

let rec getElement ((l:direction list), t)=
    match l, t with
        | [], Node(x,_,_) -> Some x
        | h::tl, Node (x, s, d) when h=L -> getElement(tl, s)
        | h::tl, Node (x, s, d) when h=R -> getElement(tl, d)
        | _,_ -> None

//----------------------------  PARTE 2  ----------------------------//

let rec insert (x, t)=
    match t with
        | Null -> Node (x, Null, Null)
        | Node(a, s, d) when x<a -> Node(a, insert(x, s),d)
        | Node(a, s, d) when x>a -> Node(a, s, insert(x, d))
        | Node(a, s, d) when x=a -> Node(a, s, d)
        | _ -> Null //?????????????

let rec insertFromList (l, t) =
    match l with
        | [] -> t
        | h::tl -> insertFromList(tl,insert(h,t))

//let intList = [ 20 ; 10 ; 60 ; 15 ; 40 ; 100 ; 30 ; 50 ; 70 ; 35 ; 42 ; 58 ; 75 ; 32 ; 37 ] ;;
//let intTree=insertFromList(intList, Null);;

//let strList1 = [ "pesca" ; "banana" ; "uva" ; "albicocca" ; "nocciola" ; "ribes" ] ;;
//let strTree1=insertFromList(strList1,Null);;

//--------------------------RIGA 656
let rec search1 (c, t)=
    match t with
        | Null -> false
        | Node(x, s, d) when x = c -> true
        | Node(x, s, d) when x > c -> search1(c, s)
        | Node(x, s, d) -> search1(c, d)

//search("ribes", strTree1);

let searchPath (c, t)=
    let rec sp t=
        match t with
            | Null -> []
            | Node(x, s, d) when x = c -> [x]
            | Node(x, s, d) when x > c -> x::sp s
            | Node(x, s, d) -> x::sp d
    let a= sp t
    let rv=List.rev a
    match rv with
        | h::l when h=c->a
        | _ -> []

let rec min t=
    match t with
        | Null -> None
        | Node(x,Null,_) -> Some x
        | Node(x,s,_) -> min s
//min tr;; 
let rec subtree (r, t)=
    match t with
        | Null -> Null
        | Node(x,s,d) when x=r -> Node (x,s,d)
        | Node(x,s,d) when x<r -> subtree (r, d)
        | Node(x,s,d) when x>r -> subtree (r, s)
        | _ -> Null;

let strList1 = [ "pesca" ; "banana" ; "uva" ; "albicocca" ; "nocciola" ; "ribes" ] ;;
let strList2 = [ "limone" ; "ciliegia" ; "mela" ; "pera" ; "noce"  ] ;;

let strTree1=insertFromList(strList1,Null);;
let strTree2=insertFromList(strList2,strTree1);;

let m7 = min ( ( subtree ("limone",  strTree2) ) ) ;;  //  Some "ciliegia"
let m8 = min ( ( subtree ("ribes",  strTree2) ) )  ;;  // Some "ribes"
