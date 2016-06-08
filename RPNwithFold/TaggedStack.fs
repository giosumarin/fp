module Stack

exception EmptyStack;;

type Stack<'a>= | Empty | S of 'a*('a Stack);;

let empty=Empty;;

let push el s=S (el,s);;

let pop s=
    match s with
        | Empty -> raise EmptyStack
        | S (el, s)->(el, s);; 

let top s=let (a, b)=pop s in a;;

let rec size=function
    | Empty->0
    | S (el, s)->1+(size s);;


