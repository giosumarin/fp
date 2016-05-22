module Stack

exception EmptyStack

type Stack<'a>= S of 'a list

let empty= S [];;

let push elem stack=
    match stack with
        | S [] -> S [elem]
        | S l -> S (elem::l);;

let pop stack=
    match stack with
        | S [] -> raise EmptyStack
        | S (h::tl) -> (h, S tl);;

let top stack=
    match stack with
        | S [] -> raise EmptyStack
        | S (h::tl) -> h;;

let size stack=
    match stack with
        | S l->l.Length;;
