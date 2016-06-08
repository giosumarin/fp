#r "Stack.dll"
open Stack

exception CalcErr of int

type operator = Add | Prod | Minus;;
type token =
    | Op of operator
    | C  of int;;

let op f s=
    let (a, b) = pop s
    let (c, d) = pop b
    push (f c a)  d;;

let rpn exp=
    let rec rpn' exp s=
        match exp with
            | [] -> if (size s)=1 then top s else raise (CalcErr (size s))
            | h::tl ->
                match h with
                    | C x -> rpn' tl (push x s)
                    | Op Add-> rpn' tl (op (+) s) 
                    | Op Minus-> rpn' tl (op (-) s) 
                    | Op Prod-> rpn' tl (op ( * ) s) 
    in rpn' exp empty;;

rpn [ C 7 ; C 5 ;  Op Minus ] 
rpn [ C 10 ; C 3 ; C 2 ; Op Prod ; Op Add ]
rpn [ C 10 ; C 3 ; Op Add ; C 2 ; Op Prod  ]
rpn [ C 10 ; C 6 ; C 1 ; Op Minus ; Op Prod ; C 4 ; Op Minus ; C 2 ; C 5 ; Op Prod ;  Op Add ]

let perFold acc x=
    match x with
        | C n ->     push n acc  
        | Op Add->   (op ( + ) acc) 
        | Op Minus-> (op ( - ) acc) 
        | Op Prod->  (op ( * ) acc);;

let a exp=List.fold (fun acc x->perFold acc x) empty exp;;

let rpn exp= top (a exp);;
