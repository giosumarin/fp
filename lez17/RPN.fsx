#r "SimpleStack.dll"
open Stack;;

exception ErrorCalc;;

type operator = Add | Prod | Minus;;

type token =
  | Op of operator
  | C  of int;;

let op f stack=
    let (a, stack)=pop stack
    let (b, stack)=pop stack
    push (f b a) stack;;


let rpn list=
    let s:Stack<int>=empty;
    let rec aux list s=
        match list with
            | [] -> 
                if (size s)<>1 then raise ErrorCalc
                let (a,b)=pop s
                a
            | h::tl ->
                match h with
                    | C x -> aux tl (push x s)
                    | Op Minus -> aux tl (op ( - ) s)
                    | Op Add -> aux tl (op ( + ) s)
                    | Op Prod -> aux tl (op ( * ) s)
    aux list s;;


let rpn1 = [ C 7 ; C 5 ;  Op Minus ] ;;
let rpn2 = [ C 10 ; C 3 ; C 2 ; Op Prod ; Op Add ];;
let rpn3 = [ C 10 ; C 3 ; Op Add ; C 2 ; Op Prod  ]
let rpn4 = [ C 10 ; C 6 ; C 1 ; Op Minus ; Op Prod ; C 4 ; Op Minus ; C 2 ; C 5 ; Op Prod ;  Op Add ]
let rpn5 = [ C 1; C 2];;

rpn rpn1;;
rpn rpn2;;
rpn rpn3;;
rpn rpn4;;
rpn rpn5;;