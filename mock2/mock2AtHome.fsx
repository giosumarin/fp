

type boolex = 
    | Const of bool 
    | Var of char 
    | Neg of boolex 
    | And of boolex *  boolex;;

type environment = char list;;

let rec eval exp (env:environment)=
    match exp with
        | Const x -> x
        | Var x -> List.exists (fun y->x=y) env
        | Neg x -> not(eval x env)
        | And (x, y)->(eval x env)&&(eval y env);;

type ifex= B of bool | V of char | If of ifex * ifex * ifex;;

let rec ifeval ie (env:environment)=
    match ie with
        | B x->x
        | V x->List.contains x env
        | If(cond,yep,nope)-> 
            if (ifeval cond env) 
              then (ifeval yep env) 
              else (ifeval nope env);;


