type t = INT | LSTINT;;

type exp =
  K of int
  | Plus of exp * exp
  | Nil
  | Cons of exp * exp
  | Hd of exp
  | Tl of exp;;

let rec tc e=
    match e with
        | K (_) -> Some INT
        | Plus(e1, e2) -> 
            match (tc e1, tc e2) with
                | (Some INT, Some INT) -> Some INT
                | _ -> None
        | Nil -> Some LSTINT
        | Cons(e1, e2) ->
            match (tc e1, tc e2) with
                | (Some INT, Some LSTINT) -> Some LSTINT
                | _ -> None
        | Hd (e) | Tl (e) ->
            match (tc e) with
                | Some LSTINT -> Some LSTINT
                | _ -> None
