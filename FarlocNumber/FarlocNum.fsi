module FarlocNum

exception Negative;;

type FN;;

val add : FN -> FN -> FN;;

val sub : FN -> FN -> FN;;

val (.<) : FN -> FN -> bool;;

val toFarloc : int -> FN;;

val fromFarloc : FN -> int;;

val mult : FN -> FN -> FN;;

val div : FN -> FN -> FN;;

