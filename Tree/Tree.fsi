module Tree

type Tree<'a when 'a:comparison>

val empty: 'a Tree;;

val add :'a -> 'a Tree -> 'a Tree;;

val count : 'a Tree -> int;;    

val inorder : 'a Tree -> 'a list;;

val ofList : 'a list -> 'a Tree;;



