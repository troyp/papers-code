datatype 'a List = ListNode of 'a * 'a List | Empty
exception EmptyList

fun getItem (ListNode (i,_)) = i
  | getItem (Empty) = raise EmptyList

fun nextNode (ListNode(_,rest)) = rest
  | nextNode (Empty) = raise EmptyList

val il = ListNode(2, ListNode(4, Empty));
val sl = ListNode("hi", ListNode("bye", Empty));
(* val il2 = ListNode(3, ListNode("hi",Empty)); *) (* type error *)
val i = getItem(il);
val s = getItem(sl);
