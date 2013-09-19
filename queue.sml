structure Queue : sig
    type 'a t
    val empty : 'a t
    val push : ('a t * 'a) -> 'a t
    val pop : 'a t -> ('a * 'a t)
end = struct
datatype 'a t = Queue of {a: 'a list, b: 'a list}
val empty = Queue{a=[],b=[]}
(* fun queue{a=[], b= r}= Queue{a=rev r, b=[]} *)
(*   | queue q = Queue q *)

fun push (Queue{a,b}, v) = Queue{a = a,b = v::b}

fun pop (Queue {a=nil, b=nil}) = raise Empty
  | pop (Queue {a=nil, b}) = pop(Queue{a = rev b, b = []})
  | pop (Queue{a = q::qs,b = r}) = (q, Queue{a = qs,b = r})
end
