structure Queue = struct
type 'a t = 'a list
val empty = []
fun push (Q, v) = Q @ [v]
fun pop (q::qs) = (q, qs)
  | pop _ = raise Empty
end
