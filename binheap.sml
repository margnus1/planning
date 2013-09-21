structure Heap=
  struct
    type 'a heap = {compare : 'a*'a->order,
                    next_avail: int ref,
                    values : 'a option Array.array ref
                    }
    type 'a prioq = 'a heap

(* We embed a binary tree in the array '!values', where the
 * left child of value i is at position 2*i+1 and the right
 * child of value i is at position 2*i+2.
 *
 * Invariants:
 *
 * (1) !next_avail is the next available position in the array
 * of !values.
 * (2) !values[i] is SOME(v) (i.e., not NONE) for 0<=iorder) *)

(* get_elt(p) is the pth element of a. Checks
 * that the value there is not NONE. *)
fun get_elt(values:'a option Array.array, p:int):'a =
  valOf(Array.sub(values,p))

val init_size = 1
fun create(cmp: 'a*'a -> order):'a heap =
  {compare = cmp,
   next_avail = ref 0,
   values = ref (Array.array(init_size,NONE))}
fun empty({compare,next_avail,values}:'a heap) = (!next_avail) = 0

exception FullHeap
exception InternalError
exception EmptyQueue

fun parent(n) = (n-1) div 2
fun left_child(n) = 2*n + 1
fun right_child(n) = 2*n + 2

(* Insert a new element "me" in the heap.  We do so by placing me
 * at a "leaf" (i.e., the first available slot) and then to
 * maintain the invariants, bubble me up until I'm <= all of my
 * parent(s).  If there's no room left in the heap, then we raise
 * the exception FullHeap.
 *)


fun insert({compare,next_avail,values}:'a heap) (me:'a): unit =
  if (!next_avail) >= Array.length(!values) then
      let 
	  val tmp = !values
	  val length = Array.length(!values)
      in
	  values := Array.array(length * 2, NONE);
	  Array.copy {src=tmp, dst= !values, di=0};
	  insert {compare=compare, next_avail=next_avail, values=values} me
      end

  else
      let fun bubble_up(my_pos:int):unit =
	      (* no parent if position is 0 -- we're done *)
      if my_pos = 0 then ()
      else
        let (* else get the parent *)
          val parent_pos = parent(my_pos);
          val parent = get_elt(!values, parent_pos)
        in
          (* compare my parent to me *)
          case compare(parent, me) of
            GREATER =>
              (* swap if me <= parent and continue *)
              (Array.update(!values,my_pos,SOME parent);
               Array.update(!values,parent_pos,SOME me);
               bubble_up(parent_pos))
          | _ => () (* otherwise we're done *)
        end
        (* start off at the next available position *)
        val my_pos = !next_avail
    in
      next_avail := my_pos + 1;
      Array.update(!values,my_pos,SOME me);
      (* and then bubble me up *)
      bubble_up(my_pos)
    end

exception EmptyQueue
(* Remove the least element in the heap and return it, raising
 * the exception EmptyQueue if the heap is empty.  To maintain
 * the invariants, we move a leaf to the root and then start
 * pushing it down, swapping with the lesser of its children.
 *)
fun extract_min({compare,next_avail,values}:'a heap):'a =
  if (!next_avail) = 0 then raise EmptyQueue
  else (* first element in !values is always the least *)
    let val result = get_elt(!values,0)
      (* get the last element so that we can put it at position 0 *)
      val last_index = (!next_avail) - 1
      val last_elt = get_elt(!values, last_index)
      (* min_child(p) is (c,v) where c is the child of p at which
       * the minimum element is stored), and v is the value
       * at that position. Requires p has a child. *)
      fun min_child(my_pos): int*'a =
        let
          val left_pos = left_child(my_pos)
          val right_pos = right_child(my_pos)
          val left_val = get_elt(!values, left_pos)
        in
          if right_pos >= last_index then (left_pos, left_val)
          else
            let val right_val = get_elt(!values, right_pos) in
              case compare(left_val, right_val)
                of GREATER => (right_pos, right_val)
                 | _ => (left_pos, left_val)
            end
        end
      (* Push "me" down until I'm no longer greater than my
       * children. When swapping with a child, choose the
       * smaller of the two.
       * Requires: get_elt(!values, my_pos) = my_val
       *)
      fun bubble_down(my_pos:int, my_val: 'a):unit =
        if left_child(my_pos) >= last_index then () (* done *)
        else let val (swap_pos, swap_val) = min_child(my_pos) in
          case compare(my_val, swap_val)
            of GREATER =>
              (Array.update(!values,my_pos,SOME swap_val);
               Array.update(!values,swap_pos,SOME my_val);
               bubble_down(swap_pos, my_val))
             | _ => () (* no swap needed *)
        end
    in
      Array.update(!values,0,SOME last_elt);
      Array.update(!values,last_index,NONE);
      next_avail := last_index;
      bubble_down(0, last_elt);
      result
    end
  end
