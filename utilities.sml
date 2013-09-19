(* Parts from http://mlton.org/Utilities *)

infix   8  * / div mod        (* +1 from Basis Library *)
infix   7  + - ^              (* +1 from Basis Library *)
infixr  6  :: @               (* +1 from Basis Library *)
infix   5  = <> > >= < <=     (* +1 from Basis Library *)
infix   4  <\ \>
infixr  4  </ />
infix   3  o
infix   2  |>
infixr  2  <|
infix   1  :=                 (* -2 from Basis Library *)
infix   0  before

(* Some basic combinators *)
fun const x _ = x
fun cross (f, g) (x, y) = (f x, g y)
fun curry f x y = f (x, y)
fun fail e _ = raise e
fun id x = x

(* Infixing, sectioning, and application operators *)
fun x <\ f = fn y => f (x, y)
fun f \> y = f y
fun f /> y = fn x => f (x, y)
fun x </ f = f x

(* Piping operators *)
val op|> = op</
val op<| = op\>

(* Misc helpers *)
fun oinvert ord = 
    case ord of
	GREATER => LESS
      | EQUAL   => EQUAL
      | LESS    => GREATER 

structure Function2 = 
struct 
fun curry f a b = f (a,b)
fun uncurry f (a,b) = f a b
fun crev f b a = f a b
end

structure Tuple2 = 
struct
fun map f (a, b) = (f a, f b)
fun nth 0 (a, _) = a
  | nth 1 (_, b) = b
  | nth _ _      = raise Subscript
end

structure Tuple3 = 
struct
fun map f (a,b,c) = (f a, f b, f c)
fun nth 0 (a,_,_) = a
  | nth 1 (_,b,_) = b
  | nth 2 (_,_,c) = c
  | nth _ _       = raise Subscript
end

structure List = struct
open List
fun max comp nil = raise Empty
  | max comp [l] = l 
  | max comp (l::elem::ls) = 
    case comp (elem, l) of
        GREATER => max comp (elem::ls)
      | _    => max comp (l::ls)
fun mapConcat f = concat o map f
end
val mapConcat = List.mapConcat

structure TextIO = struct
open TextIO
fun printLine t = print (t ^ "\n");
end
