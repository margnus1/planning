structure PDDLParser = struct
local
    (* SML/NJ: CM.autoload ("$/json-lib.cm"); *)
    open JSON
    open PDDL

fun parseFluent fluent = 
    let fun separator #"," = true
          | separator #"(" = true
          | separator #")" = true
          | separator c = Char.isSpace c
        val name::args = String.tokens separator fluent
    in (name, args) end

fun parseState (nil : value list, acc : FluentSet.set) = acc
  | parseState ((STRING fluent)::ls, acc) =
    let val (name, args) = parseFluent fluent 
        val newfluent = Fluent {name = name, arguments = args} 
    in parseState (ls, FluentSet.add (acc, newfluent)) end

fun parsePredicate variables (STRING pred) = 
    let val (truth, (pname, pargs)) =
            if (String.sub (pred, 0) = #"-")
            then (false, parseFluent (String.extract (pred, 1, NONE)))
            else (true, parseFluent pred)
        fun parsePredArg arg = if List.exists (fn e => e = arg) variables
                               then Variable arg else Literal arg
    in Predicate { truth = truth, name = pname, 
                   arguments = map parsePredArg pargs } end
    
fun parseActions (nil : (string * value) list, acc : action list) = acc
  | parseActions ((key, OBJECT ao)::ls, acc) =
    let val (name, variables) = parseFluent key
        val preconditions = ref nil : predicate list ref
        val effects = ref nil : predicate list ref
        fun parseAction nil = ()
          | parseAction ((what, ARRAY pl)::ls) = 
            ((case what of "preconditions" => preconditions | "effects" => effects)
             := map (parsePredicate variables) pl; parseAction ls)
        val () = parseAction ao
        val action = Action {name = name, variables = variables,
                             preconditions = !preconditions,
                             effects = !effects }
    in
        parseActions (ls, action :: acc)
    end

in
fun parse (OBJECT ol) =
    let
        fun findKey k l = #2 (valOf (List.find (fn (k', v) => k = k') l))
        val OBJECT ao = findKey "actions" ol
        val actions   = parseActions (ao, nil)
        val ARRAY sa  = findKey "start" ol
        val start     = parseState (sa, FluentSet.empty)
        val ARRAY ga  = findKey "goal" ol
        val goal      = map (parsePredicate []) ga
        val objects   = [] (* TODO *)
    in
        Problem { actions = actions, start = start, goal = goal, objects = objects }
    end
end
end
