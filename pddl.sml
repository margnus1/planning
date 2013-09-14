structure PDDL = struct
datatype fluent = Fluent of { name : string, arguments : string list }
structure FluentSet = ListSetFn(
    struct
    type ord_key = fluent
    fun compare (Fluent {name = na, arguments = aa}, 
                 Fluent {name = nb, arguments = ab}) = 
        case String.compare (na, nb) of
            EQUAL => List.collate String.compare (aa, ab)
          | v => v
    end)
type state = FluentSet.set
datatype pred_arg = Literal of string | Variable of string
datatype predicate = Predicate of { truth : bool,
                                    name : string,
                                    arguments : pred_arg list }
datatype action = Action of { name : string,
                              variables : string list,
                              preconditions : predicate list,
                              effects : predicate list }
datatype action_instance = Instance of { bindings : (string * string) list,
                                         action : action }
datatype problem = Problem of { actions : action list,
                                start : state,
                                goal : state }

end

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

fun parseActions (nil : (string * value) list, acc : action list) = acc
  | parseActions ((key, OBJECT ao)::ls, acc) =
    let val (name, variables) = parseFluent key
        val preconditions = ref nil : predicate list ref
        val effects = ref nil : predicate list ref
        fun parsePredicate (STRING pred) = 
            let val (truth, (pname, pargs)) =
                    if (String.sub (pred, 0) = #"-")
                    then (false, parseFluent (String.extract (pred, 1, NONE)))
                    else (true, parseFluent pred)
                fun parsePredArg arg = if List.exists (fn e => e = arg) variables
                                       then Variable arg else Literal arg
            in Predicate { truth = truth, name = pname, 
                           arguments = map parsePredArg pargs } end
        fun parseAction nil = ()
          | parseAction ((what, ARRAY pl)::ls) = 
            ((case what of "preconditions" => preconditions | "effects" => effects)
             := map parsePredicate pl; parseAction ls)
        val () = parseAction ao
        val action = Action {name = name, variables = variables,
                             preconditions = !preconditions,
                             effects = !effects }
    in
        parseActions (ls, action :: acc)
    end

fun parse' (nil, problem) = problem
  | parse' ((("actions", OBJECT ao))::ls, (Problem {start, goal, ...})) =
    parse' (ls, Problem {start = start, goal = goal,
                         actions = parseActions (ao, nil)})
  | parse' (("start", ARRAY sa)::ls, Problem {actions, start, goal}) =
    parse' (ls, Problem {actions = actions, goal = goal,
                         start = parseState (sa, start)})
  | parse' (("goal",  ARRAY ga)::ls, Problem {actions, start, goal}) =
    parse' (ls, Problem {actions = actions, start = start,
                         goal = parseState (ga, goal)})
in
fun parse (OBJECT ol) = parse' (ol, Problem {actions = nil,
                                             start = FluentSet.empty,
                                             goal  = FluentSet.empty })
end
end
