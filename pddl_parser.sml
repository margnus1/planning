structure PDDLParser = struct
local
    (* SML/NJ: CM.autoload ("$/json-lib.cm"); *)
    open JSON
    open PDDL

    structure StringSet = ListSetFn(struct type ord_key = string val compare = String.compare end)

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
        fun predVNames (Predicate {arguments, ...}) =
            List.mapPartial (fn Variable v => SOME v | _ => NONE) arguments
        val usedVars = mapConcat predVNames (!preconditions) @
                       mapConcat predVNames (!effects) |>
                       ListMergeSort.uniqueSort String.compare
        val action = Action {name = name,
                             variables = variables,
                             used_variables = usedVars,
                             preconditions = !preconditions,
                             effects = !effects }
    in
        parseActions (ls, action :: acc)
    end

fun inferObjectsFromFluents fluents =
    map (fn Fluent {arguments, ...} => arguments) fluents |>
    List.concat

fun inferObjectsFromPredicate (Predicate {arguments, ...}) =
    List.mapPartial (fn Literal v => SOME v | _ => NONE) arguments

fun inferObjectsFromPredicates preds =
    map inferObjectsFromPredicate preds |> List.concat

fun inferObjectsFromActions actions =
    map (fn Action {preconditions, effects, ...} =>
            inferObjectsFromPredicates preconditions @
            inferObjectsFromPredicates effects) actions |>
    List.concat

fun extractVariableNames nil acc = acc
  | extractVariableNames ((Action {variables, ...}) :: As) acc =
    extractVariableNames As (StringSet.addList (acc, variables))

fun findKey k l = #2 (valOf (List.find (fn (k', v) => k = k') l))
in
fun parse (OBJECT ol) =
    let
        val OBJECT ao = findKey "actions" ol
        val actions   = parseActions (ao, nil)
        val ARRAY sa  = findKey "start" ol
        val start     = parseState (sa, FluentSet.empty)
        val ARRAY ga  = findKey "goal" ol
        val goal      = map (parsePredicate []) ga
        val startObjects = inferObjectsFromFluents (FluentSet.listItems start) |>
                           ListMergeSort.uniqueSort String.compare
        val objects   = [inferObjectsFromActions actions,
                         inferObjectsFromFluents (FluentSet.listItems start),
                         inferObjectsFromPredicates goal] |>
                        List.concat |> ListMergeSort.uniqueSort String.compare
        val objectSet = StringSet.addList (StringSet.empty, objects)
        val variableNames = extractVariableNames actions StringSet.empty
    in
        if startObjects <> objects then
            TextIO.errorLine "Warning: Some objects were not mentioned in start" else ();
        if not (StringSet.isEmpty (StringSet.intersection (objectSet, variableNames))) then
            TextIO.errorLine "Warning: Variable names are shadowing objects" else ();
        Problem { actions = actions, start = start, goal = goal, objects = objects }
    end

fun parseSolution (Problem {actions, ...}) (OBJECT ol) =
    let
        val ARRAY al = findKey "plan" ol
        fun parseSFluent (STRING s) = parseFluent s
        fun parseInstance (name, arguments) =
            let
                val SOME (action as Action {variables, ...}) =
                    List.find (fn Action {name=n,...} => n=name)
                              actions
                val binding = foldl StringMap.insert' StringMap.empty
                                    (ListPair.zip (variables, arguments))
            in
                Instance { bindings = binding,
                           action = action }
            end
    in
        map (parseInstance o parseSFluent) al
    end
end
end
