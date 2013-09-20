structure Stategraph = struct
local
    open PDDL
    open PDDLToString
    structure StateMap = BinaryMapFn(struct type ord_key = state val compare = FluentSet.compare end)                
in
fun toDot (prob as Problem {start, ...}) =
    let
        val stateNumber = ref 0
        val stateNames  = ref StateMap.empty : string StateMap.map ref
        fun ++ cell = (cell := !cell + 1; !cell)
        fun getNewName () = ("state" ^ Int.toString (++stateNumber))
        fun getName state = valOf (StateMap.find (!stateNames, state))
        fun outputState state =
            let 
                val name = getName state
                val label = FluentSet.listItems state |>
                            map fluentToString |>
                            String.concatWith ",\\n"
            in
                TextIO.printLine (name ^ " [label=\"" ^ label ^ "\"];")
            end
        fun addState state =
            case StateMap.find (!stateNames, state) of
                SOME name => false
              | NONE => (stateNames := StateMap.insert (!stateNames,
                                                        state,
                                                        getNewName ());
                         outputState state;
                         true)
        fun outputLink from (action, to) =
            let
                val fromName = getName from
                val toName = getName to
            in
                TextIO.printLine (fromName ^ " -> " ^ toName ^ " [label=\"" ^
                                  instanceToString action ^ "\"];")
            end
        fun iterate nil = ()
          | iterate (state::stack) = 
            let
                val actions = possibleActions prob state
                fun origAndResult f a = (a, f a)
                val children = map (origAndResult (applyAction state)) actions
                (* Eww... *)
                val newChildren = List.filter (addState o #2) children
            in
                List.app (outputLink state) children;
                iterate (map #2 newChildren @ stack)
            end
    in
        TextIO.printLine "digraph pddl_problem {";
        addState start;
        iterate [start];
        TextIO.printLine "}"
    end
end
end
