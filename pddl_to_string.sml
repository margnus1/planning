structure PDDLToString = struct

fun instanceToString (PDDL.Instance {bindings, action = PDDL.Action {name, variables, ...}}) =
    let
        val vars = (map (fn n => case (PDDL.StringMap.find (bindings, n)) of
                                     SOME v => v
                                   | NONE => "???")
                        variables)
    in
        name ^ "(" ^ (String.concatWith ", " vars) ^ ")"
    end

fun fluentToString (PDDL.Fluent {name, arguments}) =
    name ^ "(" ^ (String.concatWith ", " arguments) ^ ")"

end
