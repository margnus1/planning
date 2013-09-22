structure PDDLToString = struct

(* Substitute is used when a variable is unbound *)
fun instanceToString (PDDL.Problem {objects, ...})
                     (PDDL.Instance {bindings, action = PDDL.Action {name, variables, ...}}) =
    let
        val substitute = case objects of obj::_ => obj
                                       | [] => "???"
        val vars = (map (fn n => case (PDDL.StringMap.find (bindings, n)) of
                                     SOME v => v
                                   | NONE => substitute)
                        variables)
    in
        name ^ "(" ^ (String.concatWith ", " vars) ^ ")"
    end

fun fluentToString (PDDL.Fluent {name, arguments}) =
    name ^ "(" ^ (String.concatWith ", " arguments) ^ ")"

end
