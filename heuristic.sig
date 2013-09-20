signature HEURISTIC = sig
    type internal_state
    type heuristic
    val compareHeuristics : (heuristic * heuristic) -> order
    val init : PDDL.problem -> internal_state
    val heuristic : internal_state -> PDDL.state -> heuristic
end
