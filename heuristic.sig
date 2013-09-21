signature HEURISTIC = sig
    type internal_state
    val init : PDDL.problem -> internal_state
    val heuristic : internal_state -> PDDL.state -> int
end
