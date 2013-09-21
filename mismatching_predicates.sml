structure MismatchingPredicates : HEURISTIC = struct
local
    open PDDL
    fun mismatchesPredicate state (pred as Predicate {truth=true, ...}) =
        not (FluentSet.member (state, bindPredicate StringMap.empty pred))
      | mismatchesPredicate state (pred as Predicate {truth=false, ...}) =
        FluentSet.member (state, bindPredicate StringMap.empty pred)
in
type internal_state = predicate list
fun init (Problem {goal, ...}) = goal
fun heuristic goal state =
    length (List.filter (mismatchesPredicate state) goal)
end
end
