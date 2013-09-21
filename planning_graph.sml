structure PlanningGraph : HEURISTIC = struct
type internal_state = PDDL.predicate list
type heuristic = int
val compareHeuristics = Int.compare

local
    open PDDL
    datatype fluent_state = True of fluent | False of fluent
    fun compareStates (True _, False _) = GREATER
      | compareStates (False _, True _) = LESS
      | compareStates (True  f1, True  f2) = compareFluents (f1, f2)
      | compareStates (False f1, False f2) = compareFluents (f1, f2)
    structure StateSet = BinarySetFn(struct type ord_key = fluent_state val compare = compareStates end)
    (* Invariant: compareStates (a, b) = GREATER forall (a, b) : state_relation *)
    type state_relation = fluent_state * fluent_state
    fun mkSRelation (stateA, stateB) =
        case compareStates (stateA, stateB) of
            GREATER => (stateA, stateB)
          | LESS    => (stateB, stateA)
          | _ => raise Fail "PlanningGraph.mkSRelation: a state cannot be mutually exclusive with itself"
    fun compareRelation ((a1, a2), (b1, b2)) =
        case compareStates (a1, b1) of
            EQUAL => compareStates (a2, b2)
          | ord => ord
    structure SRelationSet = BinarySetFn(struct type ord_key = state_relation val compare = compareRelation end)
    type state_layer = { states : StateSet.set, mutexes : SRelationSet.set }
    fun predicateToState binding (pred as Predicate {truth=true,  ...}) = True  (bindPredicate binding pred)
      | predicateToState binding (pred as Predicate {truth=false, ...}) = False (bindPredicate binding pred)
    datatype action = Id of fluent_state | Action of { name          : string,
                                                       preconditions : fluent_state list,
                                                       effects       : fluent_state list }
    fun instanceToAction (Instance {bindings, action = PDDL.Action { name, preconditions, effects, ...}}) =
            Action { name = name,
                     preconditions = map (predicateToState bindings) preconditions,
                     effects       = map (predicateToState bindings) effects }

    fun compareActions (Id f1, Id f2) = compareStates (f1, f2)
      | compareActions (Id _, Action _) = GREATER
      | compareActions (Action _, Id _) = LESS
      | compareActions (Action {name=name1, preconditions=preds1, effects=effects1},
                        Action {name=name2, preconditions=preds2, effects=effects2}) =
        case String.compare (name1, name2) of
            EQUAL => List.collate (List.collate compareStates) ([preds1, effects1], [preds2, effects2])
          | ord => ord
    (* Invariant: compareActions (a, b) = GREATER forall (a, b) : action_relation *)
    type action_relation = action * action
    fun mkARelation (actionA, actionB) =
        case compareActions (actionA, actionB) of
            GREATER => (actionA, actionB)
          | LESS    => (actionB, actionA)
          | _ => raise Fail "PlanningGraph.mkARelation: an action cannot be mutually exclusive with itself"

    type action_layer = { actions : action list, mutexes : action_relation list }

    fun plan ({states, mutexes} : state_layer) =
        let
        in
            ()
        end
in
fun init (Problem {goal, ...}) = goal

fun heuristic (preds : predicate list) (state : state) = 1 : heuristic

end
end
