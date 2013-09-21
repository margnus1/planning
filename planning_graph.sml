structure PlanningGraph : HEURISTIC = struct

local
    open PDDL
    datatype fluent_state = True of fluent | False of fluent
    fun truth (True  _) = true
      | truth (False _) = false
    fun compareStates (True _, False _) = GREATER
      | compareStates (False _, True _) = LESS
      | compareStates (True  f1, True  f2) = compareFluents (f1, f2)
      | compareStates (False f1, False f2) = compareFluents (f1, f2)
    structure StateSet = ListSetFn(struct type ord_key = fluent_state val compare = compareStates end)
    fun invertState (True  f) = False f
      | invertState (False f) = True  f
    val invertStates = StateSet.map invertState
    (* Invariant: compareStates (a, b) = GREATER forall (a, b) : state_relation *)
    type state_relation = fluent_state * fluent_state
    fun mkSRelation (stateA, stateB) =
        case compareStates (stateA, stateB) of
            GREATER => (stateA, stateB)
          | _       => (stateB, stateA)
       (* | _ => raise Fail "PlanningGraph.mkSRelation: a state cannot be mutually exclusive with itself" *)
    fun compareSRelation ((a1, a2), (b1, b2)) =
        case compareStates (a1, b1) of
            EQUAL => compareStates (a2, b2)
          | ord => ord
    structure SRelationSet = BinarySetFn(struct type ord_key = state_relation val compare = compareSRelation end)
    (* Invariant: mutexes is sorted *)
    type state_layer = { states : StateSet.set, mutexes : SRelationSet.set }
    fun predicateToState binding (pred as Predicate {truth=true,  ...}) = True  (bindPredicate binding pred)
      | predicateToState binding (pred as Predicate {truth=false, ...}) = False (bindPredicate binding pred)
    datatype action = Action of { preconditions : StateSet.set,
                                  effects       : StateSet.set }
    val singletonSS = StateSet.singleton
    fun SSfromList  l = StateSet.addList (StateSet.empty, l)
    fun Ids fs = [Action {preconditions = singletonSS (True  fs), effects = singletonSS (True  fs)},
                  Action {preconditions = singletonSS (False fs), effects = singletonSS (False fs)}]
    fun instanceToAction (Instance {bindings, action = PDDL.Action { name, preconditions, effects, ...}}) =
            Action { preconditions = map (predicateToState bindings) preconditions |> SSfromList,
                     effects       = map (predicateToState bindings) effects       |> SSfromList }

    fun compareActions (Action {preconditions=preds1, effects=effects1},
                        Action {preconditions=preds2, effects=effects2}) =
        List.collate StateSet.compare ([preds1, effects1], [preds2, effects2])

    structure ActionSet = ListSetFn(struct type ord_key = action val compare = compareActions end)
    (* Invariant: compareActions (a, b) = GREATER forall (a, b) : action_relation *)
    type action_relation = action * action

    fun mkARelation (actionA, actionB) =
        case compareActions (actionA, actionB) of
            GREATER => (actionA, actionB)
          | _    => (actionB, actionA)
       (* | _ => raise Fail "PlanningGraph.mkARelation: an action cannot be mutually exclusive with itself" *)
    fun compareARelation ((a1, a2), (b1, b2)) =
        case compareActions (a1, b1) of
            EQUAL => compareActions (a2, b2)
          | ord => ord
    structure ARelationSet = BinarySetFn(struct type ord_key = action_relation val compare = compareARelation end)

    (* Invariant: actions and mutexes are sorted *)
    type action_layer = { actions : ActionSet.set, mutexes : ARelationSet.set }
in

datatype internal_state = IS of { goal        : StateSet.set,
                                  all_fluents : fluent list,
                                  all_actions : action list }

local
    fun canPerform states (Action {preconditions, ...}) =
        StateSet.isEmpty (StateSet.difference (preconditions, states))
    fun possibleActions (IS {all_actions, ...}) states =
        ActionSet.addList (ActionSet.empty, List.filter (canPerform states) all_actions)

    fun trivialMutexes (states : StateSet.set) =
        let
            fun loop (nil : fluent_state list) acc = acc
              | loop (True s::ss) acc =
                loop ss (if StateSet.member (states, False s) then
                         mkSRelation (True s, False s) :: acc else acc)
              | loop (False _::_) _ = raise Fail "This cannot happen!"
        in
            loop (List.filter truth (StateSet.listItems states)) nil
        end

    fun aCompetes {states, mutexes} (Action {preconditions = preconditions1, effects = effects1},
                                     Action {preconditions = preconditions2, effects = effects2}) =
        let
            val sHasCommonElements = StateSet.isEmpty o StateSet.intersection
            val mHasCommonElements = SRelationSet.isEmpty o SRelationSet.intersection
            fun SRSfromList l = SRelationSet.addList (SRelationSet.empty, l)
            val needsCrossProduct = mapConcat (fn pc1 =>
                                                  map (fn pc2 => mkSRelation (pc1, pc2))
                                                      (StateSet.listItems preconditions2))
                                              (StateSet.listItems preconditions1) |>
                                    SRSfromList
        in
            sHasCommonElements (effects1, invertStates effects2) orelse
            sHasCommonElements (preconditions1, invertStates effects2) orelse
            sHasCommonElements (effects1, invertStates preconditions2) orelse
            mHasCommonElements (mutexes, needsCrossProduct)
        end

    fun planActions is (sl as {states, mutexes} : state_layer) : action_layer =
        let
            val actions = possibleActions is states
            fun mkMutexes [] acc = acc
              | mkMutexes (a1::As) acc =
                List.mapPartial (fn a2 => if aCompetes sl (a1, a2)
                                          then SOME (mkARelation (a1, a2))
                                          else NONE)
                                As |>
                curry ARelationSet.addList acc |>
                mkMutexes As
        in
            { actions = actions,
              mutexes = mkMutexes (ActionSet.listItems actions) ARelationSet.empty }
        end

    fun sCompetes {actions, mutexes} (state1, state2) =
        compareStates (state1, invertState state2) = EQUAL orelse
        let
            val al = ActionSet.listItems actions
            fun hasEffect state (Action {effects, ...}) = StateSet.member (effects, state)
            val supports1 = List.filter (hasEffect state1) al
            val supports2 = List.filter (hasEffect state2) al
            val supportCross =
                foldl (fn (a1, acc) => map (fn a2 => mkARelation (a1, a2))
                                           supports2 |>
                                       foldl ARelationSet.add' acc)
                      ARelationSet.empty
                      supports1
        in
            (* Is there a pair of supporting actions that is not mutually exclusive *)
            not (ARelationSet.isEmpty (ARelationSet.difference (supportCross, mutexes)))
        end

    fun extrapolateStates actions =
        map (fn Action {effects, ...} => effects) (ActionSet.listItems actions) |>
        foldl StateSet.union StateSet.empty

    fun plan (is as IS {goal, ...}) (sl as {states, mutexes} : state_layer) =
        let
            val al as {actions, mutexes=amutexes} = planActions is sl
            val newStates  = extrapolateStates actions
            fun mkMutexes [] acc = acc
              | mkMutexes (s1::Ss) acc =
                List.mapPartial (fn s2 => if sCompetes al (s1, s2)
                                          then SOME (mkSRelation (s1, s2))
                                          else NONE)
                                Ss |>
                curry SRelationSet.addList acc |>
                mkMutexes Ss
            val newMutexes = mkMutexes (StateSet.listItems newStates) SRelationSet.empty
        in
            if StateSet.equal (states, newStates) andalso
               SRelationSet.equal (newMutexes, mutexes) then
                0
            else
                plan is { states = newStates, mutexes = newMutexes }
        end

    fun gatherFluents' [] binding = binding
      | gatherFluents' (Predicate {name, arguments, ...} :: preds) binding =
        gatherFluents' preds (StringMap.insert (binding, name, length arguments))
    fun gatherFluents [] binding = binding
      | gatherFluents (PDDL.Action {preconditions, effects, ...} ::actions) binding =
        gatherFluents' preconditions binding |>
        gatherFluents' effects |>
        gatherFluents actions

    fun arbitraryArgs (_ : string list) 0 = [nil] : string list list
      | arbitraryArgs objects n =
        mapConcat (fn obj =>
                      map (fn args => obj :: args)
                          (arbitraryArgs objects (n-1)))
                  objects
    fun generateFluents (objects : string list) (allFluentNames : int StringMap.map)
        : fluent list =
        mapConcat (fn (name : string, argCount : int) =>
                      map (fn (args : string list)
                                    => Fluent {name=name, arguments=args})
                                (arbitraryArgs objects argCount))
                  (StringMap.listItemsi allFluentNames)

    fun generateActions objects nil acc = acc
      | generateActions objects
                        (PDDL.Action{name, variables, preconditions, effects}::acts) acc =
        map (fn binds =>
                let
                    val binding = foldl StringMap.insert' StringMap.empty
                                        (ListPair.zipEq (variables, binds))
                in
                    Action { preconditions = map (predicateToState binding) preconditions |> SSfromList,
                             effects       = map (predicateToState binding) effects       |> SSfromList}
                end)
            (arbitraryArgs objects (length variables))
        @ acc |> generateActions objects acts

in

fun init (Problem {goal, actions, objects, ...}) =
    let
        val allFluentNames = gatherFluents actions StringMap.empty |>
                             gatherFluents' goal
        val allFluents = generateFluents objects allFluentNames
        val allActions = mapConcat Ids allFluents |>
                         generateActions objects actions
    in
        IS { goal = map (predicateToState StringMap.empty) goal |> SSfromList,
             all_fluents = allFluents,
             all_actions = ListMergeSort.uniqueSort compareActions allActions }
    end

fun heuristic (is as IS {goal, all_fluents, all_actions})
              (state : PDDL.state) =
    let
        fun generateFluent fluent =
            if FluentSet.member (state, fluent) then
                True fluent else False fluent
        val states = StateSet.addList (StateSet.empty,
                                       map generateFluent all_fluents)
        val mutexes = SRelationSet.addList (SRelationSet.empty,
                                            trivialMutexes states)
    in
        plan is ({states = states, mutexes = mutexes} : state_layer)
    end

end
end
end
