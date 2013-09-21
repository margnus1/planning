functor AStarFn (Heur : HEURISTIC) = struct
local
    open PDDL

    structure StateKey = struct type ord_key = FluentSet.set
                                val compare = FluentSet.compare end
    structure StateSet = BinarySetFn(StateKey)
    structure StateMap = BinaryMapFn(StateKey)
    type elem_type = int * state
    fun compareElem ((n1, _), (n2, _)) = Int.compare (n1, n2)

    datatype parent = ROOT | P of { parent : state,
                                    action : action_instance,
                                    distance : int }

    fun distanceTo (parents : parent StateMap.map) state =
        case StateMap.find (parents, state) of
            SOME (P {distance, ...}) => distance
          | SOME ROOT                => 0
          | NONE => raise Fail "Kuken hande i AStar.distanceTo"
in

fun getPath (parents : parent StateMap.map, state) =
    case StateMap.find (parents, state) of
        SOME (P{parent, action, ...}) => action :: getPath (parents, parent)
      | SOME ROOT                     => nil
      | NONE => raise Fail "Missing ponter in parents in AStarFn.getPath"

fun search (problem as Problem {start,...}) =
    let
        val getHeur = Heur.heuristic (Heur.init problem)
        val heap = Heap.create compareElem
        val () = Heap.insert heap (getHeur start, start)
        (* If child has not been seen yet, or if the path via me is better than
           the one found so far, [re]insert child into heap and make me it's parent *)
        fun addChildren me distanceToMe [] (parents : parent StateMap.map) = parents
          | addChildren me distanceToMe ((child, act)::children) parents =
            if case StateMap.find (parents, child) of
                   SOME (P {distance, ...}) => (distance > distanceToMe + 1)
                 | SOME ROOT                => false
                 | NONE                     => true
            then
                (Heap.insert heap (getHeur child + distanceToMe + 1, child);
                 StateMap.insert (parents, child, P {parent = me,
                                                     action = act,
                                                     distance = distanceToMe + 1}) |>
                 addChildren me distanceToMe children)
            else addChildren me distanceToMe children parents

        fun loop (closed : StateSet.set)
                 (parents : parent StateMap.map) =
            let
                val (_, state) = Heap.extract_min heap
            in
                if StateSet.member (closed, state) then
                    loop closed parents
                else
                    let
                        val distanceToMe = distanceTo parents state
                        val actions = possibleActions problem state
                        fun origAndResult f a = (f a, a)
                        (* [(child, actionThatLeadToChild), ...] *)
                        val children = map (origAndResult (applyAction state)) actions
                    in
                        if isGoal problem state then
                            (state, List.rev (getPath (parents, state)))
                        else
                            loop (StateSet.add (closed, state))
                                 (addChildren state distanceToMe children parents)
                    end
            end
    in
        loop StateSet.empty (StateMap.insert (StateMap.empty, start, ROOT))
    end
end
end
