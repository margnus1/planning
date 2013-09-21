local
    open PDDL



structure StateSet = BinarySetFn(
      struct
      type ord_key = FluentSet.set
      val compare = FluentSet.compare
      end)

fun frontierAdd(frontier,visited,nil) =
    (frontier,visited)
  | frontierAdd(frontier,visited,(child, pahtToChild)::children) =
    if StateSet.member(visited,child) then
        frontierAdd(frontier,visited,children)
    else
        frontierAdd (Queue.push (frontier, (child, pahtToChild)),
		     StateSet.add (visited, child), children)

fun bfsrun(problem : problem, visited : StateSet.set, frontier : (state * action_instance list) Queue.t) =
    let
       val ((node, pathToNode), rest) = Queue.pop frontier
           handle Empty => raise Fail "Goal unreachable in BFS.search"
       val actions = possibleActions problem node
       fun origAndResult f a = (f a, a::pathToNode)
       (* [(child, pathToChild), ...] *)
       val children = map (origAndResult (applyAction node)) actions
       val (newfrontier, newvisited) = frontierAdd(rest,visited,children)
    in
        if isGoal problem node then
            (node, List.rev pathToNode)
        else
	    bfsrun(problem,newvisited,newfrontier)
    end

in
structure BFS = struct
    fun search (problem as Problem {start,...}) =
        bfsrun (problem, StateSet.empty, Queue.push (Queue.empty, (start, [])))
end
end
