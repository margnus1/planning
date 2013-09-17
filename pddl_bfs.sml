local
    open PDDL



structure StateSet = BinarySetFn(
      struct 
      type ord_key = FluentSet.set 
      val compare = FluentSet.compare
      
      end)

fun frontierAdd(frontier,visited,nil) =
    (frontier,visited)
| frontierAdd(frontier,visited,child::children) =
    
    if StateSet.member(visited,child) then
        frontierAdd(frontier,visited,children)
    else
        frontierAdd(Queue.push(frontier,child),StateSet.add(visited,child),children)

fun bfsrun(problem : problem, visited : StateSet.set, frontier) =
    let
       val (node, rest) = Queue.pop(frontier)
       val actions = possibleActions problem node
       val children = map (applyAction node) actions
       val (newfrontier,newvisited) = frontierAdd(rest,visited,children)
    in
        if isGoal problem node then
            node
        else
      bfsrun(problem,newvisited,newfrontier)
    end
    
in
structure BFS = struct
    fun search (problem as Problem {start,...}) =
        bfsrun (problem, StateSet.empty, Queue.push (Queue.empty, start))
end
end
