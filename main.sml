open PDDL
open PDDLToString

fun printUsage () =
    (TextIO.printLine (CommandLine.name () ^ " [command] [file]");
     TextIO.printLine "  [command] is one of:";
     TextIO.printLine "    bfs:";
     TextIO.printLine "      Solves the problem using BFS";
     TextIO.printLine "    astar:";
     TextIO.printLine "      Solves the problem using A*";
     TextIO.printLine "    stategraph:";
     TextIO.printLine "      Generates the entire state graph as a graphviz graph over stdout")

fun indent4 s = "    " ^ s

fun checkPathA (problem, current, nil) =
    if isGoal problem current
    then
        true
    else
        false

| checkPathA (problem as Problem{goal,...}, current, path : action_instance list) =
    let
	val head::tail = path
	val Instance {bindings, action=Action {preconditions=precond,...}, ...} = head
    in
	if matchesPredicates precond bindings current
	then
	    checkPathA (problem, (applyAction current) head, tail)
	else
	    false
    end

fun checkPath (problem as Problem{start, ...}, path : action_instance list) =
    checkPathA (problem, start, path)


val parseProblem = PDDLParser.parse o JSONParser.parseFile
structure AStar = AStarFn(MismatchingPredicates)

; (* Imperative part starts here *)
case CommandLine.arguments () of
    ["bfs", file] =>
    let
        val problem as (PDDL.Problem {start, ...}) = parseProblem file
        val timer = Timer.startRealTimer ()
        val cpuTimer = Timer.startCPUTimer ()
        val (goal, pathToGoal) = BFS.search(problem)
        val time = Timer.checkRealTimer timer
        val gcTime = Time.toReal (Timer.checkGCTime cpuTimer) /
                     Time.toReal (#usr (Timer.checkCPUTimer cpuTimer)) * 100.0
    in
        TextIO.printLine ("Goal found in " ^ Time.toString time ^ "s (" ^
                          LargeReal.fmt (StringCvt.FIX (SOME 1)) gcTime ^ "% GC):");
        List.app (TextIO.printLine o indent4 o fluentToString) (PDDL.FluentSet.listItems goal);
        TextIO.printLine "  via:";
        List.app (TextIO.printLine o indent4 o instanceToString) (pathToGoal);
	if checkPath(problem, pathToGoal) then
	    TextIO.printLine "Solution Ok!"
	else
	    raise Fail "The path is not a solution."

    end
  | ["astar", file] =>
    let
        val problem as (PDDL.Problem {start, ...}) = parseProblem file
        val timer = Timer.startRealTimer ()
        val cpuTimer = Timer.startCPUTimer ()
        val (goal, pathToGoal) = AStar.search(problem)
        val time = Timer.checkRealTimer timer
        val gcTime = Time.toReal (Timer.checkGCTime cpuTimer) /
                     Time.toReal (#usr (Timer.checkCPUTimer cpuTimer)) * 100.0
    in
        TextIO.printLine ("Goal found in " ^ Time.toString time ^ "s (" ^
                          LargeReal.fmt (StringCvt.FIX (SOME 1)) gcTime ^ "% GC):");
        List.app (TextIO.printLine o indent4 o fluentToString) (PDDL.FluentSet.listItems goal);
        TextIO.printLine "  via:";
        List.app (TextIO.printLine o indent4 o instanceToString) pathToGoal
    end
  | ["stategraph", file] => parseProblem file |>
                            Stategraph.toDot
  | ["heuristic", file] =>
    let
        val problem as (PDDL.Problem {start, ...}) = parseProblem file
    in
        TextIO.printLine ("Distance to goal: " ^
                          Int.toString (PlanningGraph.heuristic (PlanningGraph.init problem) start))
    end
  | _ => printUsage ()
;
