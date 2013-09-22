open PDDL
open PDDLToString

fun printUsage () =
    (TextIO.printLine (CommandLine.name () ^ " [command] [file] <solutionfile>");
     TextIO.printLine "  [command] is one of:";
     TextIO.printLine "    bfs:";
     TextIO.printLine "      Solves the problem using BFS";
     TextIO.printLine "    astar:";
     TextIO.printLine "      Solves the problem using A*";
     TextIO.printLine "    stategraph:";
     TextIO.printLine "      Generates the entire state graph as a graphviz graph over stdout";
     TextIO.printLine "  Solutions are saved to <solutionfile> if specified, to solution.json otherwise")

fun indent4 s = "    " ^ s

fun checkPathA (problem, current, nil) =
    if isGoal problem current then true else
    raise Fail "The resulting state is not the goal state"
  | checkPathA (problem, current,
                (head as Instance {bindings, action=Action {name, preconditions, ...}, ...}) :: tail) =
    if matchesPredicates preconditions bindings current then
        checkPathA (problem, (applyAction current) head, tail)
    else raise Fail ("The action " ^ name ^ " did not fulfill it's preconditions")

fun checkPath (problem as Problem{start, ...}, path : action_instance list) =
    checkPathA (problem, start, path)

val parseProblem = PDDLParser.parse o JSONParser.parseFile
structure AStar = AStarFn(MismatchingPredicates)

fun time f a =
    let
        val timer = Timer.startRealTimer ()
        val cpuTimer = Timer.startCPUTimer ()
        val result = f a
        val time = Timer.checkRealTimer timer
        val gcTime = Time.toReal (Timer.checkGCTime cpuTimer) /
                     Time.toReal (#usr (Timer.checkCPUTimer cpuTimer)) * 100.0
    in
        (Time.toString time ^ "s (" ^ LargeReal.fmt (StringCvt.FIX (SOME 1)) gcTime ^ "% GC)",
         result)
    end

fun search (file::rest) algorithm =
    let
        val problem as (PDDL.Problem {start, ...}) = parseProblem file
        val (time, (goal, pathToGoal)) = time algorithm problem
        val resultFile = case rest of smth::_ => smth | nil => "solution.json"
    in
        TextIO.printLine ("Goal found in " ^ time ^ ":");
        List.app (TextIO.printLine o indent4 o fluentToString) (PDDL.FluentSet.listItems goal);
        TextIO.printLine "  via:";
        List.app (TextIO.printLine o indent4 o instanceToString problem) (pathToGoal);
        if not (checkPath(problem, pathToGoal)) then
            TextIO.errorLine "Warning: The found solution does not seem to be correct"
        else ();
        PDDLToJSON.saveSolution problem pathToGoal resultFile
    end
  | search _ _ = printUsage ()

; (* Imperative part starts here *)
case CommandLine.arguments () of
    "bfs"   :: args => search args BFS.search
  | "astar" :: args => search args AStar.search
  | ["stategraph", file] => Stategraph.toDot (parseProblem file)
  | _ => printUsage ()
;
