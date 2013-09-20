open PDDLToString

fun printUsage () =
    (TextIO.printLine (CommandLine.name () ^ " [command] [file]");
     TextIO.printLine "  [command] is one of:";
     TextIO.printLine "    bfs:";
     TextIO.printLine "      Solves the problem using BFS";
     TextIO.printLine "    stategraph:";
     TextIO.printLine "      Generates the entire state graph as a graphviz graph over stdout")

fun indent4 s = "    " ^ s
val parseProblem = PDDLParser.parse o JSONParser.parseFile

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
        List.app (TextIO.printLine o indent4 o instanceToString) (List.rev pathToGoal)
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
