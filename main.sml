fun printUsage () =
    (TextIO.printLine (CommandLine.name () ^ " [command] [file]");
     TextIO.printLine "  [command] is one of:";
     TextIO.printLine "    bfs:";
     TextIO.printLine "      Solves the problem using BFS";
     TextIO.printLine "    stateGraph:";
     TextIO.printLine "      Generates the entire state graph as a graphviz graph over stdout")

fun instanceToString (PDDL.Instance {bindings, action = PDDL.Action {name, variables, ...}}) =
    let
        val vars = (map (fn n => case (PDDL.StringMap.find (bindings, n)) of
                                     SOME v => v
                                   | NONE => "???")
                        variables)
    in
        name ^ "(" ^ (String.concatWith ", " vars) ^ ")"
    end

fun fluentToString (PDDL.Fluent {name, arguments}) =
    name ^ "(" ^ (String.concatWith ", " arguments) ^ ")"
        
fun indent4 s = "    " ^ s
val parseProblem = PDDLParser.parse o JSONParser.parseFile

; (* Imperative part starts here *)
case CommandLine.arguments () of
    ["bfs", file] => 
    let
        val problem as (PDDL.Problem {start, ...}) = parseProblem file
        val (goal, pathToGoal) = BFS.search(problem)
    in
        TextIO.printLine "Goal found:";
        List.app (TextIO.printLine o indent4 o fluentToString) (PDDL.FluentSet.listItems goal);
        TextIO.printLine "  via:";
        List.app (TextIO.printLine o indent4 o instanceToString) (List.rev pathToGoal)
    end
 | _ => printUsage ()
;
