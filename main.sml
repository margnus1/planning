fun printUsage () =
    (TextIO.printLine (CommandLine.name () ^ " [file]");
     TextIO.printLine ("Prints the possible actions in the initial state of the " ^
                                  "PDDL-over-JSON file [file]"))
        
; (* Imperative part starts here *)
case CommandLine.arguments () of
    [] => printUsage ()
  | file::_ => 
    let val json = JSONParser.parseFile file
        val problem as (PDDL.Problem {start, ...}) = PDDLParser.parse json
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
        val (goal, pathToGoal) = BFS.search(problem)
        fun indent s = "    " ^ s
    in
	TextIO.printLine "Goal found:";
        List.app (TextIO.printLine o indent o fluentToString) (PDDL.FluentSet.listItems goal);
	TextIO.printLine "  via:";
	List.app (TextIO.printLine o indent o instanceToString) (List.rev pathToGoal)
    end
;
