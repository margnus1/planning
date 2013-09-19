fun printUsage () =
    (TextIO.print (CommandLine.name () ^ " [file]");
     TextIO.print ("Prints the possible actions in the initial state of the " ^
                                  "PDDL-over-JSON file [file]"))
        
; (* Imperative part starts here *)
case CommandLine.arguments () of
    [] => printUsage ()
  | file::_ => 
    let val json = JSONParser.parseFile file
        val problem as (PDDL.Problem {start, ...}) = PDDLParser.parse json
        fun instanceToString (PDDL.Instance {bindings, action = PDDL.Action {name, variables, ...}}) =
             name ^ "(" ^ (String.concatWith ", " (map (fn n => case (PDDL.StringMap.find (bindings, n)) of
                                                                    SOME v => v
                                                                  | NONE => "???")
                                                       variables)) ^ ")"
        fun fluentToString (PDDL.Fluent {name, arguments}) =
            name ^ "(" ^ (String.concatWith ", " arguments) ^ ")"
        val (goal, pathToGoal) = BFS.search(problem)
    in
	TextIO.printLine "Goal found:";
        List.app (TextIO.printLine o fluentToString) (PDDL.FluentSet.listItems goal);
	TextIO.printLine "   via:";
	List.app (fn s => TextIO.printLine (instanceToString s)) (List.rev pathToGoal)
    end
;
