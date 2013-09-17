
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
        (*val actions = PDDL.possibleActions problem start
        fun instanceToString (PDDL.Instance {bindings, action = PDDL.Action {name, variables, ...}}) =
             name ^ "(" ^ (String.concatWith ", " (map (fn n => case (PDDL.StringMap.find (bindings, n)) of
                                                                    SOME v => v
                                                                  | NONE => "???")
                                                       variables)) ^ ")" 
    in
        PP.string ppstream (Int.toString (length actions) ^ " actions possible in initial state:");
        PP.newline ppstream;
        List.app (fn i => (PP.string ppstream (instanceToString i); PP.newline ppstream)) actions *)
        fun fluentToString (PDDL.Fluent {name, arguments}) =
            name ^ "(" ^ (String.concatWith ", " arguments) ^ ")\n"
        val goal = BFS.search(problem)
    in
        List.app (TextIO.print o fluentToString) (PDDL.FluentSet.listItems goal)
    end
;
