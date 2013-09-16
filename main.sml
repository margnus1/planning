structure TextToken =
  struct
    type token = string
    type style = unit
    fun string t = t
    fun style t = ()
    fun size t = String.size t
  end

structure PP = PPStreamFn(
    structure Token = TextToken
    structure Device = SimpleTextIODev)

val ppstream = PP.openStream (SimpleTextIODev.openDev { dst = TextIO.stdOut,
                                                        wid = 80 })
fun printUsage () =
    (PP.string ppstream (CommandLine.name () ^ " [file]");
     PP.openBox ppstream (PP.Abs 8);
     PP.newline ppstream;
     PP.string ppstream ("Prints the possible actions in the initial state of the " ^
                                  "PDDL-over-JSON file [file]");
     PP.closeBox ppstream;
     PP.newline ppstream)
        
; (* Imperative part starts here *)
case CommandLine.arguments () of
    [] => printUsage ()
  | file::_ => 
    let val json = JSONParser.parseFile file
        val problem as (PDDL.Problem {start, ...}) = PDDLParser.parse json
        val actions = PDDL.possibleActions problem start
        fun instanceToString (PDDL.Instance {bindings, action = PDDL.Action {name, variables, ...}}) =
             name ^ "(" ^ (String.concatWith ", " (map (fn n => case (PDDL.StringMap.find (bindings, n)) of
                                                                    SOME v => v
                                                                  | NONE => "???")
                                                       variables)) ^ ")"
    in
        PP.string ppstream (Int.toString (length actions) ^ " actions possible in initial state:");
        PP.newline ppstream;
        List.app (fn i => (PP.string ppstream (instanceToString i); PP.newline ppstream)) actions
    end
; PP.flushStream ppstream ;
