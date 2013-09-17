structure PDDL = struct

datatype fluent = Fluent of { name : string, arguments : string list }
structure FluentSet = ListSetFn(
    struct
    type ord_key = fluent
    fun compare (Fluent {name = na, arguments = aa}, 
                 Fluent {name = nb, arguments = ab}) = 
        case String.compare (na, nb) of
            EQUAL => List.collate String.compare (aa, ab)
          | v => v
    end)
type state = FluentSet.set
                 
structure StringMap = BinaryMapFn(struct type ord_key = string val compare = String.compare end)
type binding = string StringMap.map
                                 
datatype pred_arg = Literal of string | Variable of string
datatype predicate =      Predicate of { truth         : bool,
                                         name          : string,
                                         arguments     : pred_arg list }

datatype action =            Action of { name          : string,
                                         variables     : string list,
                                         preconditions : predicate list,
                                         effects       : predicate list }
datatype action_instance = Instance of { bindings      : binding,
                                         action        : action }

datatype problem =           Problem of { actions      : action list,
                                          objects      : string list,
                                          start        : state,
                                          (* INVARIANT: There are no variables in goal *)
                                          goal         : predicate list }



(* Type: binding -> predicate list -> FluentSet.set *)
(* Pre: All variables in preds are bound in binding *)
(* Note: The truth values of the predicates are discarded. *)
fun bind binding preds = 
    let
        fun bindPred (Predicate {name, arguments, ...}) =
            let
                fun bindArg (Literal  v) = v
                  | bindArg (Variable n) = 
                    case StringMap.find (binding, n) of
                        SOME v => v
                      | NONE => raise Fail ("Unbound variable " ^ name ^ "(..., " ^ n ^ ",...) in PDDL.bind")
            in
                Fluent { name = name, arguments = map bindArg arguments }
            end
    in
        FluentSet.addList (FluentSet.empty, map bindPred preds)
    end


fun applyAction state (Instance {bindings, action = Action{effects,...}}) =
    let
	fun truth (Predicate {truth, ...}) = truth
	val (truePreds, falsePreds) = List.partition truth effects
	val add    = bind bindings truePreds
	val remove = bind bindings falsePreds
	val state = FluentSet.difference(state,remove)
        
    in
	FluentSet.union(state,add)
    end

                
(* Type: predicate list -> binding -> state -> bool *)
fun matchesPredicates preds binding state =
    let
        fun truth (Predicate {truth,...}) = truth
        val (truePreds, falsePreds) = List.partition truth preds
        val required  = bind binding truePreds
        val forbidden = bind binding falsePreds
    in 
        FluentSet.isEmpty (FluentSet.difference   (required,  state)) andalso
        FluentSet.isEmpty (FluentSet.intersection (forbidden, state))
    end

fun isGoal (Problem {goal, ...}) state = matchesPredicates goal StringMap.empty state

fun getFluentsByName name state =
    List.filter (fn Fluent {name=flun,...} => flun = name) (FluentSet.listItems state)
               
(* Type: problem -> state -> action_instance list *)
fun possibleActions (Problem {actions, objects,...}) state =
    let
        fun allInstances (action as Action {preconditions,...}) = 
            let
                (* All bindings creating an instance of action that can be applied to state *)
                fun instances ([] : predicate list) (bindings : binding) = [ bindings ] : binding list
                  | instances ((Predicate {truth, name, arguments}) :: preds) bindings =
                    let
                        val fluentArgs = map (fn Fluent {arguments,...} => arguments) 
                                             (getFluentsByName name state)
                        fun takeEqual v (l::ls) =
                            if v = l then SOME ls
                            else          NONE
                          | takeEqual _ [] = 
                            raise Fail ("Unexpected end of list in PDDL.possibleActions(takeEqual). " ^
                                        "Possibly mismatched number of arguments.")
                        (* All bindings that make Predicate match at least one of fluentArgs. *)
                        (* fluentArgs are the arguments of the fluents of the same name *)
                        fun trueBinds _        []         _  = []
                          | trueBinds bindings fluentArgs [] = [ bindings ]
                          | trueBinds bindings fluentArgs (Literal  v :: args) =
                            trueBinds bindings (List.mapPartial (takeEqual v) fluentArgs) args
                          | trueBinds bindings fluentArgs (Variable n :: args) =
                            case StringMap.find (bindings, n) of
                                SOME v => trueBinds bindings (List.mapPartial (takeEqual v) fluentArgs) args
                              | NONE => 
                                let 
                                    val uniqueValues = ListMergeSort.uniqueSort String.compare 
                                                                                (map hd fluentArgs)
                                in
                                    List.concat (
                                    map (fn v => trueBinds (StringMap.insert (bindings, n, v))
                                                           (List.mapPartial (takeEqual v) fluentArgs)
                                                           args)
                                        uniqueValues)
                                end

                        (* Note: This is a source of combinatorial explosion. If
                                 we allow a variable to be unbound when it may
                                 be arbitrary, we can avoid this. *)
                        fun arbitraryBinds bindings [] = [bindings]
                          | arbitraryBinds bindings (Literal _  :: args) = arbitraryBinds bindings args
                          | arbitraryBinds bindings (Variable n :: args) =
                            case StringMap.find (bindings, n) of
                                SOME _ => arbitraryBinds bindings args
                              | NONE   => List.concat (
                                          map (fn v => arbitraryBinds (StringMap.insert (bindings, n, v)) args)
                                              objects)

                        (* All bindings that make Predicate match none of fluentArgs. *)
                        fun falseBinds bindings [] args = arbitraryBinds bindings args
                          | falseBinds bindings _  []   = []
                          | falseBinds bindings fluentArgs (Literal v :: args) =
                            falseBinds bindings (List.mapPartial (takeEqual v) fluentArgs) args
                          | falseBinds bindings fluentArgs (Variable n :: args) = 
                            case StringMap.find (bindings, n) of
                                SOME v => falseBinds bindings (List.mapPartial (takeEqual v) fluentArgs) args
                              | NONE => List.concat (
                                        map (fn v => falseBinds bindings
                                                                (List.mapPartial (takeEqual v) fluentArgs) 
                                                                args)
                                            objects)
                    in
                        (* Note: |> is the pipelining operator, defined in utilities.sml *)
                        (if truth then trueBinds else falseBinds) bindings fluentArgs arguments |>
                        map (instances preds) |>
                        List.concat                                                                         
                    end
                    
                val (truePres, falsePres) = List.partition (fn Predicate {truth,...} => truth) preconditions
                val allTrueBindings = instances truePres StringMap.empty
                val allBindings = List.concat (List.map (instances falsePres) allTrueBindings)
            in
                map (fn binding => Instance { bindings = binding, action = action }) allBindings
            end
        
    in
        List.concat (map allInstances actions)
    end

end
