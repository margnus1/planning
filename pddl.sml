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

structure StringMap = BinaryMapFn(
	  struct
	  type ord_key = string
	  val compare = String.compare
	  end)

datatype pred_arg = Literal of string | Variable of string
datatype predicate = Predicate of { truth : bool,
                                    name : string,
                                    arguments : pred_arg list }
datatype action = Action of { name : string,
                              variables : string list,
                              preconditions : predicate list,
                              effects : predicate list }
datatype action_instance = Instance of { bindings : string StringMap.map,
                                         action : action }
datatype problem = Problem of { actions : action list,
				objects : string list,
                                start : state,
				(* INVARIANT: There are no variables in goal *)
                                goal : predicate list }

(* fun isGoal (Problem {goal,...}) state = FluentSet.equal (goal, state) *)

fun getFluentsByName name state =
    List.filter (fn Fluent {name=flun,...} => flun = name) (FluentSet.listItems state)

(* Type: problem -> state -> action_instance list *)
fun possibleActions (Problem {actions, objects,...}) state =
    let
	fun allInstances (action as Action {preconditions,...}) = 
	    let
		(* All bindings creating an instance of action that can be applied to state *)
		fun instances [] bindings = 
		    [ bindings ]
		  | instances ((Predicate {truth, name, arguments}) ::preds) bindings =
		    let
			val fluentArgs = map (fn Fluent {arguments,...} => arguments) 
					     (getFluentsByName name state)
			fun takeEqual v (l::ls) =
			    if v = l then SOME ls
			    else          NONE
			(* All bindings that make Predicate match at least one of fluentArgs. *)
			(* fluentArgs are the arguments of the fluents of the same name *)
			fun trueBinds _        []        _  = []
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
			(if truth then trueBinds else falseBinds) bindings fluentArgs arguments
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
