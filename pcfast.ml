(* Ce fichier contient la d�finition du type OCaml des arbres de
 * syntaxe abstraite du langage, ainsi qu'un imprimeur des phrases
 * du langage.
 *)
type transition = {
  state_start : string;
  letter : string;
  state_end : string;
}


type automaton = {
  letters : string list;
  states : string list;
  initial : string;
  final : string list;
  transitions : transition list;
  file : string;
}
;;

let get_transitions state transitions =
  transitions
  |> List.filter (fun t -> t.state_start = state)
  |> List.map (fun t -> (t.letter, t.state_end))
;;

    open Printf;; 

let print_letter oc letter = 
  Printf.fprintf oc "%s" letter
;;

let rec print_letters oc = function
  | [] -> Printf.fprintf oc ""
  | letter::[] -> Printf.fprintf oc "%a" print_letter letter
  | letter::r -> Printf.fprintf oc "%a , %a" print_letter letter print_letters r

let print_state oc state = 
  Printf.fprintf oc "%s" state
;;

let rec print_states oc = function
  | [] -> Printf.fprintf oc ""
  | state::[] -> Printf.fprintf oc "%a" print_state state
  | state::r -> Printf.fprintf oc "%a %a" print_state state print_states r
;;

let rec print_letters oc = function
  | [] -> Printf.fprintf oc ""
  | letter::r -> Printf.fprintf oc "%s %a" letter print_letters r
;;

let print_transition oc transition = 
  Printf.fprintf oc "(%s - %s -> %s)" transition.state_start transition.letter transition.state_end
;;

  let rec print_transitions oc = function
  | [] -> Printf.fprintf oc ""
  | transition::[] -> Printf.fprintf oc "%a\n\t" print_transition transition
  | transition::r -> Printf.fprintf oc "%a\n\t\t%a" print_transition transition print_transitions r
;;

let print_file oc file = 
  Printf.fprintf oc "%s" file
;;

let print_automaton oc automaton = 
  Printf.fprintf oc "automaton {\n";
  Printf.fprintf oc "\tletter = [%a]\n" print_letters automaton.letters;
  Printf.fprintf oc "\tstate = [%a]\n" print_states automaton.states;
  Printf.fprintf oc "\tinit  = %s\n" automaton.initial;
  Printf.fprintf oc "\tfinal = [%a]\n" print_states automaton.final;
  Printf.fprintf oc "\ttransition = [\n\t\t%a] \n}" print_transitions automaton.transitions;
  Printf.fprintf oc " file = %s\n" automaton.file
;;
