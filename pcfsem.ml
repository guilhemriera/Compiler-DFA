open Pcfast ;;
exception SemanticError of string

let check_state_declared state states =
  if not (List.mem state states) then
    raise (SemanticError ("State not declared: " ^ state))

let check_letter_declared letter letters =
  if not (List.mem letter letters) then
    raise (SemanticError ("Letter not declared: " ^ letter))

let check_states_distinct states =
  let sorted_unique_states = List.sort_uniq String.compare states in
  if List.length sorted_unique_states <> List.length states then
    raise (SemanticError "Duplicate states detected")

let check_unique_transitions transitions =
  let pairs = List.map (fun transition -> (transition.letter, transition.state_start)) transitions in
  let sorted_unique_pairs = List.sort_uniq compare pairs in
  if List.length sorted_unique_pairs <> List.length pairs then
    raise (SemanticError "Duplicate transitions detected")




let rec dfs visited state transitions =
  if List.mem state visited then visited
  else
    let next_states = List.fold_left (fun acc transition ->
      if transition.state_start = state then transition.state_end :: acc else acc
    ) [] transitions in
    let visited = state :: visited in
    List.fold_left (fun acc state -> dfs acc state transitions) visited next_states

let check_all_states_reachable automaton =
  check_state_declared automaton.initial automaton.states;
  let reachable_states = dfs [] automaton.initial automaton.transitions in
  let unreachable_states = List.filter (fun state -> not (List.mem state reachable_states)) automaton.states in
  if unreachable_states <> [] then
    Printf.printf "Warning: The following states are not reachable from the initial state, this part isn't use: %s\n" (String.concat ", " unreachable_states)
  ;;


    let rec f_include st i str progress =
  if i == (String.length st) then
    true
  else if (String.get st i) == (String.get str progress) then
    f_include st (i+1) str (progress+1)
  else 
    false
;;

let rec no_concat_by_list str list progress list_init = 
  match list with
  | [] -> true
  | st :: r -> (
    if ((String.length st) < (String.length str)) 
    then (
      if (progress == String.length str) 
      then false
      else if (f_include st 0 str progress) 
      then (
        no_concat_by_list str list_init (progress + String.length st) list_init
      ) else (
        no_concat_by_list str r progress list_init
      )
    )
    else (
      no_concat_by_list str r progress list_init
    )
  )
;;




let check_no_concat list =
  (let rec check_no_concat_aux list list_init =
  match list with
  | [] -> true;
  | x::r -> (if (no_concat_by_list x list_init 0 list_init;) 
    then (check_no_concat_aux r list_init;)
    else false;
    )
  in check_no_concat_aux list list) 
  && 
  (let rec not_duplicate list =
  match list with
  | [] -> true
  | x::r -> (if (List.mem x r) then false else not_duplicate r)
  in not_duplicate list)
;;

let check_automaton automaton =
  (* Check unambiguous letter *)
  if not (check_no_concat automaton.letters) then
    raise (SemanticError ("Ambiguous letter : word might can have several spellings"))

 
  
  (* Check initial state *)
  check_state_declared automaton.initial automaton.states;

  (* Check final states *)
  List.iter (fun state -> check_state_declared state automaton.states) automaton.final;

  (* Check duplicate *)
  check_states_distinct automaton.final;
  check_states_distinct automaton.states;
  

  (* Check transitions *)
  List.iter (fun transition ->
    check_state_declared transition.state_start automaton.states;
    check_letter_declared transition.letter automaton.letters;
    check_state_declared transition.state_end automaton.states;
  ) automaton.transitions;
  check_unique_transitions automaton.transitions;
  check_all_states_reachable automaton;