open Pcfast ;;
exception SemanticError of string

let check_state_declared state states =
  if not (List.mem state states) then
    raise (SemanticError ("State not declared: " ^ state))

let check_letter_declared letter letters =
  if not (List.mem letter letters) then
    raise (SemanticError ("Letter not declared: " ^ letter))

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

let rec check_no_concat_aux list list_init =
  match list with
  | [] -> true;
  | x::r -> (if (no_concat_by_list x list_init 0 list_init;) 
    then (check_no_concat_aux r list_init;)
    else false;
    )
  ;;

let rec not_duplicate list =
  match list with
  | [] -> true
  | x::r -> (if (List.mem x r) then false else not_duplicate r)
  ;;

let check_no_concat list =
  (check_no_concat_aux list list) && (not_duplicate list)
;;

let check_automaton automaton =
  (* Check unambiguous letter *)
  if not (check_no_concat automaton.letters) then
    raise (SemanticError ("Ambiguous letter : word might can have several spellings"))

  (* Check initial state *)
  check_state_declared automaton.initial automaton.states;

  (* Check final states *)
  List.iter (fun state -> check_state_declared state automaton.states) automaton.final;

  (* Check transitions *)
  List.iter (fun transition ->
    check_state_declared transition.state_start automaton.states;
    check_letter_declared transition.letter automaton.letters;
    check_state_declared transition.state_end automaton.states;
  ) automaton.transitions

