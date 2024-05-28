let version = "0.01" ;;

let rec index_of lst x =
  match lst with
  | [] -> raise Not_found
  | h :: t -> if h = x then 0 else 1 + index_of t x
;;

let print_trans_c oc states transitions =
  let () = Printf.fprintf oc "
int transition(char* states[], int statesSize, char* result[], int resultSize, char* final[], int finalSize, int* currentState) {
\tif (resultSize == 0) {
\t\tfor (int i = 0; i<finalSize ; i++) {
\t\t\tif (states[*currentState] == final[i]) {
\t\t\t\treturn 1;
\t\t\t}
\t\t} return 0;
\t}"; in
  let rec print_trans_c_rec oc rest_states transitions all_states =
  match rest_states with
  | [] -> Printf.fprintf oc "}\n\n\n"
  | state :: rest -> 
      (let () = Printf.fprintf oc "\tif (states[*currentState] == \"%a\") {\n\t\t" Pcfast.print_state state in
      let array = Pcfast.get_transitions state transitions in
      List.iter (fun (letter, next) ->
        Printf.fprintf oc 
"if (result[0] == \"%s\") {
\t\t\t*currentState = %d;
\t\t\treturn transition(states, statesSize, result + 1, resultSize-1, final, finalSize, currentState);
\t\t} else " letter (index_of all_states next)
      ) array;
      Printf.fprintf oc "{
\t\t\treturn transition(states, statesSize, result + 1, resultSize-1, final, finalSize, currentState);
\t\t}\n\t}\n";
    (print_trans_c_rec oc rest transitions all_states);
        )  
    in
  print_trans_c_rec oc states transitions states;
;;


let print_split_c oc =
  Printf.fprintf oc "int split(char* alphabet[], int alphabetSize, char* word, char* result[], int* resultSize) {\n
\tif (*word == \'\\0\') {\n
\t\treturn 1;\n
\t}\n

\tfor (int i = 0; i < alphabetSize; i++) {
\t\tint len = strlen(alphabet[i]);
\t\tif (strncmp(word, alphabet[i], len) == 0) {
\t\t\tresult[*resultSize] = alphabet[i];
\t\t(*resultSize)++;
\t\t\tif (split(alphabet, alphabetSize, word + len, result, resultSize)) {
\t\t\t\treturn 1;
\t\t\t}
\t\t\t(*resultSize)--;
\t\t}
\t}

    return 0;
}\n\n"
;;

let rec print_list_string oc = function
  | [] -> Printf.fprintf oc ""
  | letter::[] -> Printf.fprintf oc "\"%s\"" letter
  | letter::r -> Printf.fprintf oc "\"%s\" , %a" letter print_list_string r
;;


let print_initiation_c oc letters states init final =
  Printf.fprintf oc "\tchar* alphabet[] = {%a};
\tint alphabetSize = %d;

\tchar* states[] = {%a};
\tint statesSize = %d;

\tint init = %d;

\tchar* final[] = {%a};
\tint finalSize = %d;" print_list_string letters (List.length letters) print_list_string states (List.length states) (index_of states init) print_list_string final (List.length final) ;
  Printf.fprintf oc "\n" 
;;

let print_main_c oc letters states init final =
  Printf.fprintf oc "int main(int argc, char* argv[]) {
\tif (argc > 2) {
\t\tprintf(\"Usage: %%s <char* word>\\n\", argv[0]);
\t\treturn 1;}

\tchar* word = (argc > 1) ? argv[1] : \"\";

\tchar* result[MAX_STRINGS];
\tint resultSize = 0;";
print_initiation_c oc letters states init final ;

  Printf.fprintf oc 
"\tif (split(alphabet, alphabetSize, word , result, &resultSize))
\t{
\t\tint currentState = init;
\t\tif (transition(states, statesSize, result, resultSize, final, finalSize, &currentState))
\t\t{
\t\t\tprintf(\"true\\n\");
\t\t}
\t\telse
\t\t{
\t\t\tprintf(\"false\\n\");
\t\t}
\t}
\telse
\t{
\t\tprintf(\"The word contains unkown letters\\n\");
\t}

\treturn 0;
}";
;;

let usage () =
  let _ =
    Printf.eprintf
      "Usage: %s [file]\n\tRead a PCF program from file (default is stdin)\n%!"
    Sys.argv.(0) in
  exit 1
;;

let main () =
  let input_channel =
    match Array.length Sys.argv with
      1 -> stdin
    | 2 ->
        begin match Sys.argv.(1) with
          "-" -> stdin
        | name ->
            begin try open_in name with
              _ -> Printf.eprintf "Opening %s failed\n%!" name; exit 1
            end
        end
    | n -> usage()
  in
  let lexbuf = Lexing.from_channel input_channel in
  let _ = Printf.printf "        Welcome to PCF, version %s\n%!" version in
  while true do
    try 
      let _ = Printf.printf  "> %!" in
      let auto = Pcfparse.main Pcflex.lex lexbuf in
      let _ = Pcfast.print_automaton stdout auto in
      let _ = Pcfsem.check_automaton auto in
      (* let array = Pcfast.transitions_to_array auto.states auto.transitions in *)
      let () = 
        let oc = open_out (auto.file ^ ".c") in
        Printf.fprintf oc 
"
#include <stdio.h>
#include <string.h>

#define MAX_STRINGS 100
#define MAX_STRING_LENGTH 100

" ;
      print_trans_c oc auto.states auto.transitions ;
      
      print_split_c oc ;

      print_main_c oc auto.letters auto.states auto.initial auto.final ;

        close_out oc;
      in
      Printf.printf "\n%!" ;
    with
    | Pcflex.Eoi -> (
      Printf.printf  "Bye.\n%!" ; exit 0
    )
    | Failure msg -> Printf.printf "Erreur: %s\n\n" msg
    | Parsing.Parse_error ->
        let sp = Lexing.lexeme_start_p lexbuf in
        let ep = Lexing.lexeme_end_p lexbuf in
        Format.printf
          "File %S, line %i, characters %i-%i: Syntax error.\n"
          sp.Lexing.pos_fname
          sp.Lexing.pos_lnum
          (sp.Lexing.pos_cnum - sp.Lexing.pos_bol)
          (ep.Lexing.pos_cnum - sp.Lexing.pos_bol)
    | Pcflex.LexError (sp, ep) ->
        Printf.printf
          "File %S, line %i, characters %i-%i: Lexical error.\n"
          sp.Lexing.pos_fname
          sp.Lexing.pos_lnum
          (sp.Lexing.pos_cnum - sp.Lexing.pos_bol)
          (ep.Lexing.pos_cnum - sp.Lexing.pos_bol)
  done
;;

if !Sys.interactive then () else main();;
