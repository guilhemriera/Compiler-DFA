let version = "0.01" ;;

let rec index_of lst x =
  match lst with
  | [] -> raise Not_found
  | h :: t -> if h = x then 0 else 1 + index_of t x
;;

let rec print_trans_c oc states transitions =
  match states with
  | [] -> ()
  | state :: rest -> 
      let _ = Printf.fprintf oc "\tif (states[*currentState] == \"¨%a\") {\n" Pcfast.print_state state in
      let array = Pcfast.get_transitions state transitions in
      List.iter (fun (letter, next) ->
        Printf.fprintf oc 
"\t\t if (result[0] == \"%s\") {\n
\t\t\t  *currentState = %d;\n
\t\t\t  return transition(states, statesSize, result + 1, final, finalSize, currentState);\n
\t\t  }\n" letter (index_of states next)
      ) array;
    Printf.fprintf oc "\t}\n";
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
#include <stdio.h>\n
#include <string.h>\n
\n
#define MAX_STRINGS 100\n
#define MAX_STRING_LENGTH 100\n
\n
int transition(char* states[], int statesSize, char* result[], char* final[], int finalSize, int* currentState) {\n
\t    if (result == []) {\n
\t\t        for (int i = 0; i<finalSize ; i++) {\n
\t\t\t            if (states[*currentState] == final[i]) {\n
\t\t\t\t                return 0;\n
\t\t\t            }\n
\t\t        }\n
\t        return 1;\n
\t    }\n" ;
      print_trans_c oc auto.states auto.transitions ;
      Printf.fprintf oc "\n}\n" ;
      
      Printf.fprintf oc "int split(char* alphabet[], int alphabetSize, char* word, char* result[], int* resultSize) {\n
    if (*word == '\0') {\n
        return 1;\n
    }\n
\n
    for (int i = 0; i < alphabetSize; i++) {\n
        int len = strlen(alphabet[i]);\n
        if (strncmp(word, alphabet[i], len) == 0) {\n
            result[*resultSize] = alphabet[i];\n
            (*resultSize)++;\n
            if (split(alphabet, alphabetSize, word + len, result, resultSize)) {\n
                return 1;\n
            }\n
            (*resultSize)--;\n
        }\n
    }\n
\n
    return 0;\n
}



int main(argv, argc) {\n
";
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
