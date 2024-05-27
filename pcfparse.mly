%{
  open Pcfast;;

  (*let rec body params expr = match params with
  | [] -> expr
  | p :: prms -> Fun(p, body prms expr)
  ;;*)
%}

%token <string> TYPE_LETTER
%token <string * bool * bool> TYPE_STATE
%token <(string * bool * bool) * string * (string * bool * bool)> TYPE_TRANSITION
%token <string> IDENT
%token LPAR RPAR SEMISEMI
%token LETTER STATE INIT FINAL TRANSITION BACKARROW FRONTARROW SEMI

%start main
%type <Pcfast.expr> main

%%

main: expr SEMISEMI { $1 } /* peut-etre a jetter (pas sur de voir l'intéret d'un évaluation intermédière dans mon code) */
;

expr:
  LETTER TYPE_LETTER seqident SEMI expr                 { Letter($2, $5) }                    
  /* prototipe chaine { 
    match $3 with
    | [] -> Letter($2)
    | lst -> 
      let letters = List.map (fun id -> Letter(id)) lst in
      List.fold_right (fun l acc -> Seq(l, acc)) letters (Letter($2))
  }*/
| STATE IDENT seqident SEMI expr                  { State(($2, false, false), $5) }
| STATE IDENT seqident INIT SEMI expr             { State(($2, true, false), $6) }
| STATE IDENT seqident FINAL SEMI expr            { State(($2, false, true), $6) }
| STATE IDENT seqident INIT FINAL SEMI expr       { State(($2, true, true), $7) }
| TRANSITION IDENT BACKARROW IDENT FRONTARROW IDENT SEMI expr { Transition(($2, $4, $6), $8) }
;

atom:
  TYPE_LETTER         { LetterType($1) }
| TYPE_STATE          { StateType($1) }
| TYPE_TRANSITION     { TransitionType($1) } 
| IDENT               { Ident($1) }
| LPAR expr RPAR { $2 }
;

seqident:
  IDENT seqident  { $1 :: $2 }
| /* rien */      { [ ] }
;
