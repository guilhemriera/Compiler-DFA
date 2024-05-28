%{
  open Pcfast;;

%}

%token <int> INT
%token <string> IDENT
%token <string> STRING
%token AUTOMATON LETTER STATE INIT FINAL TRANSITION
%token LPAR RPAR LCURLBRAC RCURLBRAC LSQUARBRAC RSQUARBRAC SEMISEMI SEMI
%token EQUAL BACKARROW FRONTARROW
%left EQUAL


%start main
%type <Pcfast.automaton> main

%%

main: automaton SEMISEMI { $1 }
    | SEMISEMI main { $2 }
;


automaton:
| AUTOMATON LCURLBRAC letters states initial final transitions RCURLBRAC file
    {
      {
        letters = $3;
        states = $4;
        initial = $5;
        final = $6;
        transitions = $7;
        file = $9;
      
      }
     }
;

letters:
  LETTER EQUAL LSQUARBRAC RSQUARBRAC SEMI { [] }
| LETTER EQUAL LSQUARBRAC listing RSQUARBRAC SEMI { $4 }
;

states:
  STATE EQUAL LSQUARBRAC RSQUARBRAC SEMI { [] }
| STATE EQUAL LSQUARBRAC listing RSQUARBRAC SEMI { $4 }
;

initial:
  INIT EQUAL IDENT SEMI { $3 }
;

final:
  FINAL EQUAL LSQUARBRAC RSQUARBRAC SEMI { [] }
| FINAL EQUAL LSQUARBRAC listing RSQUARBRAC SEMI { $4 }
;

listing:
 IDENT { [$1] }
| listing IDENT { $1 @ [$2] }
;

transitions:
  TRANSITION EQUAL LSQUARBRAC RSQUARBRAC SEMI { [] }
| TRANSITION EQUAL LSQUARBRAC transition_list RSQUARBRAC SEMI { $4 }
;

transition_list:
  transition { [$1] }
| transition_list transition { $1 @ [$2] }
;

transition:
  LPAR IDENT BACKARROW IDENT FRONTARROW IDENT RPAR { 
    {
      state_start = $2;
      letter = $4;
      state_end = $6;
    }
  }
;

file:
  IDENT { $1 }