(* Ce fichier contient la d�finition du type OCaml des arbres de
 * syntaxe abstraite du langage, ainsi qu'un imprimeur des phrases
 * du langage.
 *)

type expr =
  | LettreType of string
  | StateType of string * bool * bool
  | TransitionType of expr * expr * expr
  | Ident of string
  | Lettre of expr * expr
  | State of expr * expr
  | Transition of expr * expr
;;

open Printf;; 

let rec print oc e =
  match e with
  | LettreType l -> fprintf oc "%s" l
  | StateType (s, b1, b2) -> fprintf oc "%s %s %s" 
                          s 
                          (if b1 then "initial" else "") 
                          (if b2 then "final" else "")
  | TransitionType (s1, l, s2) -> fprintf oc "(%a - %a -> %a)" print s1 print l print s2
  | Ident s -> fprintf oc "%s" s
  | Lettre (l, e) -> fprintf oc "Lettre %a ; \n %a" print l print e
  | State (st, e) -> fprintf oc "State %a ; \n %a" print st print e
  | Transition (tr, e) -> fprintf oc "Tran %a ; \n %a" print tr print e
;;