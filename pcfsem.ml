open Pcfast ;;

type pcfval =
  | LettreVal of string
  | StateVal of string * bool * bool
  | TransitionVal of expr * expr * expr
;;

let rec printval = function
  | LettreVal l -> Printf.sprintf "Lettre %s" l
  | StateVal (s, b1, b2) -> Printf.sprintf "State %s" s
  | TransitionVal (s1, l, s2) -> Printf.sprintf "Transition %s -%s-> %s" s1 l s2
;;

type env = string -> pcfval ;;

let init_env id =
  raise (Failure (Printf.sprintf "Unbound ident: %s" id))
;;

let extend env x v = fun y -> if x = y then v else env y ;;

let rec semant e rho =
  match e with
  | LettreType l -> LettreVal l
  | StateType (s, b1, b2) -> StateVal (s, b1, b2)
  | TransitionType (s1, l, s2) -> (
    TransitionVal ((semant s1 rho), (semant l rho), (semant s2 rho))
  )
  | Ident v -> rho (v)
  | Lettre (l, e) -> (
    semant e (extend rho l (LettreVal l))
  )
  | State (s, e) -> (
    semant e (extend rho s (StateVal (s, b1, b2)))
  )
  | Transition (s1, l, s2, e) -> (
    semant e (extend rho (s1^"-"^l^"->"^s2) (TransitionVal (semant s1 rho), (semant l rho), (semant s2 rho)))
  ) 


;;

let eval e = semant e init_env ;;
