(** AUTHORS : 
    
    ANDRIANARIVONY HENINTSOA 
    && 
    CHEVAIS BENJAMIN 

 **)

(* Décommenter la commande '#directory ...' selon votre version d'OCaml *)

(* OCaml version 4.02.1 *)
(* #directory "libraries/ocaml-4.02.1+ocp1/";; *)

(* OCaml version 4.02.3 *)
(* #directory "libraries/ocaml-4.02.3/";; *)

(* OCaml version 4.05.0 *)
#directory "libraries/ocaml-4.05.0/";;

(* OCaml version 4.08.1 *)
(* #directory "libraries/ocaml-4.08.1/";; *)

(* OCaml version 4.10.0 *)
(* #directory "libraries/ocaml-4.10.0/";; *)

(* OCaml version 4.11.1 *)
(* #directory "libraries/ocaml-4.11.1/";; *)

#load "expression_scanner.cmo";;
open Expression_scanner;;
open List;;


(* PARTIE I *)
(* analyse syntaxique et construction de l'arbre *)

(* fonction vérifiant si un mot de Lukasiewicz postfixé est bien formé *)
let rec is_well_formed_aux token_list =
  match token_list with
  | [] -> 0
  | elem::tail -> (
    match elem with
    | Variable(value) -> -1 + (is_well_formed_aux tail)
    | Number(value) -> -1 + (is_well_formed_aux tail)
    | End -> 0 + (is_well_formed_aux tail)
    | Minus -> 0 + (is_well_formed_aux tail)
    | _ -> 1 + (is_well_formed_aux tail)
  )
;;

let is_well_formed token_list =
  is_well_formed_aux token_list = -1
;;

let test_well_formed = string_to_token_list "13 2 5 * 1 0 / - + ;";;

is_well_formed test_well_formed;;

(* définitions des types pour les arbres *)
type operator = | Plus | Minus | Mult | Div;;
type tree =
  | Var of char
  | Cst of int
  | Unary of tree
  | Binary of operator * tree * tree
;;

let token_to_operator elem =
  match elem with
  | Add -> Plus
  | Subtract -> Minus
  | Multiply -> Mult
  | Divide -> Div
  | _ -> invalid_arg "on ne peut pas convertir ce token en operator"
;;

(* fonction qui transfome une liste de token en arbre *)
let rec parse_aux token_list stack=
  match token_list with
  | [] -> hd stack
  | elem::tail -> (
    match elem with
    | Variable(value) -> parse_aux tail (Var(value)::stack)
    | Number(value) -> parse_aux tail (Cst(value)::stack)
    | Minus -> let elem1 = (hd stack) and pile = (tl stack) in
               parse_aux tail (Unary(elem1)::pile)
    | End -> hd stack
    | _ -> let elem1 = (hd (tl stack)) and elem2 = (hd stack) and pile = (tl(tl stack)) in
           parse_aux tail (Binary(token_to_operator(elem), elem1, elem2)::pile)
  )
;;
(* trouver un moyen de rassembler tous les opérateurs, le '_' ne marche pas *)

let rec parse token_list =
  if(is_well_formed token_list)
  then parse_aux token_list []
  else failwith "not a Lukasiewicz word" (* mettre autre chose qu'un failwith *)
;;

parse test_well_formed;;




(* PARTIE II *)
(* Simplification sur l'arbre *)


(* fonction qui simplifie l'arbre (peut-etre faire plein de 
sous fonctions pour que ce soit plus lisible *)
let rec simplificate_tree tree =

;;





(* PARTIE III *)
(* Affichage du résultat *)


(* fonction qui transfrome un arbre en expression *)
let display_expr tree =

;;





(* PARTIE IV *)
(* Programme final *)


let main =

;;
