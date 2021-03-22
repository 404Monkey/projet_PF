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
let is_well_formed token_list =
  let is_well_formed_aux =
    fold_left(
        fun cpt elem ->
        match elem with
        | Variable(value) -> cpt-1
        | Number(value) -> cpt-1
        | End | Minus -> cpt
        | _ -> cpt+1
      ) 0 token_list in
  is_well_formed_aux = -1
;;

let test_well_formed = string_to_token_list "13 2 5 * 1 1 / - + ;";;

is_well_formed test_well_formed;;
                  

(* définitions des types pour les arbres de syntaxe abstraite*)
type operator = | Plus | Minus | Mult | Div;;
type tree =
  | Var of char
  | Cst of int
  | Unary of tree
  | Binary of operator * tree * tree
;;

(* fonction qui transforme un token en operator *)
let token_to_operator elem =
  match elem with
  | Add -> Plus
  | Subtract -> Minus
  | Multiply -> Mult
  | Divide -> Div
  | _ -> invalid_arg "on ne peut pas convertir ce token en operator"
;;

(* fonction qui transfome une liste de token en arbre *)
let parse_aux token_list =
  let stack =
    fold_left(
        fun cumul elem ->
        match elem with
        | Variable(value) -> Var(value)::cumul
        | Number(value) -> Cst(value)::cumul
        | Minus -> let elem1 = (hd cumul) and pile = (tl cumul) in Unary(elem1)::pile
        | End -> cumul
        | _ -> let elem1 = (hd (tl cumul)) and elem2 = (hd cumul) and pile = (tl(tl cumul)) in
               (Binary(token_to_operator(elem), elem1, elem2)::pile)
      ) [] token_list in
  hd stack
;;

let rec parse token_list =
  if(is_well_formed token_list)
  then parse_aux token_list
  else failwith "not a Lukasiewicz word" (* mettre autre chose qu'un failwith *)
;;

let t1 = parse test_well_formed;;

(* PARTIE II *)
(* Simplification sur l'arbre *)

(* fonction qui simplifie  *)
let simplificate_cst op =
  match op with
  | Plus -> (+)
  | Minus -> (-)
  | Mult -> ( * )
  | Div -> (/)
;;

(* proposition concluante+opti *)
let simplificate_var op e1 e2 func =
  match (op, e1, e2) with
  | (Mult, var, Cst(1)) | (Mult, Cst(1), var) | (Plus, var, Cst(0)) | (Plus, Cst(0), var) -> var
  | (Mult, var, Cst(0)) | (Mult, Cst(0), var) -> Cst(0)
  | (Minus, Var(x), Var(y)) -> if(x=y) then Cst(0) else Binary(op, e1, e2)
  | (Div, Var(x), Var(y)) -> if(x=y) then Cst(1) else Binary(op, e1, e2)
  | (_, Var(x), Var(y)) -> Binary(op, e1, e2)
  | (_, _, _) -> func(Binary(op, func e1, func e2))
;;
let rec simplificate tree =
  match tree with 
  | Cst(x) -> tree
  | Var(x) -> tree
  | Unary(x) -> (
    match x with
    | Cst(elem) -> Cst(-elem)
    | _ -> simplificate(Unary(simplificate x))
  )
  | Binary(op, e1, e2) -> (
    match(op, e1, e2) with
    | (_, Cst(number1), Cst(number2)) -> Cst(simplificate_cst op number1 number2)
    | (_, Var(x), _) | (_, _, Var(x)) -> simplificate_var op e2 e1 simplificate
    | (_, _, _) -> simplificate(Binary(op, simplificate e1, simplificate e2))
  ) 
;;
let t0_list = string_to_token_list "x 3 2 - * ;";;
let t0 = parse t0_list;;
simplificate t0;;


(* Tests classiques *)

let t2_list = string_to_token_list "1 2 + 4 3 - * ~;";;
let t2 = parse t2_list;;
simplificate t2;;

let t3_list = string_to_token_list "1 x * y 0 + * ;";;
let t3 = parse t3_list;;
simplificate t3;;

let t4_list = string_to_token_list "x x - x x / + ~ ;";;
let t4 = parse t4_list;;
simplificate t4;;

simplificate t1;;



(* PARTIE III *)
(* Affichage du résultat *)


(* fonction qui transfrome un arbre en expression *)
let string_of_char = String.make 1;;
string_of_char 'c';;
let rec display_expr tree =
  match tree with
  | Cst(x) -> string_of_int x
  | Var(x) -> string_of_char x (*trouver le moyen de convertir char en string *)
  | Unary(x) ->
     begin
       match x with
       | Cst(y) -> "(-" ^ display_expr x ^")"
       | Var(y) -> "(-" ^ display_expr x ^")"
       (*| Var(x) -> "(-" ^ string_of_char x ^ ")"*)
       | _ ->  "-(" ^ display_expr x ^ ")"
     end
  | Binary(op, e1, e2) ->
     begin
       let (disp_e1, disp_e2) = 
         match (e1, e2) with
         | (Cst(x),Binary(_,_,_)) | (Cst(x),Unary(_)) -> (display_expr e1,"(" ^ display_expr e2 ^")")
         | (Var(x),Binary(_,_,_)) | (Var(x),Unary(_)) -> (display_expr e1,"(" ^ display_expr e2 ^")")
         | (Binary(_,_,_),Cst(x)) | (Unary(_),Cst(x)) -> ("(" ^ display_expr e1 ^ ")",display_expr e2)
         | (Binary(_,_,_),Var(x)) | (Unary(_),Var(x)) -> ("(" ^ display_expr e1 ^ ")",display_expr e2)
         | (Binary(_,_,_),Binary(_,_,_)) | (Binary(_,_,_),Unary(_)) | (Unary(_),Binary(_,_,_)) -> ("(" ^ display_expr e1 ^ ")","(" ^ display_expr e2 ^ ")")
         | (_,_) -> (display_expr e1, display_expr e2)
       in
       match op with
       | Plus -> disp_e1 ^ "+" ^ disp_e2
       | Minus -> disp_e1 ^ "-" ^ disp_e2
       | Mult -> disp_e1 ^ "*" ^ disp_e2
       | Div -> disp_e1 ^ "/" ^ disp_e2
     end             
;;
display_expr t0;;
display_expr t2;;
display_expr t3;;
display_expr t4;;


(* PARTIE IV *)
(* Programme final *)


let main string =
  let list = string_to_token_list string in
  let tree = parse list in
  Printf.printf "%s" (display_expr tree);
  let tree_simplificate = simplificate tree in
  Printf.printf "%s" (display_expr tree_simplificate);
;;

main "x 3 + 5 7 + + 3 4 * 1 3 + / / ;";;
