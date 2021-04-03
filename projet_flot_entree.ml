(** AUTHORS :    
    
    ANDRIANARIVONY HENINTSOA 
    && 
    CHEVAIS BENJAMIN 
 
**)

open Expression_scanner;;
open List;;


(*==================== PARTIE I ====================*)
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

(* Définitions des types pour les arbres de syntaxe abstraite*)
type operator = | Plus | Minus | Mult | Div;;
type tree =
  | Var of char
  | Cst of int
  | Unary of tree
  | Binary of operator * tree * tree
;;

(* Fonction qui transforme un token en operator *)
let token_to_operator elem =
  match elem with
  | Add -> Plus
  | Subtract -> Minus
  | Multiply -> Mult
  | Divide -> Div
  | _ -> invalid_arg "on ne peut pas convertir ce token en operator"
;;

(* Fonctions qui transfoment une liste de token en arbre *)
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
let parse token_list =
  if(is_well_formed token_list)
  then parse_aux token_list
  else failwith "not a Lukasiewicz word"
;;


(*==================== PARTIE II ====================*)
(* Simplification sur l'arbre *)

(* Fonction associant une opération à la fonction correspondante  *)
let simpl_cst op =
  match op with
  | Plus -> (+)
  | Minus -> (-)
  | Mult -> ( * )
  | Div -> (/)
;;

(* Fonction simplifiant une expression *)
let simpl_tree op e1 e2 =
  if(e1 = e2)
  then
    match(op) with
    | Minus -> Cst(0)
    | Div -> Cst(1)
    | _ -> Binary(op, e1, e2)
  else
    match (op, e1, e2) with
    | (Mult, var, Cst(1))
      | (Mult, Cst(1), var)
      | (Plus, var, Cst(0))
      | (Plus, Cst(0), var) -> var
    | (Mult, var, Cst(0))
      | (Mult, Cst(0), var) -> Cst(0)
    | _ -> Binary(op, e1, e2)
;;

(* Fonction simplifiant l'arbre *)
let rec simplificate tree =
  match tree with 
  | Binary(op, e1, e2) -> (
    match(simplificate e1, simplificate e2) with
    | (Cst(number1), Cst(number2)) -> Cst(simpl_cst op number1 number2)
    | (simpl_e1, simpl_e2) -> simpl_tree op simpl_e1 simpl_e2
  )
  | Unary(x) -> Unary(simplificate x)
  | _ -> tree
;;


(*==================== PARTIE III ====================*)
(* Affichage du résultat *)


(* Fonction convertissant un char en string *)
let string_of_char = String.make 1;;


(* Fonction convertissant un opérateur au string correspondant *)
let string_of_operator op =
  match op with
  | Mult -> "*"
  | Div -> "/"
  | Plus -> "+"
  | Minus -> "-"
;;


(* Fonction qui transfrome un arbre en expression de type string *)
let rec tree_to_expr tree =
  let simplificate_expr op e1 e2 op1 op2 =
    if (op1=op)&&(op2=op)
    then (tree_to_expr e1, tree_to_expr e2)
    else if op1=op
    then (tree_to_expr e1,"(" ^ tree_to_expr e2 ^ ")")
    else if op2=op
    then ("(" ^ tree_to_expr e1 ^ ")", tree_to_expr e2)
    else ("(" ^ tree_to_expr e1 ^ ")","(" ^ tree_to_expr e2 ^ ")")
  in
  match tree with
  | Cst(x) -> string_of_int x
  | Var(x) -> string_of_char x 
  | Unary(x) ->
     begin
       match x with
       | Binary(_,_,_) | Unary(_) -> "-(" ^ tree_to_expr x ^ ")"
       | _ -> "(-" ^ tree_to_expr x ^")"
     end
  | Binary(op, e1, e2) ->
     begin
       let (expr_e1, expr_e2) = 
         match (e1, e2) with
         | (Binary(op1,_,_),Binary(op2,_,_)) -> simplificate_expr op e1 e2 op1 op2
         | (Binary(op1,_,_),_) ->
            begin
              if op1=op
              then (tree_to_expr e1, tree_to_expr e2)
              else ("(" ^ tree_to_expr e1 ^ ")",tree_to_expr e2)
            end
         | (_,Binary(op1,_,_)) ->
            begin
              if op1=op
              then (tree_to_expr e1, tree_to_expr e2)
              else (tree_to_expr e1,"(" ^ tree_to_expr e2 ^")")
            end
         | _ -> (tree_to_expr e1, tree_to_expr e2)
       in
       expr_e1 ^ string_of_operator op ^ expr_e2
     end             
;;

(* Fonction qui affiche une expression *)
let display_expr tree =
  Printf.printf "%s\n" (tree_to_expr tree)
;;


(*==================== PARTIE IV ====================*)
(* Programme final *)

let main_aux expr =
  let token_list = input_to_token_list() in
  let (res, temp) =
    fold_right(fun elem (final_list, sub_list) ->
        if (elem = End && sub_list <> [])
        then (sub_list::final_list, [End])
        else (final_list, elem::sub_list)
      )
      token_list ([], []) in
  temp::res
;;

let main expr =
  let list = main_aux expr in
  map (fun elem ->
      let tree = parse elem in
      Printf.printf "expression : \n";
      display_expr tree;
      let tree_simplificate = simplificate tree in
      Printf.printf "expression simplifié : \n";
      display_expr tree_simplificate;
      Printf.printf "=========== FIN =========== \n";)
    list
;;

main()
