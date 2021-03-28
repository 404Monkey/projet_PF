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
let simpl_cst op =
  match op with
  | Plus -> (+)
  | Minus -> (-)
  | Mult -> ( * )
  | Div -> (/)
;;

(* proposition concluante+opti *)
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

let t0_list = string_to_token_list "x 3 2 - * ;";;
let t0 = parse t0_list;;
simplificate t0;;


(* Tests classiques *)

let t2_list = string_to_token_list "1 2 + 4 3 - * ;";;
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

let string_of_char = String.make 1;;

(* fonction qui transfrome un arbre en expression *)

let string_of_operator op =
  match op with
  | Mult -> "*"
  | Div -> "/"
  | Plus -> "+"
  | Minus -> "-"
;;
       

;;
         
let rec tree_to_expr tree =
  let simplificate_expr op e1 e2 op1 op2 func =
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
         | (Binary(op1,_,_),Binary(op2,_,_)) -> simplificate_expr op e1 e2 op1 op2 tree_to_expr
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
tree_to_expr t0;;
tree_to_expr t2;;
tree_to_expr t3;;
tree_to_expr t4;;
let t_exemple_list_0 = string_to_token_list "a 2 2 ~ * + ;";;
let t_exemple_0 = parse t_exemple_list_0;;
tree_to_expr t_exemple_0;;
let t_exemple_list = string_to_token_list "a b * c * e f + * ;";;
let t_exemple = parse t_exemple_list;;
tree_to_expr t_exemple;;

(* A CONSERVER JUSQU'A CE QU'ON SOIT BON

let rec tree_to_expr tree =
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
         | (Binary(_,_,_),Binary(_,_,_)) -> ("(" ^ tree_to_expr e1 ^ ")","(" ^ tree_to_expr e2 ^ ")")
         | (_,Binary(_,_,_)) -> (tree_to_expr e1,"(" ^ tree_to_expr e2 ^")")
         | (Binary(_,_,_),_) -> ("(" ^ tree_to_expr e1 ^ ")",tree_to_expr e2)
         | _ -> (tree_to_expr e1, tree_to_expr e2)
       in
       expr_e1 ^ string_of_operator op ^ expr_e2
     end             
;;

let rec string_of_tree tree =
  match tree with
  | Cst(n) -> string_of_int n
  | Var(x) -> string_of_char x
  | Unary(exp) -> "(-"^(string_of_tree exp)^")"
  | Binary(op, x, y) ->
     match op with
     | Plus | Minus -> (string_of_tree x)^(string_of_operator op)^(string_of_tree y)
     | _ ->
        match (x, y) with
        | (Binary(_,_,_), Binary(_,_,_)) -> "("^(string_of_tree x)^")"^(string_of_operator op)^"("^(string_of_tree y)^")"
        | (Binary(_,_,_), _) -> "("^(string_of_tree x)^")"^(string_of_operator op)^(string_of_tree y)
        | (_, Binary(_,_,_)) -> (string_of_tree x)^(string_of_operator op)^"("^(string_of_tree y)^")"
        | _ -> (string_of_tree x)^(string_of_operator op)^(string_of_tree y)
;;
string_of_tree t0;;
string_of_tree t2;;
string_of_tree t3;;
string_of_tree t4;;
let t_exemple_list = string_to_token_list "a b * c * e f + * ;";;
let t_exemple = parse t_exemple_list;;
string_of_tree t_exemple;;
 *)

let display_expr tree =
  Printf.printf "%s\n" (tree_to_expr tree)
;;

(* PARTIE IV *)
(* Programme final *)

let main() =
  let list = input_to_token_list() in
  let tree = parse list in
  Printf.printf "expression : \n";
  display_expr tree;
  let tree_simplificate = simplificate tree in
  Printf.printf "expression simplifié : \n";
  display_expr tree_simplificate;
;;

main();;
