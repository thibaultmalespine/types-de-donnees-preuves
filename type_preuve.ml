(*1 Fonctions d’ordre supérieur*)

let mod_2 x = x mod 2 = 0 ;;
predicat(2);;

let liste = [1;2;3;4;5];;

let rec map p liste = 
  match liste with
     e::liste -> p(e)::map(p)(liste) |
     [] -> [];;

map(mod_2)(liste);;

let rec for_all p liste = 
  match liste with 
    e::liste -> p(e) && for_all(p)(liste) |
    [] -> true;;

for_all(mod_2)(liste);;
for_all(mod_2)([2;4;6]);;
for_all(mod_2)([]);;


let rec exists p liste = 
  match liste with 
    e::liste -> p(e) || exists(p)(liste) |
    [] -> false ;;

exists(mod_2)(liste);;
exists(mod_2)([]);;

let rec filter p liste =
  match liste with 
    e::liste when p(e) = true -> e::filter(p)(liste) | 
    e::liste -> filter(p)(liste) | 
    [] -> [];;

filter(mod_2)(liste);;

let rec partition p liste =
  match liste with 
    e::liste when p(e) = true -> let l1, l2 = partition(p)(liste) in e::l1, l2 | 
    e::liste -> let l1, l2 = partition(p)(liste) in l1, e::l2 | 
    [] -> [],[];;

partition(mod_2)(liste);;

(*2 - Représentation de Caml en Caml*)
(*EXERCICE_1*)
type base_tp = BoolT | IntT;;

type tp = ConstT of base_tp | FunT of tp * tp;;

"Exemple : " int -> bool : 
FunT (ConstT IntT, ConstT BoolT)

1. int -> (bool -> int) : 
FunT (ConstT IntT, FunT (ConstT BoolT, ConstT IntT))

2. (int -> bool) -> int : 
FunT (FunT (ConstT IntT, ConstT BoolT), ConstT IntT)

3. (int -> int) -> (int -> int) : 
FunT (FunT (ConstT IntT, ConstT IntT), FunT (ConstT IntT, ConstT IntT))


(*EXERCICE_2*)

type const_expr = BoolE of bool | IntE of int;;

type expr = 
| Const of const_expr
| Var of string
| Abs of string * tp * expr
| App of expr * expr ;;

"Exemple : " (fun (x : int) -> x) 2  :
App (Abs ("x", ConstT IntT, Var "x"), Const (IntE 2))

1. ((fun (x : int) -> x) 2) 3 :
App ( App ( Abs("x", ConstT IntT, Var "x"), Const (IntE 2)), Const (IntE 3))

2. (fun (x : int) -> fun (y : bool) -> y) 2 3 :
App (App ( Abs ("x", ConstT IntT, Abs ("y", ConstT BoolT, Var "y")) , Const (IntE 2)), Const (IntE 3))

3. (fun (f:int -> bool) -> f 2) (fun (x:int) -> true) :
App (Abs("f", FunT(ConstT IntT, ConstT BoolT), App( Var "f", Const (IntE 2))) , Abs ("x", ConstT IntT, Const (BoolE true)))


(*EXERCICE_3*)

let rec string_of_type = function 
  | ConstT c -> (match c with 
    | BoolT -> "bool" 
    | IntT  -> "int")
  | FunT (e, s) -> "("^string_of_type(e) ^ " -> " ^ string_of_type(s)^")";;


(* TEST *)
string_of_type(FunT (ConstT IntT, ConstT BoolT));;
string_of_type(FunT (ConstT IntT, FunT (ConstT BoolT, ConstT IntT)));;
string_of_type(FunT (FunT (ConstT IntT, ConstT BoolT), ConstT IntT));;
string_of_type(FunT (FunT (ConstT IntT, ConstT IntT), FunT (ConstT IntT, ConstT IntT)));;

(*EXERCICE_4*)

let rec string_of_expr = function
  | Const c -> (match c with
    | BoolE b -> string_of_bool(b)
    | IntE i -> string_of_int(i)
  )
  | Var v -> v
  | Abs (s, t, e) -> "fun ("^s^" : "^string_of_type(t)^") -> "^string_of_expr(e)
  | App (e1, e2) -> "("^string_of_expr(e1)^") ("^string_of_expr(e2)^")";;

(* TEST *)
string_of_expr(App (Abs ("x", ConstT IntT, Var "x"), Const (IntE 2)));;
string_of_expr(App ( App ( Abs("x", ConstT IntT, Var "x"), Const (IntE 2)), Const (IntE 3)));;
string_of_expr(App (App ( Abs ("x", ConstT IntT, Abs ("y", ConstT BoolT, Var "y")) , Const (IntE 2)), Const (IntE 3)));;
string_of_expr(App (Abs("f", FunT(ConstT IntT, ConstT BoolT), App( Var "f", Const (IntE 2))) , Abs ("x", ConstT IntT, Const (BoolE true))));;


(* 3 - Interlude: Listes d’ association et maps *)

let f = function x -> x mod 2 = 0;;
(fun (f:int -> bool) -> f 2) (fun (x:int) -> true);;
(fun (f : (int -> bool)) -> (f) 2) (fun (x : int) -> true);;
(*3.1 Listes d’association*)

type 'a option = None | Some of 'a ;;

let exemple_assoc = [("Max", 10); ("Nicolas", 4); ("Nicole", 9)];;

(*EXERCICE_5*)
(* 1- *)
let rec lookup_assoc cle liste = 
  match liste with   
  (nom, age)::liste when nom = cle -> Some age|
  (nom, age)::liste -> lookup_assoc(cle)(liste) |
  [] -> None ;;
  
lookup_assoc "Nicolas" exemple_assoc;;
lookup_assoc "Maurice" exemple_assoc;;

(* 2- *)
let add_assoc (cle)(valeur)(liste) = (cle, valeur)::liste ;;  
add_assoc "Mathéo" 20 exemple_assoc;;

(* 3- *)
let rec remove_assoc (cle)(liste) =
  match liste with 
  | (nom, age)::liste -> if cle = nom then remove_assoc(cle)(liste) else (nom,age)::remove_assoc(cle)(liste) 
  | [] -> [];;

remove_assoc "Nicolas" exemple_assoc;;
remove_assoc "Max" exemple_assoc;;

(*3.2 Maps*)
type ('k, 'v) map = 'k -> 'v option;;

(*EXERCICE_6*)

(* 1- *)
let exemple_map : (string, int) map = fun key 
-> match key with 
|"Max" -> Some 10
|"Nicolas" -> Some 4
|"Nicole"-> Some 9
| _ -> None ;;


(*4 Règles de typage élémentaires*)

(*EXERCICE_7*)
type base_tp =
  BoolT
  | IntT;;

type tp =
  ConstT of base_tp
  | FunT of tp * tp;;

type const_expr =
  BoolE of bool
  | IntE of int;;

type expr =
  Const of const_expr
  | Var of string
  | Abs of string * tp * expr
  | App of expr * expr;;


exception Type_error of string;;

let rec tp_of_expr ctx expr =
  match expr with
  | Const (BoolE _) -> ConstT BoolT
  | Const (IntE _) -> ConstT IntT
  | Var x -> (try List.assoc x ctx with Not_found -> raise (Type_error ("Unbound variable: " ^ x)))
  | Abs (x, t, e) -> FunT (t, tp_of_expr ((x, t) :: ctx) e)
  | App (e1, e2) ->
      let t1 = tp_of_expr ctx e1 in
      let t2 = tp_of_expr ctx e2 in
      (match t1 with
        | FunT (arg_t, ret_t) ->
            if arg_t = t2 then ret_t
            else raise (Type_error (string_of_expr e1 ^" !! " ^ string_of_expr e2 ^ " !!" ^
                                  "This expression has type " ^ string_of_type t2 ^ 
                                  " but an expression was expected of type " ^ string_of_tp arg_t))
        | _ -> raise (Type_error "Application of a non-function expression"));;


(* TEST *)
tp_of_expr [] (App (Abs ("x", ConstT IntT, Var "x"), Const (IntE 2))) ;;
tp_of_expr [] (App (Abs ("x", ConstT IntT, Var "x"), Const (BoolE true)));;

tp_of_expr [] (App (Abs ("x", ConstT IntT, Var "x"), Const (IntE 2)));;
tp_of_expr [] (App ( App ( Abs("x", ConstT IntT, Var "x"), Const (IntE 2)), Const (IntE 3)));;
tp_of_expr [] (App (App ( Abs ("x", ConstT IntT, Abs ("y", ConstT BoolT, Var "y")) , Const (IntE 2)), Const (IntE 3)));;
tp_of_expr [] (App (Abs("f", FunT(ConstT IntT, ConstT BoolT), App( Var "f", Const (IntE 2))) , Abs ("x", ConstT IntT, Const (BoolE true))));;