 """ TD """
""" environnement """


let b = true;;
let x = 3;;
let p = fun (i:int) -> true;;
let f = fun (x : int) -> fun(y : int) -> x;;


""" 2 - Vérification de typage de programmes fonctionnels """
fun ( y:int ) -> f x y;;
fun ( y:int ) -> f x;;
fun (y:int) -> p y;;

""" test """
(fun (x : int) -> 2*x) 4;;


  """ TP """

"""3 Interlude: Listes d’ association et maps"""

"""3.1 Listes d’association"""

type 'a option = None | Some of 'a ;;

let exemple_assoc = [("Max", 10); ("Nicolas", 4); ("Nicole", 9)];;

let rec lookup_assoc cle liste = 
  match liste with   
  (nom, age)::liste when nom = cle -> Some age|
  (nom, age)::liste -> lookup_assoc(cle)(liste) |
  [] -> None ;;
  
lookup_assoc "Nicolas" exemple_assoc;;
lookup_assoc "Maurice" exemple_assoc;;

let add_assoc (cle)(valeur)(liste) = (cle, valeur)::liste ;;  
add_assoc "Mathéo" 20 exemple_assoc;;

let rec remove_assoc (cle)(liste) =
  match liste with 
  | (nom, age)::liste -> if cle = nom then remove_assoc(cle)(liste) else (nom,age)::remove_assoc(cle)(liste) 
  | [] -> [];;

remove_assoc "Nicolas" exemple_assoc;;
remove_assoc "Max" exemple_assoc;;

"""3.2 Maps"""
type ('k, 'v) map = 'k -> 'v option;;

let exemple_map : (string, int) map = fun key 
-> match key with 
|"Max" -> Some 10
|"Nicolas" -> Some 4
|"Nicole"-> Some 9
| _ -> None ;;


"""4 Règles de typage élémentaires"""
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