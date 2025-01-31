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

"""1 Fonctions d’ordre supérieur"""

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

"""2 - Représentation de Caml en Caml"""
#EXERCICE_1
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


#EXERCICE_2

type const_expr = BoolE of bool | IntE of int

type expr = 
| Const of const_expr
| Var of string
| Abs of string * tp * expr
| App of expr * expr 

"Exemple : " (fun (x : int) -> x) 2 :
App (Abs ("x", ConstT IntT, Var "x"), Const (IntE 2))

1. ((fun (x : int) -> x) 2) 3 :
App ( App ( Abs("x", ConstT IntT, Var "x"), Const (IntE 2)), Const (IntE 3))

2. (fun (x : int) -> fun (y : bool) -> y) 2 3 :
App (App ( Abs ("x", ConstT IntT, Abs ("y", ConstT BoolT, Var "y")) , Const (IntE 2)), Const (IntE 3))

3. (fun (f:int -> bool) -> f 2) (fun (x:int) -> true) :
App ()

// représentation de 2+3 :
Op "+" (Const (IntE 2)) (Const (IntE 3)) 

// représentation de (2+3) < 5 :
Op "<" (Op "+" (Const (IntE 2)) (Const (IntE 3))) (Const (IntE 5))

"""3 - Interlude: Listes d’ association et maps"""

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