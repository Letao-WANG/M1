(**
   MIMP : une variante de IMP incluant quelques opérations optimisées de MIPS.

   La première étape de traduction, allant de IMP vers MIMP, a deux objectifs
   - simplifier les expressions constantes
   - introduire les instructions optimisées lorsque c'est possible
   Le fichier principal pour cette traduction est imp2mimp.ml
   
   On définit ici des "smart constructors" pour construire des opérations
   arithmétiques en choisissant les opérateurs les plus adaptés.

   Par rapport à IMP, on a aussi disparition des constantes booléennes, à
   remplacer par des entiers (1 pour true et 0 pour false).
 *)

(** 
   Nouvelles opérations héritées de MIPS :
   - addition d'une constante [Addi]
   - décalage de bits [ShiftL]
   Vous avez le droit d'étendre cette liste, tant que les nouvelles
   opérations correspondent bien à des optimisations possibles en MIPS.
*)
type unop = Addi of int | ShiftL of int

(* Le reste des définitions est essentiellement identique *)
type binop = Add | Mul | Lt
                       
type expression =
  | Cst   of int
  | Var   of string
  (* Seule nouveauté : une opération unaire, pour appliquer Addi et ShiftL *)
  | Unop  of unop * expression
  | Binop of binop * expression * expression
  | Call  of string * expression list

type instruction =
  | Putchar of expression
  | Set     of string * expression
  | If      of expression * sequence * sequence
  | While   of expression * sequence
  | Return  of expression
  | Expr    of expression
      
and sequence = instruction list
    
type function_def = {
  name: string;
  code: sequence;
  params: string list;
  locals: string list;
}
    
type program = {
  functions: function_def list;
  globals: string list;
}

(**
   Smart constructors.

   Un appel [mk_add e1 e2] construit une expression équivalente à

     Binop(Add, e1, e2)

   mais tire parti lorsque c'est possible des formes de e1 et e2 pour
   produire une expression plus simple.

   Il faudra encore construire des fonctions équivalentes pour les 
   autres opérations arithmétiques.
 *)
let mk_add e1 e2 =
  match e1, e2 with
  | Cst n1, Cst n2 ->
     Cst (n1 + n2)
  | Cst n, e | e, Cst n ->
     Unop(Addi n, e)
  | e1', e2' ->
     Binop(Add, e1', e2')

let e = 
  Binop(Mul,
        Unop(Addi 6, Var "x"),
        Unop(Addi 1,
             Unop(ShiftL 1, Var "x")))
             
