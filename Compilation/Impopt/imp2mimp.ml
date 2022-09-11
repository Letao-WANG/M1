(**
   Traduction de IMP vers MIMP.
   Deux objectifs
   - simplifier les expressions qui peuvent déjà être partiellement calculées,
   - sélectionner des opérateurs optimisés comme [Addi] lorsque c'est possible.
   La sélection repose sur des fonctions comme [mk_add] à définir dans le
   module MIMP.

   En dehors de ces simplifications et du codage des constantes booléennes par
   des entiers, la structure du programme reste la même.
 *)

open Mimp

let rec isel_expr: Imp.expression -> Mimp.expression = function
  | Imp.Cst n -> Cst n
  | Imp.Bool b ->
     failwith "not implemented"
  | Imp.Var x ->
     failwith "not implemented"
  | Imp.Binop(Imp.Add, e1, e2) ->
     mk_add (isel_expr e1) (isel_expr e2)
  | _ -> assert false

and isel_instr i =
  failwith "not implemented"

and isel_seq s =
  failwith "not implemented"

let isel_fdef f = {
    name = Imp.(f.name);
    code = isel_seq Imp.(f.code);
    params = Imp.(f.params);
    locals = Imp.(f.locals);
  }

let isel_prog p = {
    functions = List.map isel_fdef Imp.(p.functions);
    globals = Imp.(p.globals);
  }
