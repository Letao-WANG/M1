(**
   Élimination de certaines instructions inutiles

   Critère : une instruction [i] plaçant une valeur dans un registre [r]
   n'est utile que si le registre [r] est bien vivant en sortie de [i].
   Sinon on peut la supprimer.

   Note : en supprimant une instruction inutile on enlève aussi 
   potentiellement des utilisations de certains registres. Autrement dit
   on va peut-être découvrir de nouvelles instructions à supprimer en
   relançant l'analyse une nouvelle fois.
 *)

open Vips
open Liveness

let dwe_step fdef =
  (* Calcul de vivacité *)
  let _, live_out = liveness fdef in
  (* Un booléen qui passera à [true] si une instruction a été 
     supprimée, pour signaler que l'optimisation n'est peut-être
     pas finie. *)
  let change = ref false in
  Hashtbl.iter (fun l i ->
      match i with
      | Cst(r, n, next) ->
         if S.mem r (Hashtbl.find live_out l) then
           (* [r] est vivant, on garde l'instruction *)
           ()
         else begin
           (* [r] n'est pas vivant, on supprime l'instruction
              en la remplaçant par un simple saut *)
             Hashtbl.replace fdef.code l (Jump next);
             change := true
           end
    ) fdef.code;
  !change
  

let dwe fdef =
  while dwe_step fdef do () done

let dwe_prog prog =
  List.iter dwe prog.functions
