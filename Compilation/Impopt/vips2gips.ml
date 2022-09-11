(**
   Traduction entre deux représentations intermédiaires.

   Départ : VIPS
     graphe d'instructions élémentaires
     infinité de registres virtuels pour variables et valeurs temporaires

   Arrivée : GIPS
     graphe d'instructions élémentaires
     seulement registres physiques et emplacement de pile

   Le cœur de la traduction consiste à effectuer l'allocation de 
   registres. En passant, il faut ajuster certaines des opérations, pour
   ajouter des instructions de lecture ou d'écriture sur la pile lorsque
   l'on manipule des valeurs qui ont été affectées à cet endroit.

   En outre, on va profiter de cette traduction pour expliciter les
   conventions d'appel des fonctions : passer les paramètres, sauvegarder
   et restaurer les registres qui doivent l'être, allouer et désallouer 
   le tableau d'activation, renvoyer le résultat...
 *)

open Gips
open Register_allocation

(**
   Fonctions auxiliaires : traduction à l'identique des opérateurs
 *)
let tr_unop = function
  | Vips.Addi n -> Addi n
  | Vips.Move   -> Move
let tr_binop = function
  | Vips.Add -> Add
  | Vips.Mul -> Mul
  | Vips.Lt  -> Lt

(**
   Fonction principale : traduction d'une fonction VIPS en fonction GIPS. 
   Note : l'allocation de registre est faite à l'échelle d'une fonction.
 *)
let translate_fdef fdef =
  (* Initialisation d'une nouvelle table de hachage pour représenter le
     graphe de code de la fonction GIPS produite. *)
  let code = Hashtbl.create 32 in
  (* On utilise comme précédemment deux fonctions auxiliaires de création
     d'une nouvelle étiquette et d'ajout d'un nouveau nœud au graphe.
     Note : comme on conserva les étiquettes déjà utilisées dans la
     version VIPS, cette nouvelle fonction [new_label] construit des noms
     de forme différente.
   *)
  let new_label =
    let f = Vips.(fdef.name) in
    let count = ref 0 in
    fun () -> incr count; Printf.sprintf "%s_g_%i" f !count
  in
  let add_instr i =
    let l = new_label () in
    Hashtbl.add code l i;
    l
  in

  (* On récupère l'allocation des registres et le nombre d'emplacement de 
     pile utilisés. *)
  let alloc, nb_locals = allocate_function fdef in

  (** Quelques registres physiques MIPS que l'on manipulera à la main. *)
  (* Registres de travail pour manipuler les valeurs venant de la pile *)
  let tmp1 = "$t0" in
  let tmp2 = "$t1" in
  (* Registres utilisés par [syscall] *)
  let arg  = "$a0" in
  let scod = "$v0" in
  (* Registre pour le résultat d'un appel de fonction *)
  let ret  = "$v0" in
  (* Sommet de pile *)
  let sp   = "$sp" in
  (* Adresse de base du tableau d'activation d'un appel *)
  let fp   = "$fp" in
  (* Adresse de retour *)
  let ra   = "$ra" in

  (**
     Traduction d'une instruction VIPS en une ou plusieurs 
     instructions GIPS.

     Pour éviter d'avoir à renommer tous les champs [next] des
     instructions, on va garder les étiquettes déjà présentes.
     En revanche, on va créer à la volée de nouveaux nœuds et de
     nouvelles étiquettes lorsqu'une instruction VIPS est traduite
     par plusieurs instructions GIPS.

     Si une instruction VIPS [i] doit être traduite par une séquence
     d'instructions GIPS [i1; i2; ...; iN], on va créer de nouveaux
     nœuds et de nouvelles étiquettes pour les instructions [i2] à [iN],
     et garder pour [i1] l'étiquette qui était celle de [i].

     Dans ce cas, [tr_instr] renvoie l'instruction GIPS [i1] après avoir
     ajouté au graphe les instructions [iN] à [i2]. Dans tous les cas,
     [tr_instr] ne renvoie donc qu'une unique instruction GIPS.
   *)
  let tr_instr i = match i with
    (* Cas du chargement d'une constante *)
    | Vips.Cst(r, n, next) ->
       (* On consulte l'allocation du registre de destination [r] *)
       begin match Hashtbl.find alloc r with
       (* S'il s'agit d'un registre physique, on utilise directement
          l'instruction GIPS correspondante *)
       | Register r' -> Cst(r', n, next)
       (* Sinon on veut écrire la valeur [n] à une certaine adresse
          de la pile. Pour cela il faut d'abord charger cette valeur 
          dans un registre de travail, puis la transfèrer sur la pile. 
          Donc : on crée un nouveau nœud pour la deuxième opération
          (instruction d'écriture [SetLocal]), puis on renvoie
          l'instruction correspondant à la première étape. *)
       | Stacked k ->
          let l = add_instr (SetLocal(k, tmp1, next)) in
          Cst(tmp1, n, l)
       end

    (* Cas d'un opérateur unaire
       On va produire entre une et trois opérations selon les allocations
       de registre de destination et de l'opérande. Ci-dessous, un
       fragment de code où tous les cas à traiter sont détaillés. On peut
       factoriser ce programme en définissant les bonnes fonctions
       auxiliaires. *)
    | Vips.Unop(r1, op, r2, next) ->
       begin match Hashtbl.find alloc r1 with
       | Register r1' -> begin match Hashtbl.find alloc r2 with
                       | Register r2' -> Unop(r1', tr_unop op, r2', next)
                       | Stacked k2 ->
                          let l = add_instr (Unop(r1', tr_unop op, tmp1, next)) in
                          GetLocal(tmp1, k2, l)
                       end
       | Stacked k1 -> begin match Hashtbl.find alloc r2 with
                       | Register r2' -> failwith "not implemented"
                       | Stacked k2 -> failwith "not implemented"
                       end
       end

    (* Cas d'un appel de fonction
       On règle à cet endroit les deux étapes du protocole d'appel incombant
       à l'appelant, à savoir :

       - Étape 1 : appelant, avant l'appel.
         Point principal : passage des arguments sur la pile, du dernier 
         au premier

            +------+
            |  aN  |
            +------+
            |      |
            |  ..  |
            |      |
            +------+
            |  a2  |
            +------+
            |  a1  |
            +------+
            |      |  <- nouvelle valeur de $sp (pointeur de sommet de pile)

         Cette première étape doit aussi se charger de sauvegarder les
         registres temporaires pour éviter qu'ils ne soient écrasés par la
         fonction appelée. Dans une version simple, on peut enregistrer tous
         les registres sur la pile avant de passer les arguments.

       - Étape 4 : appelant, après l'appel.
         Points principaux : récupérer la valeur renvoyée par l'appel, et
         retirer les arguments qui avaient été placés sur la pile avant 
         l'appel.
         Cette dernière étape doit aussi se charger de restaurer les
         registres temporaires qui avaient été sauvegardés avant l'appel.
     *)
    | Vips.Call(r, f, args, next) ->
       (* Protocole : étape 4 *)
       let retrieve_result = match Hashtbl.find alloc r with
         | Register r' -> Unop(r', Move, ret, next)
         | Stacked k   -> SetLocal(k, ret, next)
       in
       let l = add_instr retrieve_result in
       let l = add_instr (Unop(sp, Addi (4*List.length args), sp, next)) in
       (* et restaurer les registres *)
       
       (* Appel *)
       let l = add_instr (Call(f, l)) in
       (* à noter : l'appel va enregistrer l'adresse de retour dans le
          registre $ra *)
       
       (* Protocole : étape 1 *)
       let rec pass_args args l = match args with
         | [] -> l
         | a::args -> let l = add_instr (Push(a, l)) in
                      pass_args args l
       in
       let l = pass_args args l in
       Jump l
       (* et sauvegarder les registres *)
       
    (* Cas de la fin d'exécution d'un appel de fonction
       On règle à cet endroit l'étape du protocole à la charge de l'appelé,
       à la fin de l'appel. Il faut
       - placer le résultat dans le registre dédié $v0 (désigné ici par [ret])
       - libérer l'espace de pile qui avait été réservé pour les variables
         locales de la fonction
       - restaurer les valeurs des registres $fp (adresse de référence du
         tableau d'activation de l'appelant) et $ra (adresse de retour qui
         avait été enregistrée au moment de l'appel).
     *)
    | Vips.Return r -> (* dans le protocole : 3 *)
       let l = add_instr Return in
       let l = add_instr (Pop(fp, l)) in
       let l = add_instr (Pop(ra, l)) in
       let l = add_instr (Unop(sp, Addi 8, fp, l)) in
       let i = match Hashtbl.find alloc r with
         | Register r' -> Unop(ret, Move, r', l)
         | Stacked k   -> GetLocal(ret, k, l)
       in
       i
      
  in

  (* Boucle appliquant la traduction à chaque instruction du graphe de 
     la fonction VIPS prise en argument. Notez que c'est ici que 
     l'étiquette de chaque nœud est prélevée et réutilisée. *)
  Hashtbl.iter
    (fun l i -> Hashtbl.add code l (tr_instr i))
    Vips.(fdef.code);
  
  (* On ajoute en tête du code produit les instructions destinées à
     sauvegarder les registres $fp et $ra et allouer le tableau
     d'activation de la fonction. L'étiquette générée pour la première
     de ces instructions devient la nouvelle étiquette de départ. 

     C'est cette partie qui prend en charge l'étape 2 du protocole d'appel,
     à savoir la partie incombant à l'appelé, au début de l'appel.
   *)
  let l = add_instr (Unop(sp, Addi (-4 * nb_locals), sp,
                          Vips.(fdef.entry))) in
  let l = add_instr (Unop(fp, Addi 8, sp, l)) in
  let l = add_instr (Push(ra, l)) in
  let entry = add_instr (Push(fp, l)) in

  {
    name = Vips.(fdef.name);
    code; entry
  }

(**
   Traduction d'un programme : simplement appliquer la traduction
   précédente à chaque fonction.
*)
let translate_prog prog = {
    globals = Vips.(prog.globals);
    functions = List.map translate_fdef Vips.(prog.functions)
  }
