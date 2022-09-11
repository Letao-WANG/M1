(**
   Analyse de vivacité

   L'objectif est ici d'analyser la vivacité des variables d'une 
   fonction VIPS. Une variable [x] est dite « vivante » à un point 
   de programme donné si sa valeur courante est encore susceptible
   d'être utilisée dans un moins un futur possible, autrement dit
   si un chemin dans le graphe d'instructions aboutit à une 
   instruction qui utilise [x] sans passer au préalable par une
   instruction modifiant la valeur de [x].

   Comme les instructions sont les points où une variable peut 
   changer de statut (vivante ou non), la vivacité des variables 
   n'a de sens que dans l'intervalle entre deux instructions. 
   Pour évaluer cela on calculera pour chaque instruction l'ensemble
   des variables qui sont vivantes « en entrée » (c'est-à-dire juste
   avant) et l'ensemble des variables qui sont vivantes « en sortie »
   (c'est-à-dire juste après).
   La fonction principale [liveness] renvoie deux tables associant
   chacune l'un de ces ensembles à chaque instruction de la fonction
   analysée.

   Cette vivacité obéit à deux équations 
   (appelées « équations de flot de données »)

     IN[i]  = (OUT[i] \ DEF[i]) + USE[i]

     OUT[i] = IN[j1] + ... + IN[jk]
              avec j1, ..., jk l'ensemble des successeurs de i

   avec :
   - IN[i]  l'ensemble des variables vivantes en entrée de i
   - OUT[i] l'ensemble des variables vivantes en sortie de i
   - DEF[i] l'ensemble des variables dont la valeur est (re)définie
   - USE[i] l'ensemble des variables dont la valeur est utilisée

   Les « variables » auxquelles on s'intéresse ici sont les
   registres virtuels.

   Notez une particularité de cette analyse : la vivacité d'une 
   variable à un moment donné dépend de ce qui sera fait de cette 
   variable dans le futur. Autrement dit, les informations relatives 
   à la vivacité se propagent à rebours du code : on calcule les
   variables vivantes en entrée d'une instruction en fonction des
   variables vivantes en sortir de cette même instruction, et on
   calcule les variables vivantes en sortie d'une instruction en
   fonction des variables vivantes en entrée des instructions venant
   *après* dans l'ordre du programme.
 *)
open Vips

(** 
   Définition d'une structure de données pour représenter des ensembles.

   Techniquement, [Set.Make] est un foncteur, c'est-à-dire une fonction
   prenant en paramètre un module et produisant un nouveau module.
   Ici le module [S] obtenu permet de représenter et manipuler des 
   ensembles de chaînes de caractères. Il définit un type [S.t] des
   ensembles de chaînes de caractères, une constante [S.empty] représentant
   l'ensemble vide, et un certain nombre de fonctions, dont :

   Construction d'un ensemble à un élément 
     S.singleton: string -> S.t    

   Construction d'un ensemble à partir d'une liste  
     S.of_list: string list -> S.t   

   Test d'appartenance
     S.mem: string -> S.t -> bool

   Ajout d'un élément
     S.add: string -> S.t -> S.t

   Des opérations ensemblistes classiques
     S.union: S.t -> S.t -> S.t
     S.inter: S.t -> S.t -> S.t
     S.diff:  S.t -> S.t -> S.t

   On y trouve aussi des itérateurs, des filtres, ... (voir doc caml) 

   Note : la structure d'ensemble obtenue avec [Set.Make] est immuable.
   Chaque opération renvoie donc un nouvel ensemble.
 *)
module S = Set.Make(String)


(**
   Fonctions auxiliaires définissant les ensembles de variables
   (re)définies ou utilisées par une instruction.

   Une variable est « définie » lorsque sa valeur est modifiée par
   l'instruction. Une variable est « utilisée » lorsque sa valeur
   courante est consultée par l'instruction. 

   Notez que rien n'empêche qu'une variable soit à la fois utilisée 
   et redéfinie par une instruction. Par exemple :
     i = i + 1;
 *)
let def i = match i with
  | Cst(r, n, next) -> S.singleton r
  | Putchar(r, next) -> S.empty
(* À compléter *)                   

let use i = match i with
  | Cst(_, _, _) -> S.empty
  | Putchar(r, _) -> S.singleton r
  | Binop(r, op, r1, r2, next) ->
     S.of_list [r1; r2]
(* À compléter *)                   


(**
   Fonction principale, prenant en paramètre la desscription d'une
   fonction VIPS et renvoyant deux tables IN et OUT.

   Du fait de la présence probable de cycles dans le graphe d'instructions,
   la résolution des équations passe par le calcul d'un point fixe.
   L'existence du point fixe est garantie par le théorème de Tarski.
 *) 
let liveness fdef =
  (* IN et OUT sont chacune réalisées par une table de hachage qui
     associera l'étiquette d'une instruction à un ensemble de registres
     virtuels. *)
  let live_in = Hashtbl.create 32 in
  let live_out = Hashtbl.create 32 in

  (**
     Algorithme de Kildall pour le calcul du point fixe.
     
     On maintient une collection d'instructions dont les informations IN
     et OUT doivent encore être mises à jour. Ici cette collection est 
     réalisée par une pile, mais d'autres structures pourraient aussi 
     convenir.
   *)
  let a_traiter = Stack.create () in
  Hashtbl.iter (fun l  _ ->
      Stack.push l a_traiter)
    fdef.code;

  (**
     Tant qu'il reste des instructions à mettre à jour, on en prend une
     arbitraire...
   *)
  while not (Stack.is_empty a_traiter) do
    let l = Stack.pop a_traiter in
    let i = Hashtbl.find fdef.code l in
    (**
       ... on calcule sa nouvelle information OUT...
     *)
    let new_out = match i with
      (* Pas de successeur : rien n'est vivant en sortie *)
      | Return _ -> S.empty
      (* Un successeur : on récupère ce qui est vivant
         en entrée de ce successeur *)
      | Cst(_, _, next) | Unop(_, _, _, next) ->
         Hashtbl.find live_in next
      (* Deux successeurs : on fait l'union de ce qui est
         vivant en entrée de chaque successeur *)
      | CJump(_, lab1, lab2) ->
         S.union (Hashtbl.find live_in lab1) (Hashtbl.find live_in lab2)
    in
    (**
       ... puis sa nouvelle information IN...
     *)
    let new_in =
      S.union (use i) (S.diff new_out (def i))
    in
    (**
       ... on met à jour...
     *)
    Hashtbl.replace live_out l new_out;
    (**
       ... enfin il reste :
       - mettre à jour l'information IN
       - au cas où l'information IN a changé lors de cette étape,
         remettre dans [a_traiter] l'ensemble des prédécesseurs de 
         l'instruction mise à jour
       Pour trouver les prédécesseurs d'une instruction il faut parcourir 
       le graphe [fdef.code]. On peut le faire une fois pour toutes au
       début et tabuler les résultats.
     *)
    
  done;

  live_in, live_out
