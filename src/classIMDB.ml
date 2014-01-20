open Ast
open List

(*somme des termes d'une liste*)
let somme list =
  let rec f list acc =
    match list with
      h::t -> (f t (acc +. h))
    | [] -> acc
  in
  f list 0.
    
(* moyenne usuelle *)
let mean_simple rev =
  let rec m rev acc n =
    match rev with
      h::t -> let Review_score(s) = h.review_score in m t (acc +. s) (n + 1)
    | [] -> if n = 0 then raise No_review else acc /. (float_of_int n)
  in
  m rev 0. 0
    
(* moyenne usuelle avec coefficients des scores d'une liste de reviews
   Entrée : rev review list et coef float list
   Sortie : float
   leve l'exception No_review quand les listes sont vide où que la liste coef ne contient que des 0 *)
let mean rev coef =
  let rec m rev coef acc n =
    match rev, coef with
      h::t, h2::t2 -> let Review_score(s) = h.review_score in if h2 <> 0. then m t t2 (acc +. s *. h2) (n +. h2) else m t t2 acc n;
    | [], [] -> if n = 0. then raise No_review else acc /. n; in
  m rev coef 0. 0.;;

(*Moyenne globale d'une liste de produits
  Entrée : prods (float list * product) list
  Sortie : float*)
let globalMean prods =
  let rec m prods acc n =
    match prods with
      h::t -> let (coef, p) = h in let Product(_, rev) = p in m t (acc +. mean rev coef) (n +. 1.);
    | [] -> acc /. n; in
  m prods 0. 0.;;

(*moyenne IMDB des scores d'une liste de reviews (concernant un produit) selon la formule :
  (Rv + cm)/(v + m) où
  R est la moyenne des score de la liste,
  v est le nombre de reviews (la somme de leurs coefficients)
  c la note moyenne globale des produits
  m le nombre minimum de reviews à avoir pour figurer dans le classement
  Entrée : rev review list, coef float list, m float, c float
  Sortie : float*)
let meanIMDB rev coef m c =
  let v = somme coef in
  (v *. mean rev coef +. m *. c) /. (m +. v);;

(*Fonction de comparaison de deux produits en fonction de leur moyenne
  Entrée : prod1 et prod2 (product * float)
  Sortie : int*)
let comp prod1 prod2 =
  let (_, mean1) = prod1 and (_, mean2) = prod2 in
  if mean1 > mean2 then -1
  else if mean1 < mean2 then 1
  else 0;;

let meanReview prods =
  let rec m prods acc n =
    match prods with
      h::t -> let (_, p) = h in let Product(_, rev) = p in
				let rec mrev rev acc2 n2 =
				  match rev with
				    h2::t2 -> let Review_score(s) = h2.review_score in mrev t2 (acc2 +. s) (n2 +. 1.)
				  | [] -> if n2 = 0. then raise No_review else (acc2, n2)
				in
				let (acc2, n2) = mrev rev 0. 0. in
				m t (acc +. acc2) (n +. n2)
    | [] -> if n = 0. then raise No_review else  acc /. n
  in
  m prods 0. 0.
    
(*Fonction de construction de la liste des produits ayant assez de reviews pour entrer dans le classement
  Entrée : prods (float list * product) list, m int le nombre de reviews nécessaires
  Sortie : (float list * product) list*)
let eligible prods m = 
  let rec e prods m =
    match prods with
      h::t -> let (rev, _) = h in if (somme rev) <= m then e t m  else h::e t m
    | [] -> []; in
  if (verbose)
  then Format.printf "tri des produits avec au moins %f reviews: %d produits a trier@." m (List.length prods); 
  let l = e prods m
  in
  if (verbose)
  then Format.printf "il reste %d produits@." (List.length l);
  Format.printf "moyenne des reviews : %f@." (meanReview l);
  l
    
(*Transforme une (float list * product) list en (product, int) list*)
let rec meanList prods m c = 
  match prods with
    h::t -> let (coef, p) = h in
            let Product(_, rev) = p in
	    (p, meanIMDB rev coef m c)::meanList t m c
  | [] -> [];;

(*Transforme une (float list * product) list en (product, int) list (sans tenir compte de la float list)*)
let rec simpleMeanList prods =
  match prods with
    h::t -> let (coef, p) = h in
	    let Product(_, rev) = p in
	    (p, mean_simple rev)::simpleMeanList t
  | [] -> []
    
(*Retourne un classement type IMDB d'une liste de produits
  Entrée : m float le nombre minimum de reviews à avoir pour figurer dans le classement, prods (coef list * product) list
  Sortie : (product * float) list triée selon les float*)
let triIMDB prods m =
  if (verbose)
  then Format.printf "triIMDB m = %0.2f @." m; 
  List.sort comp (meanList (eligible prods m) m (globalMean prods))
    
let triSimple prods =
  List.sort comp (simpleMeanList prods)
    
let triNaif prods =
  triIMDB prods 0.
    
