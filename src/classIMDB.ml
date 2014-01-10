open Ast
open List

 (* moyenne usuelle avec coefficients des scores d'une liste de reviews
	Entrée : rev review list et coef float list
	Sortie : float*)
let mean rev coef =
  let rec m rev coef acc n =
  match rev, coef with
    h::t, h2::t2 -> let Review_score(s, _) = h.review_score in m t t2 (acc +. float_of_int s *. h2) (n +. 1.);
  | [], [] -> acc /. n; in
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
    v est le nombre de reviews
    c la note moyenne globale des produits
    m le nombre minimum de reviews à avoir pour figurer dans le classement
    Entrée : rev review list, coef float list, m float, c float
    Sortie : float*)
let meanIMDB rev coef m c =
  let v = float_of_int (List.length rev) in
  (v *. mean rev coef +. m *. c) /. (m +. v);;

 (*Fonction de comparaison de deux produits en fonction de leur moyenne
	Entrée : prod1 et prod2 (product * float)
	Sortie : int*)
let comp prod1 prod2 =
  let (_, mean1) = prod1 and (_, mean2) = prod2 in
   if mean1 > mean2 then 1
   else if mean1 < mean2 then -1
       else 0;;
       
 (*Fonction de construction de la liste des produits ayant assez de reviews pour entrer dans le classement
	Entrée : prods (float list * product) list, m int le nombre de reviews nécessaires
	Sortie : (float list * product) list*)
let eligible prods m = 
  let rec e prods m acc =
    match prods with
      h::t -> let (rev, _) = h in if float_of_int (List.length rev) <= m then e t m acc else h::e t m acc
    | [] -> acc; in
  e prods m [];;

 (*Transforme une (float list * product) list en (product, int) list*)
let rec meanList prods m c = 
  match prods with
    h::t -> let (coef, p) = h in
            let Product(_, rev) = p in
	    (p, meanIMDB rev coef m c)::meanList t m c
  | [] -> [];;

 (*Retourne un classement type IMDB d'une liste de produits
	Entrée : m float le nombre minimum de reviews à avoir pour figurer dans le classement, prods (coef list * product) list
	Sortie : (product * int) list triée selon les int*)
let triIMDB prods m =
  List.sort comp (meanList (eligible prods m) m (globalMean prods))


let triNaif prods =
  triIMDB prods 0.
