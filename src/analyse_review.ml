open Ast 
open List
open Hashtbl
open ClassIMDB

(********************************************** Anciennete des utilisateurs *************************************************)
let comp_date rev1 rev2 =
  let Review_time(d1) = rev1.review_time and Review_time(d2) = rev2.review_time in
  if d1 > d2 then 1
  else if d1 < d2 then -1
  else 0;;

let firstReview_hashtbl user_hashtbl =
  let hashtbl = Hashtbl.create (Hashtbl.length user_hashtbl) in
  let fill_hashtbl userid reviews =
    let rev = List.hd (List.sort comp_date reviews) in
    let Review_time(t) = rev.review_time in
    Hashtbl.add hashtbl userid t
  in
  Hashtbl.iter fill_hashtbl user_hashtbl;
  hashtbl;;

let coeff_ancien product hashtbl =
  let Product(_, rev) = product in
  let rec coeflist rev hashtbl = 
    match rev with
      h::t -> let Review_time(time) = h.review_time and Review_user_id(userid) = h.review_user_id in 
	      let d = time - Hashtbl.find hashtbl userid in
	      if d < twoWeeks then smallCoef::(coeflist t hashtbl)
	      else 1.::(coeflist t hashtbl)
    | [] -> []
  in
  ((coeflist rev hashtbl), product)
    
let attribute_coeffs_ancien product_list user_hashtbl =
  let hashtbl = firstReview_hashtbl user_hashtbl in
  let rec f product_list hashtbl =
    match product_list with
      h::t -> (coeff_ancien h hashtbl)::(f t hashtbl)
    | [] -> []
  in
  (f product_list hashtbl)
    
(************************************************* Effet de mode ***************************************************************)
let coeff_mode product date =
  let Product(_, rev) = product in
  let rec coeflist rev date =
    match rev with
      h::t -> let Review_time(time) = h.review_time in
	      if (time - date) < twoMonths then smallCoef::(coeflist t date)
	      else 1.::(coeflist t date)
    | [] -> []
  in
  ((coeflist rev date), product) 
    
let rec attribute_coeffs_mode product_list =
  match product_list with
    h::t -> let Product(_, rev) = h in
	    let prod = List.sort comp_date rev in
	    let Review_time(d) = (List.hd prod).review_time in
	    (coeff_mode h d)::(attribute_coeffs_mode t)
  | [] -> []
    
(************************************************** Utilité ****************************************************************)
let coefpos x =
  (float_of_int (2 + x))
    
let coefneg x =
  1. /. (float_of_int (2 + x))
    
let coeff_util product =
  let Product(_, rev) = product in
  let rec coeflist rev =
    match rev with
      h::t -> let Review_helpfulness(pos, all) = h.review_helpfull_ness in
	      ((coefpos pos) *. (coefneg (all - pos)))::(coeflist t)
    | [] -> []
  in
  ((coeflist rev), product)
    
let rec attribute_coeffs_helpfullness product_list =
  match product_list with
    h::t -> (coeff_util h)::(attribute_coeffs_helpfullness t)
  | [] -> []
    
(************************************************** Calcul des coefs *******************************************************)
let attribute_coeffs methode product_list user_hashtable =
   (* retourne une liste de (float list)* product *)
  match methode with 
  | Simple -> let get_coef_product_simple product =
		let Product(_,review_list) = product in 
		List.map (fun _ -> 1.) review_list
	      in
	      List.map (fun p -> (get_coef_product_simple p,p)) product_list
  | Helpfullness -> attribute_coeffs_helpfullness product_list
  | Mode -> attribute_coeffs_mode product_list
  | Anciennete -> attribute_coeffs_ancien product_list user_hashtable
  | _ -> assert false
    
(************************************************* Calcul nombre reviews min *************************************************)
    
(* Entree : prod1 prod2 (float list * product)*)
let comp_nbreviews prod1 prod2 =
  let (coef1, _) = prod1 and (coef2, _) = prod2 in
  let l1 = somme coef1 and l2 = somme coef2 in
  if l1 > l2 then 1
  else if l1 < l2 then -1
  else 0
    
let min_reviews_quartile product_list_non_sorted =
  let product_list= List.sort comp_nbreviews product_list_non_sorted in
  let p = List.nth product_list ((List.length product_list) / 4) in
  let (coef, _) = p in
  let n = somme coef
  in
  if (verbose)
  then Format.printf "Quartile method: %f review@." n;
  n
    
let min_reviews_median product_list_non_sorted =
  let product_list = List.sort comp_nbreviews product_list_non_sorted in
  let p = List.nth product_list ((List.length product_list) / 2) in
  let (coef, _) = p in
  let n = somme coef
  in
  if (verbose)
  then Format.printf "Median method: %f review@." n;
  n
    
let mean_nbreviews product_list =
  let rec mean prods acc n =
    match prods with
      h::t -> let (coef, _) = h in mean t (acc +. somme coef) (n +. 1.)
    | [] -> if n = 0. then raise No_review else acc /. n in
  let n = mean product_list 0. 0.
  in
  if (verbose)
  then Format.printf "Mean method: %f review@." n;
  n
    
let define_min_reviews methode product_list =
  match methode with
  | Quartile -> min_reviews_quartile product_list
  | Median -> min_reviews_median product_list
  | Mean -> mean_nbreviews product_list
  | _ -> assert false
    
