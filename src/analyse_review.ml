open Ast 
open List

let attribute_coeffs methode product_list user_hashtable =
(* retourne une liste de (float list)* product *)
   match methode with 
   | Simple -> let get_coef_product_simple product =
		  let Product(_,review_list) = product in 
		  List.map (fun _ -> 1.) review_list
	       in
	       List.map (fun p -> (get_coef_product_simple p,p)) product_list
   | Helpfullness -> failwith "TODO"
   | Mode -> failwith "TODO"
   | Anciennete -> failwith "TODO"


let comp_nbreviews prod1 prod2 =
  let Product(_, rev1) = prod1 and Product(_, rev2) = prod2 in
  let l1 = List.length rev1 and l2 = List.length rev2 in
  if l1 > l2 then 1
  else if l1 < l2 then -1
      else 0;;

let min_reviews_quartile product_list =
  List.sort comp_nbreviews product_list;
  let p = List.nth product_list ((List.length product_list) / 4) in
  let Product(_, rev) = p in
  List.length rev;;

let define_min_reviews methode product_list =
  match methode with
  | Quartile -> min_reviews_quartile product_list
  | _ -> failwith "TODO"
