open Ast 
open List
open Hashtbl

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

(*let attribute_coeffs_mode product_list user_hashtable =
  failwith "TODO"*)

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
      else 0

let min_reviews_quartile product_list_non_sorted =
  let product_list= List.sort comp_nbreviews product_list_non_sorted in
  let p = List.nth product_list ((List.length product_list) / 4) in
  let Product(_, rev) = p in
  List.length rev

let min_reviews_median product_list_non_sorted =
  let product_list = List.sort comp_nbreviews product_list_non_sorted in
  let p = List.nth product_list ((List.length product_list) / 2) in
  let Product(_, rev) = p in
  List.length rev

let mean_nbreviews product_list =
  let rec mean prods acc n =
    match prods with
      h::t -> let Product(_, rev) = h in mean t (acc + List.length rev) (n + 1)
    | [] -> if n = 0 then raise No_review else acc / n in
  mean product_list 0 0

let define_min_reviews methode product_list =
  match methode with
  | Quartile -> min_reviews_quartile product_list
  | Median -> min_reviews_median product_list
  | Mean -> mean_nbreviews product_list
  | _ -> failwith "TODO"
