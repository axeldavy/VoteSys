open Ast 

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
