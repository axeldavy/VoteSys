open Ast
open Hashtbl
open List

let get_product_list_of_review_list review_list =
   let hashtblproduct = Hashtbl.create (List.length review_list) in(* grosse majoration *)
   let fill_hash_tbl review =
      let Product_id (product_code) = review.product_id in
	 try
	    let product_review_list = Hashtbl.find hashtblproduct product_code in
	    Hashtbl.replace hashtblproduct product_code (review::product_review_list)
	 with
	 | Not_found -> Hashtbl.add hashtblproduct product_code [review]
   in
   List.iter fill_hash_tbl review_list
   let fill_list product_code product_review_list built_review_list =
      (Product (Product_id (product_code), product_review_list))::built_review_list
   in
   Hashtbl.fold fill_list hashtblproduct []
      