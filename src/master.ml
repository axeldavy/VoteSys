open Ast
open Analyse_review
open ClassIMDB
open Cut_graph
open Cut_recom
open Graphics
open Util


let process review_list =
   let product_list = Util.get_product_list_of_review_list review_list in
   let userhashtbl = Util.get_user_hashtbl review_list in
   let entry_list = Analyse_review.attribute_coeffs Simple product_list userhashtbl in
   failwith "TODO"
