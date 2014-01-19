open Ast
open Analyse_review
open ClassIMDB
open Cut_graph
open Cut_recom
open Graphic
open Util
open Format
open List

let process review_list =
   let product_list = Util.get_product_list_of_review_list review_list in
   let userhashtbl = Util.get_user_hashtbl review_list in
   let entry_list = Analyse_review.attribute_coeffs Simple product_list userhashtbl in
   plot_distribution_notes review_list;
   plot_distributions "means_simple" [triNaif entry_list; triIMDB entry_list 100.] ["Naif";"IMDB100"]
