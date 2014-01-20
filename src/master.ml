open Ast
open Analyse_review
open ClassIMDB
open Cut_graph
open Cut_recom
open Graphic
open Util
open Format
open List

let print_classement classement =
(* classement: (product * float) list triée selon les float *)
   let calc_mean elem acc =
      let (p,score) = elem in
      score +. acc
   in
   let mean = List.fold_right calc_mean classement 0. in
   Format.printf "global mean: %0.3f@." mean;
   for i = 0 to 10 do
      let (Product (pr,rl),score) = List.nth classement i in
      let p = List.hd(rl) in
      let Product_id(product_id) = p.product_id in
      let Product_title(product_title) = p.product_title in
      Format.printf "%d: mean %f, Product %s:%s @." i score product_id product_title;
   done;
   ()

let process review_list =
   let product_list = Util.get_product_list_of_review_list review_list in
   let userhashtbl = Util.get_user_hashtbl review_list in
   let entry_list = Analyse_review.attribute_coeffs Simple product_list userhashtbl in
   let naif = triNaif entry_list 
   in
   if (verbose)
   then print_classement naif;
   let imdb100 = triIMDB entry_list 100. in
   if (verbose)
   then print_classement imdb100;
   plot_distribution_notes review_list;
   plot_distributions "means_simple" [naif; imdb100] ["Naif";"IMDB100"]
