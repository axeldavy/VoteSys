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
   let mean = (List.fold_right calc_mean classement 0.) /. (float_of_int (List.length(classement))) in
   Format.printf "mean products notes: %0.3f@." mean;
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
   if (verbose) 
   then Format.printf "Coeffs: Simple @.";
   
   let naif = triNaif entry_list in
   if (verbose)
   then print_classement naif;
   
   let imdbquartile = triIMDB entry_list (define_min_reviews Quartile product_list) in
   if (verbose)
   then print_classement imdbquartile;
   
   let imdbmedian = triIMDB entry_list (define_min_reviews Median product_list) in
   if (verbose)
   then print_classement imdbmedian;
   
   let imdbmean = triIMDB entry_list (define_min_reviews Mean product_list) in
   if (verbose)
   then print_classement imdbmean;
   
   plot_distribution_notes review_list;
   plot_distributions "means_simple" [naif; imdbquartile] ["Naif";"IMDBQuartile"]
