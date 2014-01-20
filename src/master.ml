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
  for i = 0 to 9 do
    let (Product (pr,rl),score) = List.nth classement i in
    let p = List.hd(rl) in
    let Product_id(product_id) = p.product_id in
    let Product_title(product_title) = p.product_title in
    Format.printf "%d: mean %f, Product %s:%s @." (i+1) score product_id product_title;
  done;
  ()
    
let process_2 review_list entry_list fname =
  let naif = triNaif entry_list in
  if (verbose)
  then print_classement naif;
  
  let imdbquartile = triIMDB entry_list (define_min_reviews Quartile entry_list) in
  if (verbose)
  then print_classement imdbquartile;
  
  let imdbmedian = triIMDB entry_list (define_min_reviews Median entry_list) in
  if (verbose)
  then print_classement imdbmedian;
  
  let imdbmean = triIMDB entry_list (define_min_reviews Mean entry_list) in
  if (verbose)
  then print_classement imdbmean;
  
  plot_distribution_notes review_list;
  plot_distributions fname [naif; imdbquartile] ["Naif";"IMDBQuartile"]
    
let process review_list =
  let product_list = Util.get_product_list_of_review_list review_list in
  let userhashtbl = Util.get_user_hashtbl review_list in
  let entry_list_simple = Analyse_review.attribute_coeffs Simple product_list userhashtbl in
  if (verbose) 
  then Format.printf "Coeffs: Simple @.";
  process_2 review_list entry_list_simple "means_simple";
  
  let entry_list_mode = Analyse_review.attribute_coeffs Mode product_list userhashtbl in
  if (verbose) 
  then Format.printf "@.Coeffs: Mode @.";
  process_2 review_list entry_list_mode "means_mode";
  
  let entry_list_helpfullness = Analyse_review.attribute_coeffs Helpfullness product_list userhashtbl in
  if (verbose) 
  then Format.printf "@.Coeffs: Helpfullness @.";
  process_2 review_list entry_list_helpfullness "means_helpfullness";
  
  let entry_list_anciennete = Analyse_review.attribute_coeffs Anciennete product_list userhashtbl in
  if (verbose) 
  then Format.printf "@.Coeffs: Anciennete @.";
  process_2 review_list entry_list_anciennete "means_anciennete";
  
