open Array
open Ast
(*open Gnuplot*)


let plot_distribution_notes review_list =
  let x = [|0.;0.5;1.;1.5;2.;2.5;3.;3.5;4.;4.5;5. |] in
  let y = Array.make 11 0. in
  let n = Array.make 11 0 in
  let fill_n review =
    let Review_score (score) = review.review_score in
    let iscore = int_of_float (2.*.score)  in
    n.(iscore) <- n.(iscore)+1;
    ()
  in
  List.iter fill_n review_list;
  let sum acc v =
    acc + v
  in
  let sumn = Array.fold_left sum 0 n in
  for i = 0 to 10 do
    y.(i) <- float_of_int(n.(i)) /. float_of_int(sumn);
  done;
  let gphandle = Gnuplot.init (Gnuplot.PS("./notes.ps")) in
  Gnuplot.title gphandle "Distribution des notes";
  Gnuplot.xlabel gphandle "notes";
  Gnuplot.ylabel gphandle "proportion";
  (*Gnuplot.color gphandle (Graphics.rgb 120 120 120);*)
  Gnuplot.env gphandle ~xaxis:true (-0.5) 5.5 ~yaxis:true 0. 1.;
  Gnuplot.color gphandle Graphics.red;
  Gnuplot.Array.xy gphandle ~style:Gnuplot.Points x y;
  Gnuplot.close gphandle
    
let plot_distributions name lpn ln =
   (* entry: 
      name: filename
      lpn: (product * float) list list
      ln: string list 
      effet: crée un fichier de nom name qui contient les distributions
      des moyennes*)
  let filename = name ^".ps" in
  let gphandle = Gnuplot.init (Gnuplot.PS(filename)) in
  Gnuplot.xlabel gphandle "moyennes";
  Gnuplot.ylabel gphandle "proportion";
  Gnuplot.env gphandle ~xaxis:true (-0.5) 5.5 ~yaxis:true 0. 1.2;
  let x = [|0.;0.5;1.;1.5;2.;2.5;3.;3.5;4.;4.5;5. |] in
  let colors = [|Graphics.red; Graphics.green; Graphics.blue; Graphics.magenta; Graphics.cyan; Graphics.black|] in
  let colori = ref(0) in
  let draw_distr pn name =
    let y = Array.make 11 0. in
    let n = Array.make 11 0 in
    let fill_n pnitem =
      let (p,score) = pnitem in
      let iscore = int_of_float (2.*.score)  in
      n.(iscore) <- n.(iscore)+1;
      ()
    in
    List.iter fill_n pn;
    let sum acc v =
      acc + v
    in
    let sumn = Array.fold_left sum 0 n in
    for i = 0 to 10 do
      y.(i) <- float_of_int(n.(i)) /. float_of_int(sumn);
    done;
    Gnuplot.color gphandle (colors.(!colori));
    colori := !colori+1;
    Gnuplot.Array.xy gphandle ~style:Gnuplot.Points ~label:name x y;
  in
  List.iter2 draw_distr lpn ln;
  Gnuplot.close gphandle   
