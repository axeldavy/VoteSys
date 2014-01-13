(*import Gnuplot

let draw_distribution x y = 
   let gphandle = Gnuplot.init (EPS("./test.ps")) in
   close gphandle
   
   
let draw_distribution_2 review_list =
   let x = [|1;2;3;4;5 |] in
   let y = Array.make 5 0. in
   let n = Array.make 5 0 in
   let fill_n review =
      let Review_score (score) = review.review_score in
      let iscore = int_of_float (score + 0.5) -1 in
      n(iscore) = n(iscore)+1;
   in
   List.iter fill_n review_list;
   let sumn = n(0) + n(1) + n(2) + n(3) + n(4) in
   for i = 0 to 4 do
      y(i) = float_of_int(n(i)) /. float_of_int(sumn);
   done
   draw_distribution x y*)