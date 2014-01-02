%{
  open Ast
  open Lexing

%}

%token <Ast.productid> PRODUCTID
%token <Ast.producttitle> PRODUCTTITLE
%token <Ast.productprice> PRODUCTPRICE
%token <Ast.reviewuserid> REVIEWUSERID
%token <Ast.reviewprofilename> REVIEWPROFILENAME
%token <Ast.reviewhelpfullness> REVIEWHELPFULLNESS
%token <Ast.reviewscore> REVIEWSCORE
%token <Ast.reviewtime> REVIEWTIME
%token <Ast.reviewsummary> REVIEWSUMMARY
%token <Ast.reviewtext> REVIEWTEXT
%token Teof

%start file

%type <Ast.review list> file

%%



review:
   p1 = PRODUCTID
   p2 = PRODUCTTITLE
   p3 = PRODUCTPRICE
   r1 = REVIEWUSERID
   r2 = REVIEWPROFILENAME
   r3 = REVIEWHELPFULLNESS
   r4 = REVIEWSCORE
   r5 = REVIEWTIME
   r6 = REVIEWSUMMARY
   r7 = REVIEWTEXT
   {{           product_id = p1;
                product_title = p2;
                product_price = p3;
                review_user_id = r1;
                review_profile_name= r2;
                review_helpfull_ness = r3;
                review_score = r4;
                review_time = r5;
                review_summary = r6;
                review_text = r7
   
   }}
;

file: lr = list(review) Teof {lr}
;
%%