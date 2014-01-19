


type productid = Product_id of string
type producttitle = Product_title of string
type productprice = Unknown | Product_price of float
type reviewuserid = Review_user_id of string
type reviewprofilename = Review_profile_name of string
type reviewhelpfullness = Review_helpfulness of int*int
type reviewscore = Review_score of float
type reviewtime = Review_time of int
type reviewsummary = Review_summary of string
type reviewtext = Review_text of string

type review = { product_id : productid;
                product_title : producttitle;
                product_price : productprice;
                review_user_id : reviewuserid;
                review_profile_name : reviewprofilename;
                review_helpfull_ness : reviewhelpfullness;
                review_score : reviewscore;
                review_time : reviewtime;
                review_summary : reviewsummary;
                review_text : reviewtext }

type product = Product of productid * review list

type analyseMethode = Simple | Helpfullness | Mode | Anciennete

type minReviewsMethode = Quartile | Median | Mean

exception Lexical_error of string

exception No_review

let twoMonths = 5184000;;

let twoWeeks = 1209600

let nbMinReviews = 5

let smallCoef= 0.1;;
