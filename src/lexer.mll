{

  open Lexing
  open Parser
  open Ast
  open Format

} 

let PRODUCTIDs = "product/productId"
let PRODUCTTITLEs = "product/title"
let PRODUCTPRICEs = "product/price"
let REVIEWUSERIDs = "review/userId"
let REVIEWPROFILENAMEs = "review/profileName"
let REVIEWHELPFULLNESSs = "review/helpfulness"
let REVIEWSCOREs = "review/score"
let REVIEWTIMEs = "review/time"
let REVIEWSUMMARYs = "review/summary"
let REVIEWTEXTs = "review/text"

let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let integer = digit+
let float = digit+ '.' digit+
          | digit+
let unknown = "unknown"
let string = [^'\n']*
let code = (alpha | digit)+

rule token = parse
  | '\n'
      { token lexbuf }
  | PRODUCTIDs ": " (code as s) '\n'
      { PRODUCTID (Product_id(s)) }
  | PRODUCTTITLEs ": " (string as s) '\n'
      { PRODUCTTITLE (Product_title(s)) }
  | PRODUCTPRICEs ": " (float as s) '\n'
      { PRODUCTPRICE (Product_price(float_of_string(s))) }
  | PRODUCTPRICEs ": " unknown '\n'
      { PRODUCTPRICE Unknown }
  | REVIEWUSERIDs ": " (code as s) '\n'
      { REVIEWUSERID (Review_user_id(s)) }
  | REVIEWPROFILENAMEs ": " (string as s) '\n'
      { REVIEWPROFILENAME (Review_profile_name(s)) }
  | REVIEWHELPFULLNESSs ": " (integer as i1) '/' (integer as i2) '\n'
      { REVIEWHELPFULLNESS (Review_helpfulness(int_of_string(i1), int_of_string(i2)))}
  | REVIEWSCOREs ": " (float as f) '\n'
      { REVIEWSCORE (Review_score(f))}
  | REVIEWTIMEs ": " (integer as i) '\n'
      { REVIEWTIME (Review_time(int_of_string(i)))}
  | REVIEWSUMMARYs ": " (string as s) '\n'
      { REVIEWSUMMARY (Review_summary(s))}
  | REVIEWTEXTs ": " (string as s) '\n'
      { REVIEWTEXT (Review_text(s))}
  | eof {Teof}
  | [^'\n']+ '\n'
      { raise (Ast.Lexical_error (lexeme lexbuf)) }