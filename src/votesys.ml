open Format
open Lexing
open Ast

let file = ref ""

let set_file f s = f := s 

let options = 
  []

let usage = "usage: votesys file.txt"

let () = 
  Arg.parse options (set_file file) usage;

  if !file="" then begin eprintf "Aucun fichier specifie\n@?"; exit 1 end; 

  if not (Filename.check_suffix !file ".txt") then begin
    eprintf "Le fichier d'entree doit avoir l'extension .txt\n@?";
    Arg.usage options usage;
    exit 1
  end;
  (* Ouverture du fichier source en lecture *)
  let f = open_in !file in
    
  (* Création d'un tampon d'analyse lexicale *)
  let buf = Lexing.from_channel f in
  try
    let review_list = Parser.file Lexer.token buf in
    close_in f;
    eprintf "lecture reussi\n@?e";
    exit 0 
  with
    | Ast.Lexical_error c -> 
        eprintf "Erreur dans l'analyse lexicale: %s@." c;
        exit 1
    | _ -> eprintf "erreur\n@?"; exit 1
