(* KIF reader *)
(* uses cconv module to read sexp *)

(*
uses tagged strings for encoded data
\b is the tag marker and then 1-char tag type:
s = string
i = int
f = float
u = uri
*)

(*
#require "cconv";;
#require "cconv.sexp";;
*)

open CConv
open CConvSexp
open Sexplib

open Term

(* Char.code 'a' *)
let lowercasep c = c >= 'a'

(* http://stackoverflow.com/questions/10191093/escape-sequences-in-ocaml *)
let unescape s =
  Scanf.sscanf ("\"" ^ s ^ "\"") "%S%!" (fun u -> u)

let encode_value (typecode : char) (str : string) : string =
  Printf.sprintf "\b%c%s" typecode str

let encode_term = let open Encode in sum_fix
    (fun self -> {sum_emit=fun into t -> match t with
         | Str s -> encode_value 's' @@ String.escaped s, []
         | Int i -> encode_value 'i' @@ string_of_int i, []
         | Float f -> encode_value 'f' @@ string_of_float f, []
         | Uri u -> encode_value 'u' @@ String.escaped u, []
         | Sym s -> s, []
         | Var s -> "?" ^ s, []
         | Pred (name, args) -> 
           let args_output = List.map (self.emit into) args in
           name, args_output
       })

let encode_horn = let open Encode in sum {sum_emit = fun into h ->
    let Horn (con, ants) = h in
    "=>", List.map (CConv.encode encode_term into) ants}

let is_encoded_value name = name.[0] == '\x08'

let decode_value str =
  let chars = unescape str in
  let strchars = String.sub chars 2 @@ String.length chars - 2 in
  match str.[1] with
  | 's' -> Term.Str strchars
  | 'i' -> Term.Int (int_of_string strchars)
  | 'f' -> Term.Float (float_of_string strchars)
  | 'u' -> Term.Uri strchars

let decode_term = let open Decode in sum_fix
    (fun self -> {sum_accept=fun src name args -> match name, args with
         | name, [] when name.[0] = '?' ->
           let varname = String.sub name 1 @@ String.length name - 1 in
           Term.Var varname
         | name, [] when is_encoded_value(name) -> decode_value(name)
         | name, [] -> Term.Sym name
         | name, args ->
           let decoded_args = List.map (apply src self) args in
           Term.Pred (name, decoded_args)
         | _ -> CConv.report_error "%s" name
       })

(*
CConv.encode encode_term CConvSexp.target (Pred ("a", [Var "a"; Var "b"])) |> CConv.decode_exn CConvSexp.source decode_term ;;
*)
let decode_horn = let open Decode in sum {sum_accept = fun src name args ->
    let con :: ants = List.map (CConv.decode_exn src decode_term) args in
    Horn (con, ants)}

let load_file (filename : string) =
  let sexps = Sexplib.Macro.load_sexps filename in
  let parse_sexp (ts, hs) s = match s with
    | Sexp.List ((Sexp.Atom "=>")::_) ->
      let horn = CConvSexp.decode_exn decode_horn s in (ts, horn::hs)
    | _ ->
      let term = CConvSexp.decode_exn decode_term s in (term::ts, hs)
  in List.fold_left parse_sexp ([], []) sexps
