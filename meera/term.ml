(* term stuff *)
module Term = struct
  type term =
    (* constants *)
    | Uri of string
    | Str of string
    | Int of int
    | Float of float
    (* symbol *)
    | Sym of string
    (* variable *)
    | Var of string
    (* predicate: name + args *)
    | Pred of string * term list

  (* horn clause: head, body clauses *)
  type horn_clause = Horn of term * term list
end
