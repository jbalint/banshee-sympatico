(*
TODO
   - better presentation of extracted answers
   - negation (as failure? classical? Well-founded semantics?)
   - add Date, Time, DateTime types (gYear too?)
*)

open Term

(********************************************************************************)
(* unification *)

(* substitutions map variable names to their term replacements.

   example:

   SubMap is X => (Const "x"),
           Y => (Const "1")

   apply this sub to (Var "X") gives us (Const "x")
   apply this sub to (Var "x") gives us (Var "x") ;; no substitution

*)
module SubMap = Map.Make(String)

let rec term_to_str = function
  | Sym x -> x
  | Var x -> "?" ^ x
  | Int x -> string_of_int x
  | Float x -> string_of_float x
  | Str x -> "\"" ^ x ^ "\""
  | Uri x -> "<" ^ x ^ ">"
  | Pred (p, args) ->
    let args_str = List.map term_to_str args |> String.concat ", " in
    p ^ "(" ^ args_str ^ ")"

let horn_to_str : horn_clause -> string = function
  | Horn (head, body) -> (term_to_str head) ^ " :- " ^ 
                         (String.concat ", " (List.map term_to_str body)) ^ "."

let sub_to_str sub = 
  let entry_to_str (k, v) = (term_to_str v) ^ "/" ^ k in
  String.concat "; " (List.map entry_to_str (SubMap.bindings sub))

let xsub_to_str = function
  | None -> "<empty>"
  | Some sub -> sub_to_str sub

(* apply a substitution *)
let rec apply_sub sub = function
  | Var a when SubMap.mem a sub -> apply_sub sub (SubMap.find a sub)
  | x -> x

let apply_option_subs subs t =
  let apply_opt_sub t s =
    match s with
    | Some sub -> apply_sub sub t
    | None -> t
  in
  List.fold_left apply_opt_sub t subs

(* builds a SubMap representing the unifier

   takes as input two argument lists. each list is the arguments to a predicate
   (predicate name matching happens externally)

   To unify bsP(bsW P) and bsP(bsW bsJ)
   we would call `unify' as:

   unify [Const "bsW"; Var "P"] [Const "bsW"; Const "bsJ"]

   This returns the substitution: P -> bsJ
   a.k.a. bsJ/P

   with respect to variables the second pair will replace the first pair:

   unify [Const "x"; Var "B"] [Const "x"; Var "C"] |> xsub_to_str;;

   returns {C/B}

*)

let apply_sub_to_pair (sub : Term.term SubMap.t) (pair : Term.term * Term.term) : Term.term * Term.term =
  let (a, b) = pair in
  (apply_sub sub a, apply_sub sub b)

let unify args1 args2 =
  let unify_terms sub = function
    | (Var a, Var b) when a <> b -> Some (SubMap.add a (Var b) sub)
    | (Var a, Var b) -> Some sub
    | (Var a, Uri b) | (Uri b, Var a) -> Some (SubMap.add a (Uri b) sub)
    | (Var a, Int b) | (Int b, Var a) -> Some (SubMap.add a (Int b) sub)
    | (Var a, Float b) | (Float b, Var a) -> Some (SubMap.add a (Float b) sub)
    | (Var a, Str b) | (Str b, Var a) -> Some (SubMap.add a (Str b) sub)
    | (Var a, Sym b) | (Sym b, Var a) -> Some (SubMap.add a (Sym b) sub)
    | (Sym a, Sym b) when a = b -> Some sub
    | _ -> None (* fail *)
  in
  let do_fold (a : Term.term SubMap.t option) (b : Term.term * Term.term) : Term.term SubMap.t option =
    match a with
    | None -> None
    | Some sub -> unify_terms sub @@ apply_sub_to_pair sub b
  in
  let pairs = List.combine args1 args2 in
  List.fold_left do_fold (Some SubMap.empty) pairs

(*********************)
(* index, for search *)

module TermMap = Map.Make(String)

type index_entry = TermEntry of term | HornEntry of horn_clause

let add_to_index k x idx =
  let new_val = if TermMap.mem k idx then
      x :: (TermMap.find k idx)
    else [x]
  in TermMap.add k new_val idx

let add_term idx = function
  (* best representation inside the index? *)
  | Pred (name, args) -> add_to_index name (TermEntry (Pred (name, args))) idx
  | _ -> idx

let add_horn_clause idx x =
  match x with
  (* this doesn't handle non-predicate heads - doesn't need to? *)
  | Horn (Pred (head_name, _), body) -> add_to_index head_name (HornEntry x) idx
  | _ -> idx

(********************************************************************************)
(* resolution *)

(* unify with predicate *)
let unify_wpred args pred = match pred with
  | Pred (_, pargs) -> unify pargs args

let apply_to_args args subs = 
  let apply = apply_option_subs subs in
  List.map apply args

let apply_to_pred sub = function
  | Pred (name, args) -> Pred (name, apply_to_args args [sub])

let dec2_solve term_index goals =
  let xy = ref 0 in
  let rec dec2_solve_pre (goals : term list) =

    (* for debugging *)
    let () = Printf.printf "***** Solving %s\n\n" @@ String.concat "\n" (List.map term_to_str goals) in
    (* add depth limit to prevent recursion bugs *)
    (* this is technically implemented as a counter, not as a depth check *)
    let () = match !xy with
      | n when n > 500 -> Failure "depth limit hit" |> raise
      | n -> xy := n + 1
    in

    let goal::rest = goals in
    let Pred (goal_name, goal_args) = goal in

    let matching_terms = TermMap.find goal_name term_index in
    let handle_term term =
      let (pred, subgoals) = match term with
        | TermEntry pred -> (pred, rest)
        | HornEntry Horn (pred, ants) -> (pred, rest @ ants)
      in
      match (unify_wpred goal_args pred, subgoals) with
      | (None, _) -> [] (* doesn't unify *)
      | (sub, []) -> [[sub]] (* terminate *)
      | (sub, subgoals) -> let subgoals_subst = List.map (apply_to_pred sub) subgoals in
        dec2_solve_pre subgoals_subst |> List.map (List.append [sub])
    in List.map handle_term matching_terms |> List.concat
  in dec2_solve_pre goals

(************************************************)
let build_solver_with_input_file input_file =
  let data = load_file input_file in
  let term_index = 
    let x = List.fold_left add_term TermMap.empty (fst data) in
    List.fold_left add_horn_clause x (snd data)
  in 
  fun queries ->
    dec2_solve term_index (List.map (CConvSexp.of_string_exn decode_term) queries)

let rdf_solver = build_solver_with_input_file "kifexample_rdf.kif"

let _ = rdf_solver ["(rdfs:subClassOf eg:Paper ?AncestorsOfPaper)"] |> sublist_to_str
let _ = rdf_solver ["(rdfs:subClassOf ?SubclassesOfTreeProduct eg:TreeProduct)"] |> sublist_to_str
