open Ast
open! Base

exception Unreachable

let next i =
  i := !i + 1;
  !i - 1

let next_two i =
  let a, b = (next i, next i) in
  (b, a)

let char_is_in_range ch l r =
  let ch, l, r = Char.(to_int ch, to_int l, to_int r) in
  l <= ch && ch <= r

module Transition = struct
  module T = struct
    type t = {
      dest : int;
      trans_fn : string -> int -> (int, string) Result.t;
      trans_desc : string;
    }
    [@@deriving sexp]

    let compare t1 t2 = Int.compare t1.dest t2.dest
  end

  include T
  include Comparator.Make (T)
end

include Transition

type int_to_transitions_map = Transition.t list Map.M(Int).t [@@deriving sexp]

let epsilon_trans dest_node =
  { dest = dest_node; trans_fn = (fun _ i -> Ok i); trans_desc = {|ε|} }

type t = { start_state : int; end_state : int; adj : int_to_transitions_map }
[@@deriving sexp]

let create_nfa ?edge start_state end_state =
  let adj =
    Map.of_alist_exn
      (module Int)
      [
        (start_state, match edge with Some t -> [ t ] | None -> []);
        (end_state, []);
      ]
  in
  { start_state; end_state; adj }

let add_edge node (trans : Transition.t) adj =
  Map.update adj node ~f:(fun s ->
      let s = Option.value ~default:[] s in
      trans :: s)

let add_edge_nfa node (trans : Transition.t) nfa =
  { nfa with adj = add_edge node trans nfa.adj }

let subexpression_to_nfa ?(start_state : int option) ?(end_state : int option)
    (i : int ref) (s_exp : subexpression) : t =
  let start_state = match start_state with Some s -> s | None -> next i in
  let end_state = match end_state with Some e -> e | None -> next i in
  match s_exp with
  | Match { q; itm } -> (
      let trans =
        match itm with
        | MChar ch ->
            {
              dest = end_state;
              trans_fn =
                (fun s i ->
                  if String.length s > i && Char.equal s.[i] ch then Ok (i + 1)
                  else Error "Could not match character");
              trans_desc = Printf.sprintf {|\"%c\"|} ch;
            }
        | MStr str ->
            {
              dest = end_state;
              trans_fn =
                (fun s i ->
                  if String.is_substring_at s ~pos:i ~substring:str then
                    Ok (i + String.length str)
                  else Error "Could not match string");
              trans_desc = Printf.sprintf {|\"%s\"|} str;
            }
        | MAnyChar ->
            {
              dest = end_state;
              trans_fn =
                (fun s i ->
                  if String.length s > i && (not @@ Char.equal s.[i] '\n') then
                    Ok (i + 1)
                  else Error "Could not match .");
              trans_desc = ".";
            }
        | MCharClass CClassAnyWord ->
            {
              dest = end_state;
              trans_fn =
                (fun s i ->
                  if
                    String.length s > i
                    && (char_is_in_range s.[i] 'A' 'Z'
                       || char_is_in_range s.[i] 'a' 'z'
                       || char_is_in_range s.[i] '0' '9')
                  then Ok (i + 1)
                  else Error "Could not match \\w");
              trans_desc = {|\\w|};
            }
        | MCharClass CClassAnyWordInv ->
            {
              dest = end_state;
              trans_fn =
                (fun s i ->
                  if
                    String.length s > i
                    && not
                         (char_is_in_range s.[i] 'A' 'Z'
                         || char_is_in_range s.[i] 'a' 'z'
                         || char_is_in_range s.[i] '0' '9')
                  then Ok (i + 1)
                  else Error "Could not match \\W");
              trans_desc = {|\\W|};
            }
        | MCharClass CClassAnyDigit ->
            {
              dest = end_state;
              trans_fn =
                (fun s i ->
                  if String.length s > i && char_is_in_range s.[i] '0' '9' then
                    Ok (i + 1)
                  else Error "Could not match \\d");
              trans_desc = {|\\d|};
            }
        | MCharClass CClassAnyDigitInv ->
            {
              dest = end_state;
              trans_fn =
                (fun s i ->
                  if String.length s > i && not (char_is_in_range s.[i] '0' '9')
                  then Ok (i + 1)
                  else Error "Could not match \\D");
              trans_desc = {|\\D|};
            }
        | MCharClass CClassWhitespace ->
            {
              dest = end_state;
              trans_fn =
                (fun s i ->
                  if
                    String.length s > i
                    && List.mem [ ' '; '\n'; '\t'; '\r' ] s.[i]
                         ~equal:Char.equal
                  then Ok (i + 1)
                  else Error "Could not match \\D");
              trans_desc = {|\\s|};
            }
        | MCharClass CClassWhitespaceInv ->
            {
              dest = end_state;
              trans_fn =
                (fun s i ->
                  if
                    String.length s > i
                    && not
                         (List.mem [ ' '; '\n'; '\t'; '\r' ] s.[i]
                            ~equal:Char.equal)
                  then Ok (i + 1)
                  else Error "Could not match \\D");
              trans_desc = {|\\S|};
            }
        | _ -> raise Unreachable
      in
      match q with
      | Some { greedy; typ = QOneOrMore } ->
          let start', end' = next_two i in
          let first_dest, secon_dest =
            if greedy then (start', end_state) else (end_state, start')
          in
          create_nfa start_state end_state
          |> add_edge_nfa start_state (epsilon_trans start')
          |> add_edge_nfa start' { trans with dest = end' }
          |> add_edge_nfa end' (epsilon_trans secon_dest)
          |> add_edge_nfa end' (epsilon_trans first_dest)
      | Some { greedy = true; typ = QZeroOrOne } ->
          create_nfa ~edge:(epsilon_trans end_state) start_state end_state
          |> add_edge_nfa start_state trans
      | Some { greedy = false; typ = QZeroOrOne } ->
          create_nfa ~edge:trans start_state end_state
          |> add_edge_nfa start_state (epsilon_trans end_state)
      | Some { greedy; typ = QZeroOrMore } ->
          let start', end' = next_two i in
          let first_dest, secon_dest =
            if greedy then (start', end_state) else (end_state, start')
          in
          create_nfa start_state end_state
          |> add_edge_nfa start_state (epsilon_trans secon_dest)
          |> add_edge_nfa start_state (epsilon_trans first_dest)
          |> add_edge_nfa start' { trans with dest = end' }
          |> add_edge_nfa end' (epsilon_trans end_state)
          |> add_edge_nfa end' (epsilon_trans start')
      | None -> create_nfa ~edge:trans start_state end_state
      | _ -> raise Unreachable)
  | _ -> raise Unreachable

let expression_to_nfa ?(start_state : int option) ?(end_state : int option)
    (i : int ref) (exp : expression) : t =
  match exp with
  | [ s_exp ] -> subexpression_to_nfa ?start_state ?end_state i s_exp
  | exp ->
      let start_state = match start_state with Some s -> s | None -> next i in
      let end_state = match end_state with Some e -> e | None -> next i in
      let nfa, prev_end_state =
        List.fold exp
          ~init:(create_nfa start_state end_state, start_state)
          ~f:(fun (nfa, prev_end_state) s_exp ->
            let s_exp_nfa =
              subexpression_to_nfa ~start_state:prev_end_state i s_exp
            in
            let adj =
              Map.merge_skewed
                ~combine:(fun ~key:_ v1 v2 -> v2 @ v1)
                nfa.adj s_exp_nfa.adj
              (* |> add_edge prev_end_state (epsilon_trans s_exp_nfa.start_state) *)
            in
            ({ nfa with adj }, s_exp_nfa.end_state))
      in
      let adj' = add_edge prev_end_state (epsilon_trans end_state) nfa.adj in
      { nfa with adj = adj' }

let regex_to_nfa (i : int ref) (rg : regex) : t =
  match rg with
  | [] ->
      let start_state, end_state = next_two i in
      create_nfa ~edge:(epsilon_trans end_state) start_state end_state
  | [ exp ] -> expression_to_nfa i exp
  | _ ->
      let start_state, end_state = next_two i in
      List.fold rg ~init:(create_nfa start_state end_state) ~f:(fun nfa exp ->
          (* TODO: could potentially also set the expression end_state to the regex end_state *)
          let exp_nfa = expression_to_nfa ~start_state i exp in
          let adj =
            Map.merge_skewed
              ~combine:(fun ~key:_ v1 v2 -> v1 @ v2)
              nfa.adj exp_nfa.adj
            |> add_edge exp_nfa.end_state (epsilon_trans end_state)
          in
          { nfa with adj })

let of_regex = regex_to_nfa (ref 0)

let to_dot nfa =
  let s =
    {|digraph finite_state_machine {
    fontname="Helvetica,Arial,sans-serif"
    node [fontname="Helvetica,Arial,sans-serif"]
    edge [fontname="Helvetica,Arial,sans-serif"]
    rankdir=LR;
    node [shape = circle];|}
  in
  let dot =
    Map.fold nfa.adj ~init:s ~f:(fun ~key:node ~data:set s ->
        Set.fold set ~init:s ~f:(fun s dest ->
            Printf.sprintf "%s\n\t%d -> %d [label = \"%s\"];" s node dest.dest
              dest.trans_desc))
  in
  dot ^ "\n}"
