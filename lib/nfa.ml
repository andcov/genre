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

let subexpression_to_nfa (i : int ref) (_ : subexpression) : t =
  let start_state = next i in
  let end_state = next i in
  let nfa = create start_state end_state in
  {
    nfa with
    adj =
      add_edge start_state
        { dest = end_state; trans_fn = (fun _ -> true); trans_desc = "sexp" }
        nfa.adj;
  }

let expression_to_nfa (i : int ref) (exp : expression) : t =
  let start_state = next i in
  let end_state = next i in
  let nfa, prev_end_state =
    List.fold exp
      ~init:(create start_state end_state, start_state)
      ~f:(fun (nfa, prev_end_state) s_exp ->
        let s_exp_nfa = subexpression_to_nfa i s_exp in
        let adj =
          Map.merge_skewed
            ~combine:(fun ~key:_ _ _ -> raise Unreachable)
            nfa.adj s_exp_nfa.adj
          |> add_edge prev_end_state (epsilon_trans s_exp_nfa.start_state)
        in
        ({ nfa with adj }, s_exp_nfa.end_state))
  in
  let adj' = add_edge prev_end_state (epsilon_trans end_state) nfa.adj in
  { nfa with adj = adj' }
let add_edge_nfa node (trans : Transition.t) nfa =
  { nfa with adj = add_edge node trans nfa.adj }

let regex_to_nfa (i : int ref) (rg : regex) : t =
  let start_state = next i in
  let end_state = next i in
  let nfa = create start_state end_state in
  match rg with
  | [] ->
      { nfa with adj = add_edge start_state (epsilon_trans end_state) nfa.adj }
  | _ ->
      List.fold rg ~init:nfa ~f:(fun nfa exp ->
          let exp_nfa = expression_to_nfa i exp in
          let adj =
            Map.merge_skewed
              ~combine:(fun ~key:_ _ _ -> raise Unreachable)
              nfa.adj exp_nfa.adj
            |> add_edge start_state (epsilon_trans exp_nfa.start_state)
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