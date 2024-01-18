type 'a parser = char list -> 'a * char list

let ( >> ) f g x = g (f x)
let str_to_ch_list = String.to_seq >> List.of_seq
let c_is_digit = function '0' .. '9' -> true | _ -> false
let c_to_int ch = Char.(code ch - code '0')

exception StrError of string
exception CharError of char
exception FilterError
exception NotEnoughMatchesError
exception LengthError
exception NoMatch

let parse str p = fst @@ p @@ str_to_ch_list str

let p_str to_match chs =
  let rec matching prefs chs =
    match (prefs, chs) with
    | p :: ps, c :: cs when p == c -> matching ps cs
    | [], rst -> ((), rst)
    | _ -> raise (StrError to_match)
  in
  matching (str_to_ch_list to_match) chs

let p_character to_match chs =
  match chs with
  | c :: rst when to_match == c -> ((), rst)
  | _ -> raise (CharError to_match)

let p_filter pred p chs =
  match p chs with
  | res, rest when pred res -> (res, rest)
  | _ -> raise FilterError

let p_map f p chs =
  let v, r = p chs in
  (f v, r)

let ( <*> ) p1 p2 chs =
  let l, chs = p1 chs in
  let r, rst = p2 chs in
  ((l, r), rst)

let ( *> ) p1 p2 chs =
  let (_, r), rst = (p1 <*> p2) chs in
  (r, rst)

let ( <* ) p1 p2 chs =
  let (l, _), rst = (p1 <*> p2) chs in
  (l, rst)

let ( <**> ) p1 p2 chs =
  let l, chs = p1 chs in
  let r, rst = (Lazy.force p2) chs in
  ((l, r), rst)

let ( **> ) p1 p2 chs =
  let _, chs = p1 chs in
  let r, rst = (Lazy.force p2) chs in
  (r, rst)

let rec p_any parsers chs =
  match parsers with
  | p :: rst -> ( try p chs with _ -> p_any rst chs)
  | [] -> raise NoMatch

let p_zero_or_more p chs =
  let rec matching chs acc =
    try
      if List.length chs <> 0 then
        let res, rest = p chs in
        matching rest (res :: acc)
      else (List.rev acc, chs)
    with _ -> (List.rev acc, chs)
  in
  matching chs []

let p_one_or_more p chs =
  match p_zero_or_more p chs with
  | [], _ -> raise NotEnoughMatchesError
  | vals, chars -> (vals, chars)

let ( ?? ) p chs =
  try
    let v, r = p chs in
    (Some v, r)
  with _ -> (None, chs)

let p_char = function ch :: rest -> (ch, rest) | _ -> raise LengthError
let p_digit = p_filter c_is_digit p_char |> p_map c_to_int

let p_number =
  p_one_or_more p_digit
  |> p_map (List.fold_left (fun acc i -> (acc * 10) + i) 0)
