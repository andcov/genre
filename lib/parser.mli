type 'a parser

val parse : string -> 'a parser -> 'a
val p_str : string -> unit parser
val p_character : char -> unit parser
val p_filter : ('a -> bool) -> 'a parser -> 'a parser
val p_map : ('a -> 'b) -> 'a parser -> 'b parser
val ( <*> ) : 'a parser -> 'b parser -> ('a * 'b) parser
val ( *> ) : 'a parser -> 'b parser -> 'b parser
val ( <* ) : 'a parser -> 'b parser -> 'a parser
val ( <**> ) : 'a parser -> 'b parser Lazy.t -> ('a * 'b) parser
val ( **> ) : 'a parser -> 'b parser Lazy.t -> 'b parser
val p_zero_or_more : 'a parser -> 'a list parser
val p_one_or_more : 'a parser -> 'a list parser
val ( ?? ) : 'a parser -> 'a option parser
val p_char : char parser
val p_digit : int parser
val p_number : int parser
val p_any : 'a parser list -> 'a parser
