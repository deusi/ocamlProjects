(* Turning a string into a regular expression *)

(* Tokens (substrings that make up regular expressions) *)

(* Added ST and BR which are essential for some of the functions described below. *)
type tok = LP | RP | U | DOT | Ch of char | ST | BR of string

(* turn a string into a list of regex tokens *)

(* tokenize converts a string into a list of tok. Modified version is able to handle both ST and BR *)
let rec tokenize st i =
  if String.length st = i then []
  else if st.[i] = '\\' then read_escape st (i+1)
  else if st.[i] = '[' then read_bracket st (i+1)
  else (match st.[i] with '.' -> DOT | '*' -> ST | c -> Ch c)::(tokenize st (i+1))
and read_escape st i =
  if String.length st = i then failwith "trailing backslash"
  else (match st.[i] with '(' -> [LP]
    | ')' -> [RP]
    | '|' -> [U]
    | '.' | '[' | ']' -> [Ch st.[i]]
    | c -> [Ch '\\'; Ch c]) @ (tokenize st (i+1))

(* read_bracket, well ... read brackets. It uses tolenize in order to create a list of tok and uses special cases in order to construct it properly.
The function is defined in a way that it will be mainly called by tokenize, so calling the function by itself might produce unexpected outcome.
Hope that the function itself is not gonna be called using values without [. Otherwise, it's just gonna be rude. *)
and read_bracket st i =
  if String.index_from_opt st i ']' = None || String.length st = i then failwith "brackets unbalanced"
  else if st.[i] = ']' then BR ("]") :: (tokenize st ((String.index_from st (i+1) ']') +1))
  else BR (String.sub st i ((String.index_from st i ']') - i)) :: (tokenize st ((String.index_from st i ']') +1))

type regexp = Union of regexp * regexp
| Concat of regexp * regexp
| Char of char
| Bracket of (char * char) list * bool
| Star of regexp
| Wild
| NullSet (* matches no strings at all *)

(* This wonder of coding takes a string and transforms it into the most elaborate way of storing data (it took me lots of time to define Bracket properly). What it does is it reads the input from the strings and transform it to the type assigned to Bracket. *)
let parse_bracket (br : string) : regexp =
  if br = "" then (Bracket ([],true)) else  (* Decided to add a special case for empty true bracket, since empty false bracket can be produced *)
  let rec parse_bracket_helper acc st i j =
    if i > j then List.rev acc
    else if (j-i >= 2) && ((st.[i] = '-' && st.[i+1] = '-') || (st.[i+1] = '-' && st.[i] > st.[i+2])) then failwith "invalid range"
    else if (j-i >= 2) && (st.[i+1] = '-' && st.[i] < st.[i+2]) then parse_bracket_helper ( (st.[i],st.[i+2]) :: acc ) st (i+3) j
    else parse_bracket_helper ( (st.[i],st.[i]) :: acc ) st (i+1) j
in if br.[0] = '^' then Bracket (parse_bracket_helper [] br (if br.[0] = '^' then 1 else 0) ((String.length br) - 1),false)
                        else Bracket (parse_bracket_helper [] br (if br.[0] = '^' then 1 else 0) ((String.length br) - 1),true)

(* parse a list of tokens into a regexp *)
(* each function takes a list of tokens, tries to parse a kind of regex from the list,
  and returns the list of unused tokens *)
let rec
  (* parse a "Base" Case regexp: Empty, NullSet, Bracket, Wild, or Char *)
  parse_base_case token_list = match token_list with
  | [] -> (NullSet, [])
  | DOT::tl -> (Wild,tl)
  | LP::tl -> let (re,tl') = parse_regex tl in
    (match tl' with RP::tl'' -> (re,tl'')
    | _ -> failwith "expected close paren")
  | (Ch c)::tl -> (Char c,tl)
  (* Added the case of BR, so it would refer to parse_bracket and make the right regexpr type *) 
  | (BR k)::tl -> ((parse_bracket k),tl)
  | _ -> failwith "unexpected token"
and
  term_helper tl re = match tl with (* parse a "factor" in a regex of the form r1 r2 (r3 ...) *)
  | RP::tl' -> (re,tl)
  | U::tl' -> (re,tl)
  | [] -> (re,[])
  | _ -> let (re_next,tl') = parse_starred_base tl in
    term_helper tl' (Concat (re,re_next))
and
  parse_terms tl = match parse_starred_base tl with (* parse a regex in the form r1 r2 (r3...) *)
  | (t,RP::tl') -> (t, RP::tl')
  | (t,U::tl') -> (t, U::tl')
  | (t,[]) -> (t, [])
  | (t,tl')-> term_helper tl' t
and
  regex_helper tl re = (* helper for parsing union of terms *)
    let (t_next,tl') = parse_terms tl in match tl' with
    | U::tl'' -> regex_helper tl'' (Union (re,t_next))
    | _ -> (Union (re, t_next), tl')
and
  parse_regex tl = match parse_terms tl with (* parse a regex *)
  | (t,U::tl') -> regex_helper tl' t
  | (t,tl') -> t,tl'
and
  (* Handles special Star case. Otherwise just calls parse_base_case *)
  parse_starred_base token_list =
  let star_helper tk =
  match tk with
    | (x,ST::t) -> (Star x, t) (* The parser group is not able to handle multiple stars correctly. However, the hw notes didn't mention it at all, so I decided to leave it be. *)
  | _ -> tk
    in star_helper (parse_base_case token_list)

let rex_parse s =
  let n = String.length s in
  let s' = if n > 1 && s.[0] = '^' then String.sub s 1 (n-1) else (".*"^s) in
  let n' = String.length s' in
  let s'' = if n' > 1 && s'.[n'-1] = '$' then String.sub s' 0 (n'-1) else (s'^".*") in
  let tok_list = tokenize s'' 0 in
  match parse_regex tok_list with
  | (re,[]) -> re
  | _ -> failwith ("regular expression string "^s^" is unbalanced")

(* Looking for a specific character in regexp. Does that until it finds the char or until the list is empty. Nothing else to add, really. *) 
let bracket_match (rexpr : regexp) (c : char) =
  match rexpr with                                                              
  | Bracket (a,b) ->
  (let rec bracket_match_helper lst c = match lst with
    | [] -> false
    | (x,y)::t when x=y -> if x = c then true else bracket_match_helper t c
    | (x,y) :: t -> if c >= x && c <= y then true else bracket_match_helper t c 
  in if b then bracket_match_helper a c else not (bracket_match_helper a c))
  | _ -> invalid_arg "bracket_match"
    
(* Check if an exploded string matches a regex -  a helper function *)
(* uses "continuation"-style search: k is a "continuation function" that
   checks the rest of the string *)

(* Just matches given expression with all the possible cases, handling each one in its manner *)
let rec re_match re s k = match (re,s) with
  (* If the first character matches Char c, continue checking the rest of the string with k *)
  | (Char c, []) -> false
  | (Char c, c2::t) -> (c=c2) && (k t)
  (*Handles Bracket cases *) 
  | (Bracket (e1,e2), []) -> false 
  | (Bracket (e1,e2), e3::t) -> (bracket_match (Bracket (e1,e2)) e3) && (k t)
  (* BRAND NEW, COMPLETELY REDEFINED. Handles Star cases *)                         
  | (Star r, h::t) -> (re_match r s (fun s' -> re_match re s' k))
  | (Star r, t) -> (k t)
  (* Wild always eats a char *)
  | (Wild, []) -> false
  | (Wild, _::t) -> (k t)
  (* Check r1|r2 by first continuing with r1, and if that fails, continue with r2 *)
  | (Union (r1,r2), _) -> (re_match r1 s k) || (re_match r2 s k)
  (* Check r1 r2 by checking r1, and if that succeeds, "continue" with r2 *)
  | (Concat (r1,r2), _) -> (re_match r1 s (fun s' -> re_match r2 s' k))
  | NullSet, _ -> false

let explode s =
  let n = String.length s in
  let rec exphelp i acc =
    if i = n then acc else exphelp (i+1) (s.[i]::acc)
  in List.rev (exphelp 0 [])

(* given a regexp and string, decide if the string matches *)
let regex_match r s =
  re_match r (explode s) ((=) [])

(* Common ocaml idiom: other files will call the regexp type RegExpr.t *)
type t = regexp
