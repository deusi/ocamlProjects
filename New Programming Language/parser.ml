open Program

(* small example programs:
   Because we're keeping type inference simple, we'll require functions to have a single argument,
   and declare the type of that argument after a colon.
   To simplify parsing function applications, we'll have explicit "app" expressions, so to apply function f to argument x,
   the program will say (app f x).  A multiple argument function will need to be applied to each argument in turn, so the equivalent to the
   Ocaml expression (f x y) will be (app (app f x) y).
*)
let example1 =
  "(let f (fun g : int -> int  (app g 0))
          (print (app f (fun x : int (+ x 2)))))"

let example2 =
  "(let gcd (fun a : int (fun b : int
            (let q (/ a b)
            (let r (- a (* q b))
                   (seq (while (not (= r 0))
                               (seq (set a b)
                                    (set b r)
                                    (set q (/ a b))
                                    (set r (- a (* q b)))))
                         b)))))
            (print (app (app gcd 217) 527)))"

let example3 =
"(let y 0
    (let x (+ 1 2)
        (while (> x 0)
            (seq
                (set x (- x 1))
                (if (> 1 0) x y)
                (if (< 1 0) x y)
                (while (> 1 0) (print y))
                (+ 1 2)
                (seq (let z (+ 2 3) (< 1 2)))
            )
        )
    )
)"

let example4 =
"(let y (* 0 0)
  (let z (if (> y 4)
           (seq (set y (- y 1)) 1)
           0)
    (seq (print y)
         (if (= (+ 1 0) (- 2 1))
             (let z readint z)
             (+ 42 17)))
  )
)"

(* We want this one to work, eventually *)
let example5 =
"(let rev
    (fun in : list
      (let out []
        (seq
          (while (not (= in []))
            (seq
              (set out (cons (head in) out))
              (set in (tail in))))
          out)))
    (app rev [1 2 3 4 5]))"

(* And this longer one, too... *)
let example6 =
"(let readlist
    (fun x : unit
      (let out []
      (let cont true
      (let x 0
      (seq
      (while cont
        (seq
          (set x readint)
          (if (< x 0) (set cont false) (set out (cons x out)))))
      out)))))
(let printlist
  (fun l : list
    (while (not (= l []))
      (seq
        (print (head l))
        (set l (tail l)))))
(let rev
  (fun in : list
    (let out []
      (seq
        (while (not (= in []))
          (seq
            (set out (cons (head in) out))
            (set in (tail in))))
        out)))
(app printlist (app rev (app readlist ()))))))"

(* all of the lexical tokens we might encounter in a program *)
type token = OP | CP | AND | OR | NOT | PLUS | MINUS | TIMES | DIV | LET | ID of string | CONST of int | BCONST of bool | LT | GT | EQ | IF |
	     SET | SEQ | WHILE | PRINT |
	     APP | FUN | COLON | ARROW | INT | BOOL | UNIT | READ | LB | RB | CONS | TAIL | HEAD | LIST

(* COMMENT: Added some new tokens to handel new expr types I added before in program.ml *)

(* Split a string into a list of words, delimited by spaces, parens, colons, and -> *)
(* never mind the magic regexp *)
let wordlist s =
  let splitlist = Str.full_split (Str.regexp "\\b\\|(\\|)\\|:\\|\\(->\\)\\|\\[\\|\\]") s in
  let rec filter_splist lst = match lst with
    | [] -> []
    | (Str.Delim "(")::t -> "(" :: (filter_splist t)
    | (Str.Delim ")")::t -> ")" :: (filter_splist t)
    | (Str.Delim "[")::t -> "[" :: (filter_splist t)
    | (Str.Delim "]")::t -> "]" :: (filter_splist t)
    | (Str.Delim "->")::t -> "->" :: (filter_splist t)
    | (Str.Delim ":")::t -> ":"::(filter_splist t)
    | (Str.Delim _) :: t -> filter_splist t
    | (Str.Text s) :: t -> let s' = String.trim s in
                           let t' = (filter_splist t) in
                           if not (s' = "") then s' :: t' else t'
  in filter_splist splitlist

(* turn a word into a token *)
let tokenize_string = function
  | "(" -> OP
  | ")" -> CP
  | "[" -> LB
  | "]" -> RB
  | "and" -> AND
  | "or" -> OR
  | "not" -> NOT
  | "+" -> PLUS
  | "*" -> TIMES
  | "-" -> MINUS
  | "/" -> DIV
  | "let" -> LET
  | ">" -> GT
  | "<" -> LT
  | "=" -> EQ
  | "if" -> IF
  | "set" -> SET
  | "seq" -> SEQ
  | "while" -> WHILE
  | "app" -> APP
  | "fun" -> FUN
  | ":" -> COLON
  | "->" -> ARROW
  | "int" -> INT
  | "bool" -> BOOL
  | "unit" -> UNIT
  | "print" -> PRINT
  | "true" -> BCONST true
  | "false" -> BCONST false
  | "readint" -> READ
  | "cons" -> CONS
  | "tail" -> TAIL
  | "head" -> HEAD
  | "list" -> LIST
  | s -> if Str.string_match (Str.regexp "[0-9]+") s 0 then (CONST (int_of_string s))
	 else if Str.string_match (Str.regexp "[a-z]+") s 0 then (ID s) else failwith ("invalid token:"^s)

(* and a list of words into a list of tokens *)
let tokens wl = List.map tokenize_string wl

(* find_line should go here : given the lines of a file and a token index,
  on which line does the token occur? *)
      
(* COMMENT: Being used in interpreter.ml to find a line on which an error occured. Here, just finds the line corresponding to the ind number after subtracting a length of string, so ind < 0 *)
let find_line lst ind = 
  let rec find_line_helper l i c = match l with
    | [] -> c
    | h::t -> if i >= (List.length (wordlist h)) then find_line_helper t (i - (List.length (wordlist h))) (c+1) else c
            in find_line_helper lst ind 1


exception ParseError of string * int  (* COMMENT: Added a new exception which displays the message of the error and the number of remaining tokens. Also changed every failwith after this line to that exception *)

(* Parse a type expression in a function definition.
   Return the type and the list of unused tokens for further parsing.
   A type expression is either: INT, BOOL, UNIT, LIST or  (typeExpr)  or typeExpr -> typeExpr  *)
let rec _parse_type_expr tlist =
  let (ty1, tl) =
    match tlist with
    | INT::t -> (IntT,t)
    | BOOL::t -> (BoolT,t)
    | UNIT::t -> (UnitT,t)
    | LIST::t -> (ListT,t)
    (* Read up until we find a close paren: covers types like "(int->bool) -> int" *)
    | OP::t -> (match _parse_type_expr t with
                | ty, CP::t' -> (ty,t')
	              | _,t' -> raise (ParseError ("imbalanced parentheses in type expression", (List.length t'))))
    | _ -> raise (ParseError ("unexpected token in type expression.", (List.length tlist)))
  in match tl with (* peek at tail: is there an arrow (so more type expr to read)? *)
     | ARROW::t1 -> let (ty2, t2) = _parse_type_expr t1 in (FunT(ty1,ty2),t2)
     | _ -> (ty1,tl) (* No, we're done here. *)


  (* parse an expression from a list of tokens, returning the expression and the list of unused tokens *)
let rec _parser tlist = match tlist with
  | [] -> raise (ParseError ("Ran out of tokens without closing parenthesis", (List.length tlist)))
| (BCONST b)::t -> (BConst b,t)
| (CONST i)::t -> (Const i, t)
| (ID s)::t -> (Name s, t)
| READ::t -> (Readint, t)       (* COMMENT: Added definition for each newly added token. Primitive tokens are self-explanatory. More complex ones call helper functions *)
| OP::HEAD::t -> let (e,t') = _parse_single t in (Head(e),t')
| OP::TAIL::t -> let (e,t') = _parse_single t in (Tail(e),t')
| OP::CONS::t -> let (e1,e2,t') = _parse_two t in (Cons (e1,e2), t')
                                               
| LB::t -> let (lst,t') = _parse_list_literals [] t in (LConst lst, t') 

| OP::PLUS::t -> let (e1,e2,t') = _parse_two t in (Add (e1,e2), t')
| OP::MINUS::t -> let (e1,e2,t') = _parse_two t in (Sub (e1,e2), t')
| OP::TIMES::t -> let (e1,e2,t') = _parse_two t in (Mul (e1,e2), t')
| OP::DIV::t -> let (e1,e2,t') = _parse_two t in (Div (e1,e2), t')
| OP::AND::t -> let (e1,e2,t') = _parse_two t in (And (e1,e2), t')
| OP::OR::t -> let (e1,e2,t') = _parse_two t in (Or (e1,e2), t')
| OP::EQ::t -> let (e1,e2,t') = _parse_two t in (Eq (e1,e2), t')
| OP::GT::t -> let (e1,e2,t') = _parse_two t in (Gt (e1,e2), t')
| OP::LT::t -> let (e1,e2,t') = _parse_two t in (Lt (e1,e2), t')
| OP::WHILE::t -> let (e1,e2,t') = _parse_two t in (While (e1,e2), t')
| OP::APP::t -> let (e1,e2,t') = _parse_two t in (Apply (e1,e2), t')
| OP::FUN::(ID s)::COLON::t ->
       let (tExp, t') = _parse_type_expr t in
       let (bExp,t'') = _parse_single t' in (Fun (s,tExp,bExp),t'')
| OP::LET::(ID s)::t -> let (v,e,t') = _parse_two t in (Let (s,v,e), t')
| OP::SET::(ID s)::t -> let (e,t') = _parse_single t in (Set (s,e), t')
| OP::IF::t -> let (c,t1) = _parser t in
    let (thn,els,t2) = _parse_two t1 in (If (c,thn,els), t2)
| OP::NOT::t -> let (e,t') = _parse_single t in (Not(e),t')
| OP::PRINT::t -> let (e,t') = _parse_single t in (Print(e), t')
| OP::SEQ::t -> let (l,t') = _parse_list t in (Seq(l),t')
| OP::CP::t -> (Seq [], t)
| _ -> raise (ParseError ("Unexpected token: unbalanced parentheses or keyword out of call position", (List.length tlist)))

(* parse a single expression and "eat" the following close paren *)
and _parse_single t = match _parser t with
| e, CP::t'' -> (e,t'')
| _,t' -> raise (ParseError ("parser: missing closing paren.", (List.length t')))

(* "eat" the close paren after two expressions *)
and _parse_two t =
  let (e1,t1) = _parser t in
  let (e2,t2) = _parser t1 in
  match t2 with CP::t' -> (e1,e2,t')
  | _ -> raise (ParseError ("parser: missing closing paren.", (List.length t2)))

(* parse a list of expressions, consuming the final closing paren *)
and _parse_list t =  match t with
| CP::t' -> ([],t')
| [] -> raise (ParseError ("unfinished expression sequence: missing close paren(s).", (List.length t))) 
| _ -> let (e,t1) = _parser t in
       let (el,t2) = _parse_list t1 in (e::el, t2)

                                     
and _parse_list_literals acc t = match t with   (* COMMENT: Handles [ and ]. Accepts only integer values inside the parenthesis. Fails if one of the values is not an int or if closing parenthesis are missing. *)
  | RB::t' -> ((List.rev acc),t')
  | CONST i::t' -> _parse_list_literals (i::acc) t'
  | _::[] -> raise (ParseError ("unclosed list literal", (List.length t)))
  | _::t' -> raise (ParseError ("list literal with non-int element", (List.length t')))
                                      


let parse_program tlist = match _parser tlist with
| e,[] -> e
| _,t -> raise (ParseError ("parse failed: extra tokens in input.", (List.length t)))
