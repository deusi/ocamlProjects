(* program.ml - a data structure for representing programs *)

type expr =
  Const of int | BConst of bool
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | If of expr * expr * expr
  | Let of string * expr * expr
  | Name of string
  | And of expr * expr
  | Or of expr * expr
  | Not of expr
  | Lt of expr * expr
  | Eq of expr * expr
  | Gt of expr * expr
  | Seq of expr list
  | While of expr * expr
  | Set of string * expr
  | Fun of string * expType * expr
  | Apply of expr * expr
  | Print of expr
  | Readint                (* COMMENT: Added new types to expr in order to handle various input types. Most of the names are intuitive. Readint reads a user-input int and then adds it as Const i *)
  | LConst of int list
  | Cons of expr * expr
  | Head of expr
  | Tail of expr
 and expType = IntT | BoolT | UnitT | FunT of expType * expType | ListT

(* Type to represent a state of the program, e.g. the current stack of variables and the values they are bound to *)
type stType = (string * result) list
 (* Type to represent a value in the program *)
 and result = IntV of int | BoolV of bool | UnitV | ClosureV of expr*string*stType | ListV of int list

(* Searches the stack and updates the most recent binding with the new value *)
let rec assign name value state =
  match state with
  | [] -> failwith "assign to unbound name"
  | (n,v)::t when n=name -> (name,value)::t
  | b::t -> b::(assign name value t)

(* pop a variable binding off the stack *)
let rec pop name state =
  match state with
  | [] -> failwith "popping unbound name: internal error"
  | (n,v)::t when n=name -> t
  | b::t -> b::(pop name t)

(* evaluate an expression: return the value and the new program state *)
let rec eval exp state = match exp with
  | Const n -> (IntV n, state)
  | BConst b -> (BoolV b, state)
  | Readint -> (IntV (read_int ()), state)
  | LConst e -> (ListV e, state)
  | Head e -> evalHead e state     (* COMMENT: Primitives, like Readint and LConst just return the result values of their types. More advanced expressions, like Cons, first evaluate their value in specific sub functions. *)
  | Tail e -> evalTail e state
  | Cons (e1,e2) -> evalCons e1 e2 state
  | Add (e1,e2) -> evalInt (+) e1 e2 state
  | Mul (e1,e2) -> evalInt ( * ) e1 e2 state
  | Sub (e1,e2) -> evalInt (-) e1 e2 state
  | Div (e1,e2) -> evalInt (/) e1 e2 state
  | If (cond,thn,els) -> evalIf cond thn els state
  | Let (nm,vl,exp') -> evalLet nm vl exp' state
  | Name nm -> (List.assoc nm state, state)
  | And (e1,e2) -> evalBool (&&) e1 e2 state
  | Or (e1,e2) -> evalBool (||) e1 e2 state
  | Not e -> let (BoolV b, st') = eval e state in (BoolV (not b), st')
  | Lt (e1, e2) -> evalComp (<) e1 e2 state
  | Eq (e1, e2) -> evalComp (=) e1 e2 state
  | Gt (e1, e2) -> evalComp (>) e1 e2 state
  | Seq elist -> evalSeq elist state
  | While (cond,body) -> evalWhile cond body state
  | Set (name, e) -> let (vl, st') = eval e state in (UnitV, assign name vl st')
  | Fun (argname,_,body) -> (ClosureV (body,argname,state), state) (* "Captures" current environment at definition. *)
  | Apply (f,e) -> evalFunc f e state
  | Print e -> let (r,st') = eval e state in
	       let () = match r with
		 | UnitV -> print_string "()"
		 | IntV i -> print_int i
		 | BoolV b -> print_string (if b then "True" else "False")
                 | ListV l -> print_string ("[" ^ (String.concat " "(List.map (fun x -> string_of_int x) l)) ^ "]")
		 | ClosureV _ -> print_string "<fun>" in
	       let () = print_string "\n" in
	       let () = flush stdout in
	       (UnitV, st')
and evalInt f e1 e2 state =
  let (IntV i1, st1) = eval e1 state in
  let (IntV i2, st2) = eval e2 st1 in
  IntV (f i1 i2), st2
and evalIf cond thn els state =
  let (BoolV b, st') = eval cond state in
  if b then eval thn st' else eval els st'
and evalLet name vl exp state =
  let (r, st') = eval vl state in
  let (r', st'') = eval exp ((name,r)::st') in
  (r', pop name st'')
and evalBool f e1 e2 state =
  let (BoolV b1, st1) = eval e1 state in
  let (BoolV b2, st2) = eval e2 st1 in
  BoolV (f b1 b2), st2
and evalComp cmp e1 e2 state =
  let (r1, st1) = eval e1 state in
  let (r2, st2) = eval e2 st1 in
  (BoolV (cmp r1 r2), st2)
and evalSeq elist st = match elist with (* Whee, tail recursion. *)
  | [] -> (UnitV, st)
  | e::[] -> eval e st
  | e::t -> let (_, st') = eval e st in
	    evalSeq t st'
and evalWhile cond body st = (* Note the tail recursion. An infinite while loop won't blow the stack *)
  let (BoolV b, st') = eval cond st in
  if (not b) then (UnitV, st') else
    let (_, st'') = eval body st' in
    evalWhile cond body st''
and evalFunc f arg state = (* Note: we need to evaluate the function with environment at time of definition *)
  let (ClosureV (body,argname,def_st), st') = eval f state in
  let (argval, st'') = eval arg st' in (* but computing its argument could change state at call site *)
  let (result, _) = eval body ((argname,argval)::def_st) in
  (result, st'') (* So state after call must be the state after argument computation *)
and evalHead intList st = match (eval intList st) with
  | (ListV (h::t),st') -> (IntV h, st')     (* COMMENT: Implement head, tail and cons in the program. Essentially do the same things as their real-Ocaml counterparts. *)
  | _ -> raise (Failure "head")
and evalTail intList st = match (eval intList st) with  (* COMMENT: I know, error cases are redundant, added them for testing in the earlier stages and just decided to keep them, since they don't increase computational time. *)
  | (ListV (h::t),st') -> (ListV t, st')
  | _ -> raise (Failure "tail")
and evalCons const lst st = match eval const st with
  | (IntV n,st') -> (match eval lst st' with (ListV l, state) -> (ListV (n::l),state) | _ -> raise (Failure "cons2")) (* COMMENT: Stuck a big time on this one. Need to evaluate them separately to pass on the proper state, *)
  | _ -> raise (Failure "cons")
   

(* Type checking/inference: Figure out type for an expression.  Fail if the expression is not well-typed.*)
let rec typeof exp env = match exp with
  | Const _ -> IntT
  | BConst _ -> BoolT
  | Readint -> IntT
  | LConst _ -> ListT
  | Head e -> (match (typeof e env) with ListT -> IntT | _ -> failwith "head of non-list") (* COMMENT: Defined types of expressions I added earlier. Resulting outputs are counterparts of real-Ocaml types of lists, heads, tails and so on. *)
  | Tail e -> (match (typeof e env) with ListT -> ListT | _ -> failwith "head of non-list") (* e : ListT => Tail e : ListT *)
  | Cons (e1,e2) ->                                                                         (* e1 : IntT, e2 : ListT => Cons (e1,e2) : ListT *)
     (match (typeof e1 env, typeof e2 env) with
      | (IntT,ListT) -> ListT
      | _ -> failwith "incompatible cons arguments")
  | Add (e1,e2) | Sub (e1,e2) | Mul (e1,e2)
  | Div (e1,e2) ->
     ( match (typeof e1 env, typeof e2 env) with
       | (IntT,IntT) -> IntT
       | _ -> failwith "Arithmetic on non-integer arguments")
  | And (e1,e2)
  | Or (e1,e2) ->
     ( match (typeof e1 env, typeof e2 env) with
       | (BoolT,BoolT) -> BoolT
       | _ -> failwith "Boolean operation on non-Bool arguments")
  | Not e -> if (typeof e env) = BoolT then BoolT else failwith "Not of non-Boolean"
  | Lt (e1,e2)
  | Gt (e1,e2) ->
     ( match (typeof e1 env, typeof e2 env) with
       | (IntT,IntT) -> BoolT
       | _ -> failwith "Comparison of non-integer values" )
  | Eq (e1,e2) ->
     ( match (typeof e1 env, typeof e2 env) with
       | (IntT,IntT) | (BoolT,BoolT) | (UnitT,UnitT) | (ListT,ListT) -> BoolT
       | _ -> failwith "Equality test on incompatible values" )
  | If (cond,thn,els) ->
     if not ((typeof cond env) = BoolT) then failwith "If on non-boolean condition" else
       let (t1,t2) = (typeof thn env, typeof els env) in
       if (t1 = t2) then t1 else failwith "Different types for then/else branches"
  | Name name -> (try List.assoc name env with Not_found -> failwith ("Unbound variable "^name))
  | Let (name,vl,e) ->
     let t = typeof vl env in
     typeof e ((name,t)::env)
  | Seq elist -> seqType elist env
  | While (c,body) ->
     ( match (typeof c env, typeof body env) with
       | (BoolT, _) -> UnitT
       | _ -> failwith "Non-boolean condition for while")
  | Set (name, e) -> if (typeof (Name name) env) = (typeof e env) then UnitT else failwith "assign type mismatch"
  | Fun (argname, argType, body) ->
     let resType = typeof body ((argname,argType)::env) in
     FunT (argType,resType)
  | Apply (e1,e2) ->
     ( match (typeof e1 env) with
       | FunT (argtype, restype) -> if (typeof e2 env) = argtype then restype
				       else failwith "incompatible function argument"
       | _ -> failwith "Apply of non-function value")
  | Print e -> let _ = typeof e env in UnitT
and seqType el env = match el with
  | [] -> UnitT
  | [e] -> typeof e env
  | e::rest -> let _ = typeof e env in seqType rest env
