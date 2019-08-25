(* progOpt.ml -- optimizations for our little programming language *)

open Program

(* COMMENT: Helper function to check if the given value is a constant and seq_check function to determine whether to add the value to the sequence or not. *)
   
let is_const = function
 Const _ | BConst _ | LConst _ -> true
 | _ -> false

let seq_check = function
  |Const _ | BConst _ | LConst _  | Name _ -> true
  | _ -> false

(* COMMENT: Function to shorten and simplify the output for pretty much every defined type in the program. Most of them are quite straightforward. Most of the time just evaluate inner expression. If encounters some special cases, behaves accordingly to the way it should handle the case. *)

(* insert (correct) definition of const_fold here *)
let rec const_fold e = match e with
  | (Const _) | (BConst _) | (LConst _ ) | Readint | Name _ -> e
  | Print e -> Print (const_fold e)
  | Apply (e1, e2) -> Apply (const_fold e1, const_fold e2)
  | Add (e1, e2) -> (match ((const_fold e1),(const_fold e2)) with (Const m,Const n) -> Const (m+n) | _ -> e)
  | Sub (e1, e2) -> (match ((const_fold e1),(const_fold e2)) with (Const m,Const n) -> Const (m-n) | _ -> e)
  | Mul (e1, e2) -> (match ((const_fold e1),(const_fold e2)) with (Const m,Const n) -> Const (m*n) | _ -> e)
  | Div (e1, e2) -> (match ((const_fold e1),(const_fold e2)) with (Const m,Const n) -> Const (m/n) | _ -> e) 
  | If (e1, e2, e3) -> if const_fold e1 = BConst true then const_fold e2 else if const_fold e1 = BConst false then const_fold e3 else If (e1, const_fold e2, const_fold e3)
  | Let (str, e1, e2) -> if is_const (const_fold e1) && is_const (const_fold e2) then const_fold e2 else Let (str, const_fold e1, const_fold e2)
  | And (e1, e2) -> (match ((const_fold e1),(const_fold e2)) with (BConst m,BConst n) -> BConst (m&&n) | _ -> e)
  | Or (e1, e2) -> (match ((const_fold e1),(const_fold e2)) with (BConst m,BConst n) -> BConst (m||n) | _ -> e)
  | Not e1 -> (match (const_fold e1) with BConst m -> BConst (not m) | _ -> e)
  | Lt (e1, e2) -> (match ((const_fold e1),(const_fold e2)) with (Const m,Const n) -> BConst (m<n) | _ -> e)
  | Gt (e1, e2) -> (match ((const_fold e1),(const_fold e2)) with (Const m,Const n) -> BConst (m>n) | _ -> e)
  | Eq (e1, e2) -> (match ((const_fold e1),(const_fold e2)) with (Const m,Const n) -> BConst (m=n) | _ -> e)
  | Seq el -> (let rec seq_helper acc lst = match lst with
                | [] -> if (List.length acc = 1 && seq_check (List.hd acc)) then List.hd acc else Seq (List.rev acc)   (* COMMENT: For sequence, evaluates every expression inside the expression list, neglecting all the constants except the last one and simplifiying all non-constant expressions. If the sequence consist of one constant element, return that element instead. *)
                | h::t ->  if (seq_check (const_fold h)) && (List.length lst > 1)
                          then seq_helper acc t else seq_helper ((const_fold h)::acc) t
                        in seq_helper [] el)
  | While (e1, e2) -> if (const_fold e1) = BConst false then Seq [] else While (const_fold e1, const_fold e2)
  | Set (str, e1) -> Set (str, const_fold e1)
  | Fun (str, etype, e1) -> Fun (str, etype, const_fold e1)
  | Head e1 -> (match (const_fold e1) with LConst (h::t) -> Const h | _ -> failwith "Constant folding on head")
  | Tail e1 -> (match (const_fold e1) with LConst (h::t) -> LConst t | _ -> failwith "Constant folding on tail")
  | _ -> e

 (* COMMENT: For Head and Tail, fails if the inner expression of the type which is different from the one that Head and Tail handle. *)
