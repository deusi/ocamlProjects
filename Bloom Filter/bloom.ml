(* IMPORTANT COMMENT: While using ocamlopt -o bloomtest str.cmxa bloom.ml bloomtest.ml on my computer worked perfectly fine, I've had linking error on some of the lab computers. In case if you have this error as well, use ocamlc -o bloomtest str.cma bloom.ml bloomtest.ml or ocamlc -o bloomtest bloom.ml bloomtest.ml (or one of the iterations of it) *)

(* COMMENT: I didn't find a necessity in adding excessive comments to this file, since the homework itself is quite abstract and Dr. Nick did a good job in explaining how everything works. However, I still added some comments, since it is always sad to lose points due to lack of comments. *)

(* Bloom Filter implementation.  This file will not compile as is. *)
module type memset = sig
    type elt (* type of values stored in the set *)
    type t (* abstract type used to represent a set *)
    val mem : elt -> t -> bool
    val empty : t
    val is_empty : t -> bool
    val add : elt -> t -> t
    val from_list : elt list -> t
    val union : t -> t -> t
    val inter : t -> t -> t
  end

(* Define the hasher signature here *)
(* COMMENT: Hasher signature used for definition of int list which being parsed in filter section *)
module type hasher = sig
    type t
    val hashes : t -> int list
  end

(* Define SparseSet module here, using the Set.Make functor *)
(* COMMENT: Uses Ocaml in-built module Set.Make to define all the properties of SparseSet, while also adding from_list function. *)              
module SparseSet : memset with type elt = int = struct
  include Set.Make(
  struct                              
    let compare = Pervasives.compare
    type t = int
  end )
  (* COMMENT: Adds values from the list to the array which represents existing values. *)
  let from_list lst = List.fold_left (fun x y -> add y x) empty lst
  end                

(* Fill in the implementation of the memset signature here.  You'll need to expose the elt type *)
module BoolSet : memset with type elt = int = struct
  type elt = int
  type t = bool array
  (* element-wise or of two arrays: *)
  (* COMMENT: Implementation of memset signature for Boolset module. Definition of each function is entuitive enough and the names tell the purposes of them *)
    let rec (&@) s1 s2 = let (short,long) =
      if (Array.length s1 < Array.length s2) then (s1,s2) else (s2,s1) in
      Array.mapi (fun i li -> if i < (Array.length short) then li && short.(i) else li) long                   
    let rec (|@) s1 s2 = let (short,long) =
      if (Array.length s1 < Array.length s2) then (s1,s2) else (s2,s1) in
      Array.mapi (fun i li -> if i < (Array.length short) then li || short.(i) else li) long
    let make_arr_t i = let a = (Array.make (i+1) false) in Array.set a i true; a
    let empty = [||]
    let is_empty arr = match arr with
      | [||] -> true
      | _ -> false
    let mem m arr = if arr.(m) then true else false
    let add i arr = arr |@ (make_arr_t i)
    let union arr1 arr2 = arr1 |@ arr2
    let inter arr1 arr2 = arr1 &@ arr2
    let from_list alst = List.fold_left (fun x y -> add y x) empty alst
  end

(* Fill in the implementation of a Bloom.Filter, matching the memset signature, here. *)
(* You will need to add some sharing constraints to the signature below. *)
(* COMMENT: Definition of the filter, which is the essence of bloom filter. Most of the functions are basic and self-explanatory. *)
module Filter(S : memset with type elt = int)(H : hasher) : memset
  with type elt = H.t =
  struct
    type elt = H.t
    type t = S.t
    (* Implement the memset signature: *)
    (*COMMENT: Most of the functions below use S and H local definitions to produce the output. For example, mem applies S.mem to elements of H.hashes and created boolean array, which then folds with && in order to check membership. Add behaves in a similar manner. *)
    let mem lst arr = List.fold_left (&&) true (List.map (fun h -> S.mem h arr) (H.hashes lst))
    let empty = S.empty
    let is_empty arr = S.is_empty arr
    let add lst arr = List.fold_left (fun x y -> S.add y x) arr (H.hashes lst)
    let union arr1 arr2 = S.union arr1 arr2
    let inter arr1 arr2 = S.inter arr1 arr2
    (* COMMENT: From_list has the same definition in both cases. It essentially adds all the elements of the given list together and creates a boolean array with truth value for each number. *)
    let from_list lst = List.fold_left (fun x y -> add y x) empty lst
  end

(* A hashparam module for strings... *)
module StrHash = struct                           (* ??? StrHash or StringHash? ??? *)
    type t = string (* I hash values of type string *)
    let hlen = 15
    let mask = (1 lsl hlen) - 1
    let hashes s =
      let rec hlist n h = if n = 0 then [] else (h land mask)::(hlist (n-1) (h lsr hlen)) in
      hlist 4 (Hashtbl.hash s)
  end

(* Add the IntHash module here *)
(* COMMENT: Creates the output of the type hashes n has. I'm not really sure about what's its practical application in this code, but hey, you can always ask Dr. Nick about that! *)
module IntHash = struct
    type t = int
    let h1 n = (795*n + 962) mod 1031
    let h2 n = (386*n + 517) mod 1031
    let h3 n = (937*n + 693) mod 1031     
    let hashes n = [(h1 n); (h2 n); (h3 n)]
  end

