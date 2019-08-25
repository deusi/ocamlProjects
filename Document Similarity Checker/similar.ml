open SimUtil

let ngram_n = 4
(* Your code goes here: *)

(* Define the function that lower-cases and filters out non-alphabetic characters *)
(* COMMENT: The function filters out the string input in the way described above. Essential part of the code, as it gets the string ready for further manipulations. *)
let filter_chars s = String.map (fun x -> match x with 'a' .. 'z' -> x | _ -> ' ') (String.lowercase_ascii s)

(* extract a list of n-grams from a string, naively *)
(* COMMENT: Uses string to create ngrams. At this point, still unfiltered *)
let ngrams n s = List.init ((String.length s) - n + 1) (fun x -> String.sub s x n) 

(* Define the function that converts a string into a list of "normalized" n-grams *)
(* COMMENT: Combines filter_chars and ngrams output into a list of 4-ngrams. Essential for further manipulations in the future, since it allows us to calculate the number of occurences of particular ngrams in the input file *)
let n_grams s = List.filter (fun x -> not (String.contains x ' ')) (ngrams ngram_n (filter_chars s))

(* Define a function to convert a list into a bag *)
(* COMMENT: Creates a list of tuples, first element of each tuple being the string representing ngram and second is the number of occurences. *)
let bag_of_list lst =
  let bag_helper lstSort uniqLst tupLst = List.fold_left (fun acc x -> (x,(List.fold_left (fun init y -> if x = y then init+1 else init) 0 lstSort)) :: acc) tupLst uniqLst in
  bag_helper (List.fast_sort (fun x y -> if x = y then 0 else if x > y then 1 else -1) lst) (List.sort_uniq (fun x y -> if x = y then 0 else if x > y then 1 else -1) lst) []

(* Bag utility functions *)

(* multiplicity of e in bag b - 0 if not in the bag *)
(* COMMENT: Helper function for intersection_size. Tells us the number of occurence of a particular element (return second value of a tuple). *)
let multiplicity e b = List.fold_left (fun acc x -> if (fst x) = e then (snd x) else acc) 0 b
  
(* size of a bag is the sum of the multiplicities of its elements *)
(*  COMMENT: Helper function of union_size. Returns the size of a single set. *)
let size b = List.fold_left (fun acc x -> (snd x) + acc) 0 b

(* Define the similarity function between two sets: size of intersection / size of union *)
(* COMMENT: Similarity just uses the formula given above to produce the number which tells us about similarity of two different sets (bags, in our case). *)
let intersection_size s1 s2 = List.fold_left (fun acc x -> (min (snd x) (multiplicity (fst x) s2)) + acc) 0 s1 
let union_size s1 s2 = size s1 + size s2 - (intersection_size s1 s2) 
let similarity s1 s2 = float_of_int (intersection_size s1 s2) /. float_of_int (union_size s1 s2)

(* Find the most similar representative file *)
(* COMMENT: Matches the names of a file to their similarity and then choses the biggest one (by comparing each of them). *)
let find_max repsims repnames = List.fold_left (fun acc x -> max acc x) (0.,"") (List.combine repsims repnames)

let main all replist_name target_name =             (* COMMENT: Main function just compares similarity between given files (replist_name) and target_file. Most of the functions below just apply these two inputs to the functions we defined above. *)
  (* Read the list of representative text files *)
  let repfile_list = SimUtil.file_lines replist_name in
  (* Get the contents of the repfiles and the target file as strings *)
  let rep_contents = List.map (fun x -> SimUtil.file_as_string x) repfile_list in
  let target_contents = SimUtil.file_as_string target_name in
  (* Compute the list of normalized n-grams from each representative *)
  let rep_ngrams = List.map (fun x -> n_grams x) rep_contents in
  (* Convert the target text file into a list of normalized n-grams *)
  let target_ngrams = n_grams target_contents in
  (* Convert all of the stem lists into stem sets *)
  let rep_bags = List.map (fun x -> bag_of_list x) rep_ngrams in
  let target_bag = bag_of_list target_ngrams in
  (* Compute the similarities of each rep set with the target set *)
  let repsims = List.map (fun x -> similarity x target_bag) rep_bags in   (* COMMENT: Creates a list of similarities between each file in the list and the target file. *)
  let (sim,best_rep) = find_max repsims repfile_list in                   (* COMMENT: Just find the file which is most similar to target file. Used in else part. *)
  let () = if all then                                                    (* COMMENT: If all is true, prints every given name in the replist and their similarities to target file. Otherwise, just prints the file which was the most similar to the target file. *)
  (* print out similarities to all representative files *)
  let () = print_endline "File\tSimilarity" in
  (List.iter2 (fun x y -> (print_endline (x ^ "\t" ^ (string_of_float y)))) repfile_list repsims)
  else begin
  (* Print out the winner and similarity *) 
  let () = print_endline ("The most similar file to " ^ target_name ^ " was " ^ best_rep ^ "\n" ^ "Similarity: " ^ (string_of_float sim)) in
  print_endline ""  end in
  (* this last line just makes sure the output prints before the program exits *)
  flush stdout
    
