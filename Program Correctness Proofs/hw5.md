# Homework 5: Reasoning about program correctness

*CSci 2041: Advanced Programming Principles, Spring 2019 (afternoon)*

**Due:** Wednesday, April 10 at 11:59pm

our solutions to this homework should be submitted in markdown files
under the directory `hw5` in your personal class repository.  Since
you're submitting them on GitHub, your files can be in
"Github-Flavored Markdown", so if you want to use mathematical
symbols, you can use their
[HTML character entities](http://dev.w3.org/html5/html-author/charref).

Note: the gitbot cannot offer feedback on your solutions to these problems, it will only verify that you have the correct file names and problem headings.

## 1. Induction on integers and `nat`

*Answers to this problem should appear in the file named `hw5/naturals.md`*

In each of the following, you will use natural or structural induction
to prove that a property applies to some infinite set of elements.
For each proof, you should clearly state the property you are proving,
the base case(s) you are proving, the inductive case(s) you are
proving, and what the inductive hypothesis gives you.  Each step in
your proof must be accompanied by a justification explaining why the
step can be taken.

### `exp`

Consider the following OCaml function definition:
```
let rec exp x n =
  if n = 0 then 1.0
  else x *. (exp x n-1)
  ```

Prove that for all `n : ℕ`, for all `𝓍 : float`:

> `exp 𝓍 n` ≡ 𝓍 <sup>`n`</sup>


### `exp_nat`

Let us adapt `exp` to work on elements of the type `nat` defined in
lecture:

```
type nat = Zero | Succ of nat

let rec to_int n = match n with
| Zero -> 0
| Succ n' -> 1 + (to_int n')

let rec exp_nat x n = match n with
| Zero -> 1.0
| Succ n' -> x *. (exp_nat x n')
```

Use structural induction on the type `nat` to prove that for all `n : nat`, for all `𝓍 : float`,

> `exp 𝓍 (to_int n)` ≡ `exp_nat 𝓍 n`

### `geq_nat`

Suppose we want to write code that compares two structured natural
numbers, `m` and `n`:

```
(* test whether m >= n without converting to integers *)
(* keep decrementing n down to zero, and if we run into m on the way, we know m < n. *)
let rec geq_nat m n = match n with
| Zero -> true (* m is always at least Zero *)
| Succ n' -> if m = n' then false (* n = m+1, so m is not >= n *)
                 else geq_nat m n' (* m >= n iff m > n-1 *)
```

Use structural induction on the type `nat` to prove that for all `m : nat`, for all `n : nat`:

> `geq_nat m n` ≡ `(to_int m) >= (to_int n)`.


## 2. Induction on Lists

*Answers to the following questions should appear in a file named `hw5/list.md`*

Use the following function definitions to answer this question:

```
let rec map f l = match l with
| [] -> []
| h::t -> (f h)::(map f t)

let id = (fun x -> x)

let rec length l = match l with
| [] -> 0
| _::t -> 1 + (length t)

let reverse lst =
  let rec tail_rev lst acc = match lst with
  | [] -> acc
  | h::t -> tail_rev t (h::acc)
  in tail_rev lst []
```

For each of the following identities, use structural induction on
lists to prove the identity is true for all elements ℓ of type `int
list`.  Your proofs should clearly and formally state property P(ℓ)
to be proved,  the base case, the inductive case, and the inductive
hypothesis.

I. ``map id l ≡ l``

II. ``length l ≡ length (reverse l)``

## 3. Polynomials

*Answers to this question should appear in a file named `hw5/poly.md`*

Code to manipulate univariate polynomials (e.g. 𝓍<sup>4</sup> + 3𝓍 + 1, or
(𝓍-3)(𝓍-7)) turns up in many computer science applications, including
graphics, error-correcting codes, computer algebra, machine learning and
optimization theory.  There are two main methods of representing polynomials:
symbolic and implicit.  The implicit representation is a list or vector of
coefficients, one for each power of 𝓍; we won't deal with this representation
in this problem.

The symbolic representation is essentially another expression type:

```
(* Symbolic representation of univariate polynomials *)
type polyExpr =
| Int of int
| X
| Add of polyExpr * polyExpr
| Mul of polyExpr * polyExpr

(* degree of polynomial p *)
let rec deg p = match p with
| Int _ -> 0
| X -> 1
| Add (e1,e2) -> max (deg e1) (deg e2)
| Mul (e1,e2) -> (deg e1) + (deg e2)

(* Compute a representation of p1(p2(X)) by replacing each instance of
X in p1 with p2 *)
let rec compose p1 p2 = match p1 with
| Int _ -> p1
| X -> p2
| Add (e1, e2) -> Add (compose e1 p2, compose e2 p2)
| Mul (e1, e2) -> Mul (compose e1 p2, compose e2 p2)

(* Some simple arithmetic simplifications on polynomials *)
let rec simplify p =
	let simp_add pp = match pp with
	| (Int 0, p) | (p, Int 0) -> p
	| (Int i1, Int i2) -> Int (i1+i2)
	| (p1,p2) -> Add(p1,p2) in
	let simp_mul pp = match pp with
	| (Int 0, _) | (_, Int 0) -> Int 0
	| (Int 1, p) | (p, Int 1) -> p
	| (Int i1, Int i2) -> Int (i1*i2)
	| (p1, p2) -> Mul(p1,p2) in
	match p with
		| Int _ -> p
		| X -> p
		| Add (p1,p2) -> simp_add (simplify p1, simplify p2)
		| Mul (p1,p2) -> simp_mul (simplify p1, simplify p2)
```

Use structural induction to prove the following identities hold for
all inputs of type `polyExpr`.  As in the previous questions, your proofs should clearly and formally state the property P(𝓅) to be proved, the base case, the inductive case, and the inductive hypothesis.

I. ``deg (compose p1 p2) ≡ (deg p1)*(deg p2)``

II. ``deg (simplify p) <= deg p``


## All done!

Don't forget to commit all of your changes to the various files required
for this homework, and push them to your individual class repository
on UMN github:

+ `hw5/naturals.md`
+ `hw5/list.md`
+ `hw5/poly.md`

You can always check that your changes have pushed
correctly by visiting the repository page on github.umn.edu.

## Late Grading

As described in the class syllabus, you may opt to have one homework turned in late but graded for full credit.  If you would like to turn in this homework late, then by 11:59pm on the original due date (Wednesday, April 10, 2019), add a file named `late_request` (the contents can be anything - e.g. your `umn.edu` username.) to the `hw5` directory in your personal repository, and commit and push this to `github.umn.edu`.  If this file is present in your repo, we will wait until *11:59pm on Monday, April 15, 2019* to pull your repo for grading.  Note that choosing this option means you will not be able to submit any other homeworks for late grading. **NOTE: the filename _MUST_ be `late_request` (no extension) and the file _MUST_ be in your github repo by the _REGULAR_ due date in order for the gitbot to detect your request.**

(If you created this file, pushed it, and then change your mind before the original deadline, you can remove the `late_request` file from your repo using `git rm late_request` followed by `git commit` and `git push`.)
