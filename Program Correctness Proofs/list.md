### I. ``map id l ≡ l``

### Property

P(l): `map id l ≡ l`

### Base Case

P([]): `map id [] ≡ []`

≡ `[] ≡ []` [eval of map] , ✓

### Inductive Case

∀ l : int list, ∀ h : int. P(l) ⇒ P(h::l)

#### IH:
`map id l` ≡ l

Assuming IH, we need to prove P(h::l): `map id (h::l) ≡ (h::l)`

≡ `(id h)::(map id l) ≡ (h::l)` [eval of map]

≡ `h::(map id l) ≡ (h::l)` [eval of id]

≡ `h::(map id l) ≡ (h::l)` [IH], ✓

(Since if `map id l ≡ l`, then by definition of int lists, it follows that `h::(map id l)` ≡ (h::l))

or alternatively,  (to make it clearer)

 `List.tl (h::(map id l)) ≡ List.tl (h::l)` [applying List.tl to both lists]

≡ `map id l` ≡ `l` [IH], ✓

(Hence it follows that Inductive Case can be reduced to IH)

Since IC holds, it follows that IH holds as well. Thus proved.

Q.E.D.

### II. ``length l ≡ length (reverse l)``

### Property

P(l): `length l ≡ length (reverse l)`

### Base Case

P([]): `length [] ≡ length (reverse [])`

≡ `length [] ≡ length []` [eval of reverse]

≡ 0 ≡ 0 [eval of length], ✓

### Inductive Case

∀ l : int list, ∀ h : int. P(l) ⇒ P(h::l)

#### IH:
`length l ≡ length (reverse l)`

Assuming IH, we need to prove P(h::l): `length (h::l) ≡ length (reverse (h::l))`

≡ `length (h::l) ≡ length (tail_rev (h::l) [])` [eval of reverse]

≡ `length (h::l) ≡ length (tail_rev l (h::[]))`[eval of tail_rev]

≡ `1 + length l ≡ length (tail_rev l (h::[]))`[eval of length]

≡ `length [h] + length l ≡ length (tail_rev l (h::[]))` [reverse eval of length]

(lemma that `length (tail_rev l [])@[h])) ≡ length (tail_rev l []) + length [h]` was proven in ex10. You can find it at the end. Note that `length (append l1 l2) = (length l1) + (length l2)` is simply a more general case, hence should hold)

≡ `length [h] + length l ≡ length (tail_rev l [])@[h])` [substituting to equivalent relation by def. of tail_rev]

≡ `length [h] + length l ≡ length (tail_rev l []) + length [h]` [lemma]

≡ `1 + length l ≡ length (tail_rev l []) + 1` [eval of length]

≡ `length l ≡ length (tail_rev l [])` [subtracting 1 from both sides]

≡ `length l ≡ length (reverse l)` [reverse eval of reverse], ✓

Since we were able to reduce IC to IH, it follows that IC holds and so IH is proven.

Q.E.D.

### Lemma

### `length (append l1 l2) = (length l1) + (length l2)`

### Property
P(l): &forall; l2 . `length (append l l2) = (length l) + (length l2)`

### Base Case
P([]): &forall; l2 . `length (append [] l2) = (length []) + (length l2)`

We have
+ `length (append [] l2)` = `length l2` ***[eval of `append`]***
+ = `0 + (length l2)` ***[arith]***
+ = `(length []) + (length l2)` ***[reverse eval of `length`]***, &check;

### Inductive Case
&forall; l . [&forall; l2 . `length (append l l2) = (length l) + (length l2)`] &rArr; [ &forall; h . &forall; l2. `length (append (h::l) l2) = (length (h::l)) + (length l2)` ]

#### IH: &forall; l2. `length (append l l2) = (length l) + (length l2)`
Let h be an arbitrary integer and l2 an arbitrary int list, then we want to show that `length (append (h::l) l2) = (length (h::l)) + (length l2)`:

+ `length (append (h::l) l2)` = `length (h::(append l l2))` ***[eval of `append`]***
+ = `1 + (length (append l l2))` ***[eval of `length`]***
+ = `1 + (length l) + (length l2)` ***[by IH]***
+ = `(length (h::l)) + (length l2)` ***[reverse eval of `length`]***, &check;
