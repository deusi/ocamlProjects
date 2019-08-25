### I. ``deg (compose p1 p2) ≡ (deg p1)*(deg p2)``

### Property

P(p1): ∀ p2: polyExpr. `deg (compose p1 p2) ≡ (deg p1)*(deg p2)`

### Base Cases

+ Int i:

P(Int i): ∀ p2: polyExpr. `deg (compose (Int i) p2) ≡ (deg (Int i))*(deg p2)`

≡ `deg (Int i) ≡ (deg (Int i))*(deg p2)` [eval of compose]

≡ `0 ≡ 0*(deg p2)` [eval of deg]


≡ `0 ≡ 0` [simplification], ✓

+ X:

P(X): ∀ p2: polyExpr. `deg (compose X p2) ≡ (deg X)*(deg p2)`

≡ `deg p2 ≡ (deg X)*(deg p2)` [eval of compose]

≡ `deg p2 ≡ 1*(deg p2)` [eval of deg]

≡ `deg p2 ≡ deg p2` [simplification], ✓

### Inductive Case

∀ pe1: polyExpr, ∀ pe2: polyExpr, ∀ pv: polyExpr. P(pe1) && P(pe2) ⇒ P(Add(pe1,pe2))

#### IH:
∀ pv: polyExpr. `deg (compose pe1 pv) ≡ (deg pe1)*(deg pv)` && `deg (compose pe2 pv) ≡ (deg pe2)*(deg pv)`

Assuming IH, we need to prove P(Add(pe1,pe2)): ∀ pv: polyExpr. `deg (compose Add(pe1,pe2) pv) ≡ (deg Add(pe1,pe2))*(deg pv)`

≡ `deg (Add (compose pe1 pv, compose pe2 pv)) ≡ (deg Add(pe1,pe2))*(deg pv)` [eval of compose]

≡ `max (deg (compose pe1 pv)) (deg (compose pe2 pv)) ≡ (max (deg pe1) (deg pe2))*(deg pv)` [eval of deg]

Case 1: pe1 = pe2

`deg (compose pe1 pv) ≡ (deg pe1)*(deg pv)` [IH], ✓

Case 2: pe1 > pe2 

(by definition, if deg pe1 > deg pe2, then (deg (compose pe1 pv)) > (deg (compose pe2 pv)), otherwise, by IH, if `deg (compose pe1 pv) ≡ (deg pe2)*(deg pv)`, then `deg (compose pe1 pv) ≡ deg (compose pe2 pv)` and hence it is reduced to case 1)

`deg (compose pe1 pv) ≡ (deg pe1)*(deg pv)` [IH], ✓

Case 3: pe1 < pe2

`deg (compose pe2 pv) ≡ (deg pe2)*(deg pv)` [IH], ✓

Since for IH to be true, both of its parts have to be true, it folows that IC will hold as long as we are able to reduce it to one of the subexpressions of IH. Hence proved.

### Inductive Case

∀ pe1: polyExpr, ∀ pe2: polyExpr, ∀ pv: polyExpr. P(pe1) && P(pe2) ⇒ P(Mul(pe1,pe2))

#### IH: 
∀ pv: polyExpr. `deg (compose pe1 pv) ≡ (deg pe1)*(deg pv)` && `deg (compose pe2 pv) ≡ (deg pe2)*(deg pv)`

Assuming IH, we need to prove P(Mul,pe1,pe2): `deg (compose Mul(pe1,pe2) pv) ≡ (deg Mul(pe1,pe2))*(deg pv)`

≡ `deg (Mul (compose pe1 pv, compose pe2 pv)) ≡ (deg Mul(pe1,pe2))*(deg pv)` [eval of compose]

≡ `deg (compose pe1 pv) + deg (compose pe2 pv) ≡ ((deg pe1) + (deg pe2))*(deg pv)` [eval of deg]

≡ `deg (compose pe1 pv) + deg (compose pe2 pv) ≡ (deg pe1)*(deg pv) + (deg pe2)*(deg pv)` [distributive property]

(By IH, P(pe1) and P(pe2) both hold. Hence, by the definition of summation, their sum will also hold)

≡ `deg (compose pe1 pv) + deg (compose pe2 pv) ≡ (deg pe1)*(deg pv) + (deg pe2)*(deg pv)` [IH], ✓

Since we are given that `deg (compose pe1 pv) ≡ (deg pe1)*(deg pv)` && `deg (compose pe2 pv) ≡ (deg pe2)*(deg pv)`, it follows that their sum also holds the equality. Hence proven.

Both IH hold, then it follows that P(p1) is true for every possible case. Hence proven.

Q.E.D.

### II. ``deg (simplify p) <= deg p``

### Property 

P(p): `deg (simplify p) <= deg p`

### Base Case

+ Int i:

P(Int i): `deg (simplify (Int i)) <= deg (Int i)`

≡ `deg (Int i) <= deg (Int i)` [eval of simplify]

≡ `0 <= 0` [eval of deg]

≡ true [simplification], ✓

+ X: 

P(X): `deg (simplify X) <= deg X`

≡ `deg X <= deg X` [eval of simplify]

≡ `1 <= 1` [eval of deg]

≡ `true` [simplification], ✓

### Inductive Case

∀ p1 : polyExpr, ∀ p2 : polyExpr. P(p1) && P(p2) ⇒ P(Add(p1,p2))

#### IH:
`deg (simplify p1) <= deg p1` && `deg (simplify p2) <= deg p2`

Assuming IH, we need to prove P(Add(p1,p2)): `deg (simplify Add(p1,p2)) <= deg Add(p1,p2)`

≡ `deg (simp_add (simplify p1, simplify p2)) <= deg Add(p1,p2)` [eval of simplify]

At this point, we have 3 different cases.

Case 1: For simp_add (simplify p1, simplify p2), (Int 0, p) | (p, Int 0) -> p.

For p1:

≡ `deg (simplify p1) <= deg Add(p1,p2)` [eval of simplify] 

≡ `deg (simplify p1) <= max (deg p1) (deg p2)`

(Since in this case we are given that (simplify p2) is Int 0)

≡ `deg (simplify p1) <= deg p1` [eval of max]

≡ `deg (simplify p1) <= deg p1` [IH], ✓

For p2, proof follows from the symmetry (really, just substitute p1 to p2 for the case above).

Case 2: For simp_add (simplify p1, simplify p2), (Int i1, Int i2) -> Int (i1+i2)

(where p = i1 + i2)

≡ `deg (Int p) <= deg Add(p1,p2)` [eval of simplify]

≡ `0 <= max (deg p1) (deg p2)` [eval of deg]

(Since by definition of deg, it cannot produce an output less then zero, we can simplify)

≡ `true` [simplification]

Case 3: For simp_add (simplify p1, simplify p2), (p1,p2) -> Add(p1,p2)

≡ `deg Add(simplify p1,simplify p2) <= deg Add(p1,p2)` [eval of simp_add]

≡ `max (deg (simplify p1)) (deg (simplify p2)) <= max (deg p1) (deg p2)` [eval of deg]

Subcase 1: When expression evaluates to one of the forms given by IH.

≡ `deg (simplify p1) <= deg p1` or `deg (simplify p2) <= deg p2` [IH], ✓

Subcase 2: Now, we'll show that it can't evaluate to any other cases by using contradiction.

Suppose that `max (deg (simplify p1)) (deg (simplify p2)) <= max (deg p1) (deg p2)` evaluated to

`deg (simplify p1) <= deg p2` or `deg (simplify p2) <= deg p1`. But from IH, it follows that

`deg (simplify p1) <= deg (simplify p2) <= deg p2` or `deg (simplify p2) <= deg (simplify p2) <= deg p1`.

Then, unless `deg (simplify p1) = deg (simplify p2)`, in which case it goes back to subcase 1, it is impossible, because max function returns the largest value.

Hence proved.

Since P(And(p1,p2)) holds for every possible case, it follows that IH holds as well.

### Inductive Case

∀ p1 : polyExpr, ∀ p2 : polyExpr. P(p1) && P(p2) ⇒ P(Mul(p1,p2))

#### IH:
`deg (simplify p1) <= deg p1` && `deg (simplify p2) <= deg p2`

Assuming IH, we need to prove P(Mul(p1,p2)): `deg (simplify Mul(p1,p2)) <= deg Mul(p1,p2)`

≡ `deg (simp_mul (simplify p1, simplify p2)) <= deg Mul(p1,p2)` [eval of simplify]

In order to prove Inductive Case, we need to consider four cases.

Case 1: For simp_mul (simplify p1, simplify p2), (Int 0, _) | (_, Int 0) -> Int 0

≡ `deg (Int 0) <= deg Mul(p1,p2)` [eval of simp_mul]

≡ `0 <= (deg p1) + (deg p2)` [eval pf deg]

(By definition of deg, the values on the right have to be at least equal to 0)

≡ `true` [simplification], ✓

Case 2: For simp_mul (simplify p1, simplify p2), (Int 1, p) | (p, Int 1) -> p

≡ `deg (simplify p1) <= deg Mul(p1,p2)` [eval of simp_mul]

≡ `deg (simplify p1) <= (deg p1) + (deg p2)` [eval of deg]

(Since by IH, it follows that `deg (simplify p1) <= (deg p1)` and by def. of deg, (deg p2) >= 0)

≡ `deg (simplify p1) <= (deg p1) + (deg p2)` [IH], ✓

Proof for p2 follows from the symmetry (just swap p1 to p2 and p2 to p1 in the case above).

Case 3: For simp_mul (simplify p1, simplify p2), (Int i1, Int i2) -> Int (i1 * i2)

≡ `deg (Int(i1*i2)) <= deg Mul(p1,p2)` [eval of simp_mul]

≡ `0 <= (deg p1) + (deg p2)` [eval of deg]

(Since by definition of deg, it follows that any output is at least 0)

≡ `true` [simplification], ✓

Case 4: For simp_mul (simplify p1, simplify p2), (p1, p2) -> Mul(p1,p2)

≡ `deg Mul(simplify p1,simplify p2) <= deg Mul(p1,p2)` [eval of simp_mul]

≡ `(deg (simplify p1)) + (deg (simplify p2)) <= (deg p1) + (deg p2)` [eval of deg]

(From the IH and by the properties of summation, we can conclude that the inequality holds)

≡ `(deg (simplify p1)) + (deg (simplify p2)) <= (deg p1) + (deg p2)` [IH], ✓

Since we were able to prove that P(Mul(p1,p2)) holds for all cases, it implies that IH holds as well. Hence proven.

We were able to show that P(p) holds for every possible value.

Q.E.D.



