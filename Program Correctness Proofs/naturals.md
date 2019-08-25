### ``exp``

### Property

P(n): ∀ x : float. `exp x n` ≡ x <sup>`n`</sup>

### Base Case

P(0): ∀ x : float. `exp x 0` ≡ x <sup>`0`</sup>

≡ `exp x 0` ≡ 1.0 [property of exponent]

≡ 1.0 ≡ 1.0 [eval of exp] , ✓

### Inductive Case

∀ n : ℕ, ∀ x : float. P(n) ⇒ P(n+1)

#### IH:
∀ x : float. `exp x n` ≡ x <sup>`n`</sup>

Assuming IH, we need to prove P(n+1): `exp x (n+1)` ≡ x <sup>`n+1`</sup>

≡ `exp x (n+1)` ≡ x *. x <sup>`n`</sup> [property of exponent]

≡ x *. `exp x ((n+1)-1)` ≡ x *. x <sup>`n`</sup> [eval of exp]

≡ `exp x n` ≡ x <sup>`n`</sup> [dividing both sides by x and simplification]

(Note that for division by x, we are allowed to assume that x != 0, since otherwise the expression would take form 0 ≡ 0, which will hold true without division by x)

≡ `exp x n` ≡ x <sup>`n`</sup>  [IH], ✓

Since we assumed that IH holds, it follows that Inductive Case holds as well, since we were able to reduce IC to IH. Thus IH is proved.

Q.E.D.

### ``exp_nat``

### Property

P(n): ∀ x : float. `exp x (to_int n)` ≡ `exp_nat x n`

### Base Case

P(Zero): ∀ x : float. `exp x (to_int Zero)` ≡ `exp_nat x Zero`

≡ `exp x (to_int Zero)` ≡ 1.0 [eval pf exp_nat]

≡ `exp x 0` ≡ 1.0 [eval of to_int]

≡ 1.0 ≡ 1.0 [eval of exp], ✓

### Inductive Case

∀ n : nat, ∀ x : float. P(n) ⇒ P(Succ n)

#### IH:
∀ x : float. `exp x (to_int n)` ≡ `exp_nat x n`

Assuming IH, we need to prove P(Succ n): `exp x (to_int Succ n)` ≡ `exp_nat x Succ n`

≡ `exp x (1 + (to_int n))` ≡ `exp_nat x Succ n` [eval of to_int]

≡ `exp x (1 + (to_int n))` ≡ x *. `exp_nat x n` [eval of exp_nat]

≡ x *. `exp x (1 + (to_int n) - 1)` ≡ x *. `exp_nat x n` [eval of exp]

≡ `exp x (to_int n)` ≡ `exp_nat x n` [division of both sides by x and simplification]

(Again, in case of x = 0, it reduces to 0 ≡ 0 and division is not needed)

≡ `exp x (to_int n)` ≡ `exp_nat x n` [IH], ✓

Since we were able to reduce Inductive Case to IH, it follows that IC holds and so IH is proven.

Q.E.D.

### ``geq_nat``

### Property

P(n): ∀ m : nat. `geq_nat m n` ≡ `(to_int m) >= (to_int n)`

### Base Case

P(Zero): ∀ m : nat. `geq_nat m Zero` ≡ `(to_int m) >= (to_int Zero)`

≡ `geq_nat m Zero` ≡ `(to_int m) >= 0` [eval of to_int]

≡ true ≡ `(to_int m) >= 0` [eval of geq_nat]

≡ true ≡ true [by definition of to int, always at least 0], ✓

### Inductive Case

∀ n : nat, ∀ m : nat. P(n) ⇒ P(Succ n)

#### IH:
∀ m : nat. `geq_nat m n ≡ (to_int m) >= (to_int n)`

Assuming IH, we need to prove P(Succ n): `geq_nat m Succ n ≡ (to_int m) >= (to_int Succ n)`

≡ `geq_nat m Succ n ≡ (to_int m) >= 1 + (to_int n)` [eval of to_int]

When evaluating Succ n, we have two different cases. Let's consider them both.

Case 1: For Succ n, n = m 

≡ `false ≡ (to_int m) >= 1 + (to_int n)` [eval of geq_nat]

≡ `false ≡ false` [simplification], ✓

(Since m = n, to_int should have produced the same value i. Obviously, i >= i+1 is a false statement)

Case 2: For Succ n, m > n

≡ `geq_nat m n ≡ (to_int m) >= 1 + (to_int n)` [eval of geq_nat]

≡ `geq_nat m n ≡ (to_int m) > (to_int n)` [by properties of inequality sign]

(Since for this case we consider m > n, we still would be able to apply IH. For m <= n, we have case 1)

≡ `geq_nat m n ≡ (to_int m) > (to_int n)` [IH], ✓

Since Inductive Step holds true for both cases, it follows that IH holds as well. Thus proven.

Q.E.D.
