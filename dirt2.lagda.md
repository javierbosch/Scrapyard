---
title     : "PUC-Assignment4: PUC Assignment 4"
layout    : page
permalink : /PUC-Assignment4/
---

```agda
module Assignment4 where
```

## YOUR NAME AND EMAIL GOES HERE
# Name
Guilherme Horta Alvares da Silva

# Email
guilhermehas@hotmail.com

## Introduction

You must do _all_ the exercises labelled "(recommended)".

Exercises labelled "(stretch)" are there to provide an extra challenge.
You don't need to do all of these, but should attempt at least a few.

Exercises without a label are optional, and may be done if you want
some extra practice.

Please ensure your files execute correctly under Agda!

**IMPORTANT** For ease of marking, when modifying the given code please write

    -- begin
    -- end

before and after code you add, to indicate your changes.


## Imports

```agda
import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; sym; trans; cong; cong₂; _≢_)
open import Data.Empty using (⊥; ⊥-elim)
open import Data.Nat using (ℕ; zero; suc; _+_; _*_)
open import Data.Product using (_×_; ∃; ∃-syntax) renaming (_,_ to ⟨_,_⟩)
open import Data.String using (String)
open import Data.String.Unsafe using (_≟_)
open import Relation.Nullary using (¬_; Dec; yes; no)
```


## DeBruijn


```agda
module DeBruijn where
```

Remember to indent all code by two spaces.

```agda
  open import plfa.DeBruijn
```


#### Exercise (`mul`) (recommended)

Write out the definition of a lambda term that multiplies
two natural numbers, now adapted to the inherently typed
DeBruijn representation.

```agda
  mul : ∀ {Γ} → Γ ⊢ `ℕ ⇒ `ℕ ⇒ `ℕ
  mul = μ ƛ ƛ (case (# 0) `zero (plus · (# 1) · (# 3 · # 0 · # 1)))
```


#### Exercise `V¬—→`

Following the previous development, show values do
not reduce, and its corollary, terms that reduce are not
values.


#### Exercise `mul-example` (recommended)

Using the evaluator, confirm that two times two is four.

```agda
  2*2 : ∀ {Γ} → Γ ⊢ `ℕ
  2*2 = mul · two · two

  mulᶜ : ∀ {Γ A} → Γ ⊢ Ch A ⇒ Ch A ⇒ Ch A
  mulᶜ = ƛ ƛ ƛ ƛ (# 3 · (# 2 · # 1) · # 0)

  2*2ᶜ : ∀ {Γ} → Γ ⊢ `ℕ
  2*2ᶜ = mulᶜ · twoᶜ · twoᶜ · sucᶜ · `zero

  2*2-eval : Steps 2*2ᶜ
  2*2-eval = eval (gas 100) 2*2ᶜ

  2*2-evalued : 2*2-eval ≡
    steps
    ((ƛ (ƛ (ƛ (ƛ ` (S (S (S Z))) · (` (S (S Z)) · ` (S Z)) · ` Z)))) ·
    (ƛ (ƛ ` (S Z) · (` (S Z) · ` Z)))
    · (ƛ (ƛ ` (S Z) · (` (S Z) · ` Z)))
    · (ƛ `suc ` Z)
    · `zero
    —→⟨ ξ-·₁ (ξ-·₁ (ξ-·₁ (β-ƛ V-ƛ))) ⟩
    (ƛ
      (ƛ
      (ƛ
        (ƛ (ƛ ` (S Z) · (` (S Z) · ` Z))) · (` (S (S Z)) · ` (S Z)) ·
        ` Z)))
    · (ƛ (ƛ ` (S Z) · (` (S Z) · ` Z)))
    · (ƛ `suc ` Z)
    · `zero
    —→⟨ ξ-·₁ (ξ-·₁ (β-ƛ V-ƛ)) ⟩
    (ƛ
      (ƛ
      (ƛ (ƛ ` (S Z) · (` (S Z) · ` Z))) ·
      ((ƛ (ƛ ` (S Z) · (` (S Z) · ` Z))) · ` (S Z))
      · ` Z))
    · (ƛ `suc ` Z)
    · `zero
    —→⟨ ξ-·₁ (β-ƛ V-ƛ) ⟩
    (ƛ
      (ƛ (ƛ ` (S Z) · (` (S Z) · ` Z))) ·
      ((ƛ (ƛ ` (S Z) · (` (S Z) · ` Z))) · (ƛ `suc ` Z))
      · ` Z)
    · `zero
    —→⟨ β-ƛ V-zero ⟩
    (ƛ (ƛ ` (S Z) · (` (S Z) · ` Z))) ·
    ((ƛ (ƛ ` (S Z) · (` (S Z) · ` Z))) · (ƛ `suc ` Z))
    · `zero
    —→⟨ ξ-·₁ (ξ-·₂ V-ƛ (β-ƛ V-ƛ)) ⟩
    (ƛ (ƛ ` (S Z) · (` (S Z) · ` Z))) ·
    (ƛ (ƛ `suc ` Z) · ((ƛ `suc ` Z) · ` Z))
    · `zero
    —→⟨ ξ-·₁ (β-ƛ V-ƛ) ⟩
    (ƛ
      (ƛ (ƛ `suc ` Z) · ((ƛ `suc ` Z) · ` Z)) ·
      ((ƛ (ƛ `suc ` Z) · ((ƛ `suc ` Z) · ` Z)) · ` Z))
    · `zero
    —→⟨ β-ƛ V-zero ⟩
    (ƛ (ƛ `suc ` Z) · ((ƛ `suc ` Z) · ` Z)) ·
    ((ƛ (ƛ `suc ` Z) · ((ƛ `suc ` Z) · ` Z)) · `zero)
    —→⟨ ξ-·₂ V-ƛ (β-ƛ V-zero) ⟩
    (ƛ (ƛ `suc ` Z) · ((ƛ `suc ` Z) · ` Z)) ·
    ((ƛ `suc ` Z) · ((ƛ `suc ` Z) · `zero))
    —→⟨ ξ-·₂ V-ƛ (ξ-·₂ V-ƛ (β-ƛ V-zero)) ⟩
    (ƛ (ƛ `suc ` Z) · ((ƛ `suc ` Z) · ` Z)) ·
    ((ƛ `suc ` Z) · `suc `zero)
    —→⟨ ξ-·₂ V-ƛ (β-ƛ (V-suc V-zero)) ⟩
    (ƛ (ƛ `suc ` Z) · ((ƛ `suc ` Z) · ` Z)) · `suc (`suc `zero) —→⟨
    β-ƛ (V-suc (V-suc V-zero)) ⟩
    (ƛ `suc ` Z) · ((ƛ `suc ` Z) · `suc (`suc `zero)) —→⟨
    ξ-·₂ V-ƛ (β-ƛ (V-suc (V-suc V-zero))) ⟩
    (ƛ `suc ` Z) · `suc (`suc (`suc `zero)) —→⟨
    β-ƛ (V-suc (V-suc (V-suc V-zero))) ⟩
    `suc (`suc (`suc (`suc `zero))) ∎)
    (done (V-suc (V-suc (V-suc (V-suc V-zero)))))
  2*2-evalued = refl
```

## More

```agda
module More where
```

Remember to indent all code by two spaces.


### Syntax

```agda
  infix  4 _⊢_
  infix  4 _∋_
  infixl 5 _,_

  infixr 7 _⇒_
  infixr 8 _`⊎_
  infixr 9 _`×_

  infix  5 ƛ_
  infix  5 μ_
  infixl 7 _·_
  infixl 8 _`*_
  infix  8 `suc_
  infix  9 `_
  infix  9 S_
  infix  9 _`∷_
  infixr 9 #_
```

### Types

```agda
  data Type : Set where
    `ℕ    : Type
    _⇒_   : Type → Type → Type
    Nat   : Type
    _`×_  : Type → Type → Type
    _`⊎_  : Type → Type → Type
```
Code I changed
```agda
    `⊤    : Type
    `⊤²    : Type
    `⊥    : Type
    `List : Type → Type
```

### Contexts

```agda
  data Context : Set where
    ∅   : Context
    _,_ : Context → Type → Context
```

### Variables and the lookup judgment

```agda
  data _∋_ : Context → Type → Set where

    Z : ∀ {Γ A}
        ---------
      → Γ , A ∋ A

    S_ : ∀ {Γ A B}
      → Γ ∋ B
        ---------
      → Γ , A ∋ B
```

### Terms and the typing judgment

```agda
  data _⊢_ : Context → Type → Set where

    -- variables

    `_ : ∀ {Γ A}
      → Γ ∋ A
        -----
      → Γ ⊢ A

```
Code I changed
```agda
    -- Void Type
    `⊥ :  ∀ {Γ}
      → Γ ⊢ `⊥
      ---------------
      → Γ ⊢ `⊥

    -- Unit Type
    `⊤ :  ∀ {Γ}
      ---------
      → Γ ⊢ `⊤

    `⊤² :  ∀ {Γ}
      ---------
      → Γ ⊢ `⊤²

```
```agda
    -- functions

    ƛ_  :  ∀ {Γ A B}
      → Γ , A ⊢ B
        ---------
      → Γ ⊢ A ⇒ B

    _·_ : ∀ {Γ A B}
      → Γ ⊢ A ⇒ B
      → Γ ⊢ A
        ---------
      → Γ ⊢ B

    -- naturals

    `zero : ∀ {Γ}
        ------
      → Γ ⊢ `ℕ

    `suc_ : ∀ {Γ}
      → Γ ⊢ `ℕ
        ------
      → Γ ⊢ `ℕ

    case : ∀ {Γ A}
      → Γ ⊢ `ℕ
      → Γ ⊢ A
      → Γ , `ℕ ⊢ A
        -----
      → Γ ⊢ A

    -- fixpoint

    μ_ : ∀ {Γ A}
      → Γ , A ⊢ A
        ----------
      → Γ ⊢ A

    -- primitive numbers

    con : ∀ {Γ}
      → ℕ
        -------
      → Γ ⊢ Nat

```
Code I changed
```agda
    _`+_ : ∀ {Γ}
      → Γ ⊢ Nat
      → Γ ⊢ Nat
      -------
      → Γ ⊢ Nat
```
```agda

    _`*_ : ∀ {Γ}
      → Γ ⊢ Nat
      → Γ ⊢ Nat
        -------
      → Γ ⊢ Nat

    -- let

    `let : ∀ {Γ A B}
      → Γ ⊢ A
      → Γ , A ⊢ B
        ----------
      → Γ ⊢ B

    -- products

    `⟨_,_⟩ : ∀ {Γ A B}
      → Γ ⊢ A
      → Γ ⊢ B
        -----------
      → Γ ⊢ A `× B

    `proj₁ : ∀ {Γ A B}
      → Γ ⊢ A `× B
        -----------
      → Γ ⊢ A

    `proj₂ : ∀ {Γ A B}
      → Γ ⊢ A `× B
        -----------
      → Γ ⊢ B

    -- alternative formulation of products

    case× : ∀ {Γ A B C}
      → Γ ⊢ A `× B
      → Γ , A , B ⊢ C
        --------------
      → Γ ⊢ C

    `[] : ∀ {Γ A}
      -----------
      → Γ ⊢ `List A

    _`∷_ : ∀ {Γ A}
      → Γ ⊢ A
      → Γ ⊢ `List A
      -------------
      → Γ ⊢ `List A

```

### Abbreviating de Bruijn indices

```agda
  lookup : Context → ℕ → Type
  lookup (Γ , A) zero     =  A
  lookup (Γ , _) (suc n)  =  lookup Γ n
  lookup ∅       _        =  ⊥-elim impossible
    where postulate impossible : ⊥

  count : ∀ {Γ} → (n : ℕ) → Γ ∋ lookup Γ n
  count {Γ , _} zero     =  Z
  count {Γ , _} (suc n)  =  S (count n)
  count {∅}     _        =  ⊥-elim impossible
    where postulate impossible : ⊥

  #_ : ∀ {Γ} → (n : ℕ) → Γ ⊢ lookup Γ n
  # n  =  ` count n
```

## Renaming

```agda
  ext : ∀ {Γ Δ} → (∀ {A} → Γ ∋ A → Δ ∋ A) → (∀ {A B} → Γ , A ∋ B → Δ , A ∋ B)
  ext ρ Z      =  Z
  ext ρ (S x)  =  S (ρ x)

  rename : ∀ {Γ Δ} → (∀ {A} → Γ ∋ A → Δ ∋ A) → (∀ {A} → Γ ⊢ A → Δ ⊢ A)
  rename ρ (` x)          =  ` (ρ x)
```
Code I changed
```agda
  rename ρ (`⊥ x)         =  `⊥ (rename ρ x)
  rename ρ (`⊤)           =  `⊤
  rename ρ (`⊤²)          = `⊤²
  rename ρ (`[])          = `[]
```
```agda
  rename ρ (x `∷ xs)      = rename ρ x `∷ rename ρ xs
  rename ρ (ƛ N)          =  ƛ (rename (ext ρ) N)
  rename ρ (L · M)        =  (rename ρ L) · (rename ρ M)
  rename ρ (`zero)        =  `zero
  rename ρ (`suc M)       =  `suc (rename ρ M)
  rename ρ (case L M N)   =  case (rename ρ L) (rename ρ M) (rename (ext ρ) N)
  rename ρ (μ N)          =  μ (rename (ext ρ) N)
  rename ρ (con n)        =  con n
  rename ρ (M `+ N)       =  rename ρ M `+ rename ρ N
  rename ρ (M `* N)       =  rename ρ M `* rename ρ N
  rename ρ (`let M N)     =  `let (rename ρ M) (rename (ext ρ) N)
  rename ρ `⟨ M , N ⟩     =  `⟨ rename ρ M , rename ρ N ⟩
  rename ρ (`proj₁ L)     =  `proj₁ (rename ρ L)
  rename ρ (`proj₂ L)     =  `proj₂ (rename ρ L)
  rename ρ (case× L M)    =  case× (rename ρ L) (rename (ext (ext ρ)) M)
```

## Simultaneous Substitution

```agda
  exts : ∀ {Γ Δ} → (∀ {A} → Γ ∋ A → Δ ⊢ A) → (∀ {A B} → Γ , A ∋ B → Δ , A ⊢ B)
  exts σ Z      =  ` Z
  exts σ (S x)  =  rename S_ (σ x)

  subst : ∀ {Γ Δ} → (∀ {C} → Γ ∋ C → Δ ⊢ C) → (∀ {C} → Γ ⊢ C → Δ ⊢ C)
  subst σ (` k)          =  σ k
```
Code I changed
```agda
  subst σ (`⊥ N)         =  `⊥ (subst σ N)
  subst σ (`⊤)           =  `⊤
  subst σ (`⊤²)          =  `⊤²
  subst σ (`[])          =  `[]
  subst σ (M `+ N)       =  subst σ M `+ subst σ N
```
```agda
  subst σ (x `∷ xs)      =  subst σ x `∷ subst σ xs
  subst σ (ƛ N)          =  ƛ (subst (exts σ) N)
  subst σ (L · M)        =  (subst σ L) · (subst σ M)
  subst σ (`zero)        =  `zero
  subst σ (`suc M)       =  `suc (subst σ M)
  subst σ (case L M N)   =  case (subst σ L) (subst σ M) (subst (exts σ) N)
  subst σ (μ N)          =  μ (subst (exts σ) N)
  subst σ (con n)        =  con n
  subst σ (M `* N)       =  subst σ M `* subst σ N
  subst σ (`let M N)     =  `let (subst σ M) (subst (exts σ) N)
  subst σ `⟨ M , N ⟩     =  `⟨ subst σ M , subst σ N ⟩
  subst σ (`proj₁ L)     =  `proj₁ (subst σ L)
  subst σ (`proj₂ L)     =  `proj₂ (subst σ L)
  subst σ (case× L M)    =  case× (subst σ L) (subst (exts (exts σ)) M)
```

## Single and double substitution

```agda
  _[_] : ∀ {Γ A B}
    → Γ , A ⊢ B
    → Γ ⊢ A
    ------------
    → Γ ⊢ B
  _[_] {Γ} {A} N V =  subst {Γ , A} {Γ} σ N
    where
    σ : ∀ {B} → Γ , A ∋ B → Γ ⊢ B
    σ Z      =  V
    σ (S x)  =  ` x

  _[_][_] : ∀ {Γ A B C}
    → Γ , A , B ⊢ C
    → Γ ⊢ A
    → Γ ⊢ B
      ---------------
    → Γ ⊢ C
  _[_][_] {Γ} {A} {B} N V W =  subst {Γ , A , B} {Γ} σ N
    where
    σ : ∀ {C} → Γ , A , B ∋ C → Γ ⊢ C
    σ Z          =  W
    σ (S Z)      =  V
    σ (S (S x))  =  ` x
```

## Values

```agda
  data Value : ∀ {Γ A} → Γ ⊢ A → Set where
```
Code I changed
```agda
    V-⊥ : ∀ {Γ} {N : Γ ⊢ `⊥}
      ---------------------------
      → Value {Γ = Γ} (`⊥ N)

    V-⊤ : ∀ {Γ}
      ---------------------------
      → Value (`⊤ {Γ})

    V-⊤² : ∀ {Γ}
      ---------------------------
      → Value (`⊤² {Γ})
```
```agda
    -- functions

    V-ƛ : ∀ {Γ A B} {N : Γ , A ⊢ B}
        ---------------------------
      → Value (ƛ N)

    -- naturals

    V-zero : ∀ {Γ} →
        -----------------
        Value (`zero {Γ})

    V-suc_ : ∀ {Γ} {V : Γ ⊢ `ℕ}
      → Value V
        --------------
      → Value (`suc V)

    -- primitives

    V-con : ∀ {Γ n}
        ---------------------
      → Value {Γ = Γ} (con n)

    -- products

    V-⟨_,_⟩ : ∀ {Γ A B} {V : Γ ⊢ A} {W : Γ ⊢ B}
      → Value V
      → Value W
        ----------------
      → Value `⟨ V , W ⟩

```
Code I changed
```agda
    -- Lists

    V-[] : ∀ {Γ A}
      ---------------------------
      → Value {Γ = Γ} (`[] {A = A})

    V-∷ : ∀ {Γ A} {V : Γ ⊢ A} {W : Γ ⊢ `List A}
      → Value V
      → Value W
      ----------------
      → Value (V `∷ W)

```

Implicit arguments need to be supplied when they are
not fixed by the given arguments.

## Reduction

```agda
  infix 2 _—→_

  data _—→_ : ∀ {Γ A} → (Γ ⊢ A) → (Γ ⊢ A) → Set where

    -- functions

    ξ-·₁ : ∀ {Γ A B} {L L′ : Γ ⊢ A ⇒ B} {M : Γ ⊢ A}
      → L —→ L′
        ---------------
      → L · M —→ L′ · M

    ξ-·₂ : ∀ {Γ A B} {V : Γ ⊢ A ⇒ B} {M M′ : Γ ⊢ A}
      → Value V
      → M —→ M′
        ---------------
      → V · M —→ V · M′

    β-ƛ : ∀ {Γ A B} {N : Γ , A ⊢ B} {V : Γ ⊢ A}
      → Value V
        --------------------
      → (ƛ N) · V —→ N [ V ]

    -- naturals

    ξ-suc : ∀ {Γ} {M M′ : Γ ⊢ `ℕ}
      → M —→ M′
        -----------------
      → `suc M —→ `suc M′

    ξ-case : ∀ {Γ A} {L L′ : Γ ⊢ `ℕ} {M : Γ ⊢ A} {N : Γ , `ℕ ⊢ A}
      → L —→ L′
        -------------------------
      → case L M N —→ case L′ M N

    β-zero :  ∀ {Γ A} {M : Γ ⊢ A} {N : Γ , `ℕ ⊢ A}
        -------------------
      → case `zero M N —→ M

    β-suc : ∀ {Γ A} {V : Γ ⊢ `ℕ} {M : Γ ⊢ A} {N : Γ , `ℕ ⊢ A}
      → Value V
        ----------------------------
      → case (`suc V) M N —→ N [ V ]

    -- fixpoint

    β-μ : ∀ {Γ A} {N : Γ , A ⊢ A}
        ----------------
      → μ N —→ N [ μ N ]

    -- primitive numbers

```
Code I changed
```agda
    ξ-+₁ : ∀ {Γ} {L L′ M : Γ ⊢ Nat}
      → L —→ L′
      -----------------
      → L `+ M —→ L′ `+ M

    ξ-+₂ : ∀ {Γ} {V M M′ : Γ ⊢ Nat}
      → Value V
      → M —→ M′
      -----------------
      → V `+ M —→ V `+ M′

```
```agda
    ξ-*₁ : ∀ {Γ} {L L′ M : Γ ⊢ Nat}
      → L —→ L′
        -----------------
      → L `* M —→ L′ `* M

    ξ-*₂ : ∀ {Γ} {V M M′ : Γ ⊢ Nat}
      → Value V
      → M —→ M′
        -----------------
      → V `* M —→ V `* M′

```
Code I changed
```agda
    ξ-List₁ : ∀ {Γ A} {x x′ : Γ ⊢ A} {xs : Γ ⊢ `List A}
      → x —→ x′
      -----------------
      → x `∷ xs —→ x′ `∷ xs

    ξ-List₂ : ∀ {Γ A} {x : Γ ⊢ A} {xs xs´ : Γ ⊢ `List A}
      → Value x
      → xs —→ xs´
      -----------------
      → x `∷ xs —→ x `∷ xs´

    δ-+ : ∀ {Γ c d}
      -------------------------------------
      → con {Γ = Γ} c `+ con d —→ con (c + d)

```
```agda
    δ-* : ∀ {Γ c d}
        -------------------------------------
      → con {Γ = Γ} c `* con d —→ con (c * d)

    -- let

    ξ-let : ∀ {Γ A B} {M M′ : Γ ⊢ A} {N : Γ , A ⊢ B}
      → M —→ M′
        ---------------------
      → `let M N —→ `let M′ N

    β-let : ∀ {Γ A B} {V : Γ ⊢ A} {N : Γ , A ⊢ B}
      → Value V
        -------------------
      → `let V N —→ N [ V ]

    -- products

    ξ-⟨,⟩₁ : ∀ {Γ A B} {M M′ : Γ ⊢ A} {N : Γ ⊢ B}
      → M —→ M′
        -------------------------
      → `⟨ M , N ⟩ —→ `⟨ M′ , N ⟩

    ξ-⟨,⟩₂ : ∀ {Γ A B} {V : Γ ⊢ A} {N N′ : Γ ⊢ B}
      → Value V
      → N —→ N′
        -------------------------
      → `⟨ V , N ⟩ —→ `⟨ V , N′ ⟩

    ξ-proj₁ : ∀ {Γ A B} {L L′ : Γ ⊢ A `× B}
      → L —→ L′
        ---------------------
      → `proj₁ L —→ `proj₁ L′

    ξ-proj₂ : ∀ {Γ A B} {L L′ : Γ ⊢ A `× B}
      → L —→ L′
        ---------------------
      → `proj₂ L —→ `proj₂ L′

    β-proj₁ : ∀ {Γ A B} {V : Γ ⊢ A} {W : Γ ⊢ B}
      → Value V
      → Value W
        ----------------------
      → `proj₁ `⟨ V , W ⟩ —→ V

    β-proj₂ : ∀ {Γ A B} {V : Γ ⊢ A} {W : Γ ⊢ B}
      → Value V
      → Value W
        ----------------------
      → `proj₂ `⟨ V , W ⟩ —→ W

    -- alternative formulation of products

    ξ-case× : ∀ {Γ A B C} {L L′ : Γ ⊢ A `× B} {M : Γ , A , B ⊢ C}
      → L —→ L′
        -----------------------
      → case× L M —→ case× L′ M

    β-case× : ∀ {Γ A B C} {V : Γ ⊢ A} {W : Γ ⊢ B} {M : Γ , A , B ⊢ C}
      → Value V
      → Value W
        ----------------------------------
      → case× `⟨ V , W ⟩ M —→ M [ V ][ W ]
```

## Reflexive and transitive closure

```agda
  infix  2 _—↠_
  infix  1 begin_
  infixr 2 _—→⟨_⟩_
  infix  3 _∎

  data _—↠_ : ∀ {Γ A} → (Γ ⊢ A) → (Γ ⊢ A) → Set where

    _∎ : ∀ {Γ A} (M : Γ ⊢ A)
        --------
      → M —↠ M

    _—→⟨_⟩_ : ∀ {Γ A} (L : Γ ⊢ A) {M N : Γ ⊢ A}
      → L —→ M
      → M —↠ N
        ------
      → L —↠ N

  begin_ : ∀ {Γ} {A} {M N : Γ ⊢ A}
    → M —↠ N
      ------
    → M —↠ N
  begin M—↠N = M—↠N
```


## Values do not reduce

```agda
  V¬—→ : ∀ {Γ A} {M N : Γ ⊢ A}
    → Value M
      ----------
    → ¬ (M —→ N)
  V¬—→ V-ƛ           ()
```
Code I changed
```agda
  V¬—→ (V-⊥)         ()
  V¬—→ V-⊤           ()
  V¬—→ V-⊤²          ()
```
```agda
  V¬—→ V-zero        ()
  V¬—→ (V-suc VM)    (ξ-suc M—→M′)     =  V¬—→ VM M—→M′
  V¬—→ V-con         ()
  V¬—→ V-⟨ VM , _ ⟩  (ξ-⟨,⟩₁ M—→M′)    =  V¬—→ VM M—→M′
  V¬—→ V-⟨ _ , VN ⟩  (ξ-⟨,⟩₂ _ N—→N′)  =  V¬—→ VN N—→N′
```
```agda
  V¬—→ V-[]          ()
  V¬—→ (V-∷ V W) (ξ-List₁ V→x) = V¬—→ V V→x
  V¬—→ (V-∷ V W) (ξ-List₂ VV W→xs) = V¬—→ W W→xs
```


## Progress

```agda
  data Progress {A} (M : ∅ ⊢ A) : Set where

    step : ∀ {N : ∅ ⊢ A}
      → M —→ N
        ----------
      → Progress M

    done :
        Value M
        ----------
      → Progress M

  progress : ∀ {A}
    → (M : ∅ ⊢ A)
      -----------
    → Progress M
  progress (` ())
  progress (ƛ N)                              =  done V-ƛ
```
Code I changed
```agda
  progress (`⊥ N)                             =  done V-⊥
  progress (`⊤)                               =  done V-⊤
  progress (`⊤²)                              =  done V-⊤²
  progress (`[])                              =  done V-[]
  progress (x `∷ xs) with progress x
  progress (x `∷ xs) | step x→N = step (ξ-List₁ x→N)
  progress (x `∷ xs) | done Vx with progress xs
  progress (x `∷ xs) | done Vx | step xs→N = step ( ξ-List₂ Vx xs→N)
  progress (x `∷ xs) | done Vx | done Vxs = done (V-∷ Vx Vxs)
```
```agda
  progress (L · M) with progress L
  ...    | step L—→L′                         =  step (ξ-·₁ L—→L′)
  ...    | done V-ƛ with progress M
  ...        | step M—→M′                     =  step (ξ-·₂ V-ƛ M—→M′)
  ...        | done VM                        =  step (β-ƛ VM)
  progress (`zero)                            =  done V-zero
  progress (`suc M) with progress M
  ...    | step M—→M′                         =  step (ξ-suc M—→M′)
  ...    | done VM                            =  done (V-suc VM)
  progress (case L M N) with progress L
  ...    | step L—→L′                         =  step (ξ-case L—→L′)
  ...    | done V-zero                        =  step β-zero
  ...    | done (V-suc VL)                    =  step (β-suc VL)
  progress (μ N)                              =  step β-μ
  progress (con n)                            =  done V-con
  progress (L `+ M) with progress L
  ...    | step L—→L′                         =  step (ξ-+₁ L—→L′)
  ...    | done V-con with progress M
  ...        | step M—→M′                     =  step (ξ-+₂ V-con M—→M′)
  ...        | done V-con                     =  step δ-+
  progress (L `* M) with progress L
  ...    | step L—→L′                         =  step (ξ-*₁ L—→L′)
  ...    | done V-con with progress M
  ...        | step M—→M′                     =  step (ξ-*₂ V-con M—→M′)
  ...        | done V-con                     =  step δ-*
  progress (`let M N) with progress M
  ...    | step M—→M′                         =  step (ξ-let M—→M′)
  ...    | done VM                            =  step (β-let VM)
  progress `⟨ M , N ⟩ with progress M
  ...    | step M—→M′                         =  step (ξ-⟨,⟩₁ M—→M′)
  ...    | done VM with progress N
  ...        | step N—→N′                     =  step (ξ-⟨,⟩₂ VM N—→N′)
  ...        | done VN                        =  done (V-⟨ VM , VN ⟩)
  progress (`proj₁ L) with progress L
  ...    | step L—→L′                         =  step (ξ-proj₁ L—→L′)
  ...    | done (V-⟨ VM , VN ⟩)               =  step (β-proj₁ VM VN)
  progress (`proj₂ L) with progress L
  ...    | step L—→L′                         =  step (ξ-proj₂ L—→L′)
  ...    | done (V-⟨ VM , VN ⟩)               =  step (β-proj₂ VM VN)
  progress (case× L M) with progress L
  ...    | step L—→L′                         =  step (ξ-case× L—→L′)
  ...    | done (V-⟨ VM , VN ⟩)               =  step (β-case× VM VN)
```


## Evaluation

```agda
  data Gas : Set where
    gas : ℕ → Gas

  data Finished {Γ A} (N : Γ ⊢ A) : Set where

     done :
         Value N
         ----------
       → Finished N

     out-of-gas :
         ----------
         Finished N

  data Steps : ∀ {A} → ∅ ⊢ A → Set where

    steps : ∀ {A} {L N : ∅ ⊢ A}
      → L —↠ N
      → Finished N
        ----------
      → Steps L

  eval : ∀ {A}
    → Gas
    → (L : ∅ ⊢ A)
      -----------
    → Steps L
  eval (gas zero)    L                     =  steps (L ∎) out-of-gas
  eval (gas (suc m)) L with progress L
  ... | done VL                            =  steps (L ∎) (done VL)
  ... | step {M} L—→M with eval (gas m) M
  ...    | steps M—↠N fin                  =  steps (L —→⟨ L—→M ⟩ M—↠N) fin
```

## Examples

```agda
  cube : ∅ ⊢ Nat ⇒ Nat
  cube = ƛ (# 0 `* # 0 `* # 0)

  _ : cube · con 2 —↠ con 8
  _ = 
    begin
      cube · con 2
    —→⟨ β-ƛ V-con ⟩
      con 2 `* con 2 `* con 2
    —→⟨ ξ-*₁ δ-* ⟩
      con 4 `* con 2
    —→⟨ δ-* ⟩
      con 8
    ∎

  exp10 : ∅ ⊢ Nat ⇒ Nat
  exp10 = ƛ (`let (# 0 `* # 0)
              (`let (# 0 `* # 0)
                (`let (# 0 `* # 2)
                  (# 0 `* # 0))))

  _ : exp10 · con 2 —↠ con 1024
  _ =
    begin
      exp10 · con 2
    —→⟨ β-ƛ V-con ⟩
      `let (con 2 `* con 2) (`let (# 0 `* # 0) (`let (# 0 `* con 2) (# 0 `* # 0)))
    —→⟨ ξ-let δ-* ⟩
      `let (con 4) (`let (# 0 `* # 0) (`let (# 0 `* con 2) (# 0 `* # 0)))
    —→⟨ β-let V-con ⟩
      `let (con 4 `* con 4) (`let (# 0 `* con 2) (# 0 `* # 0))
    —→⟨ ξ-let δ-* ⟩
      `let (con 16) (`let (# 0 `* con 2) (# 0 `* # 0))
    —→⟨ β-let V-con ⟩
      `let (con 16 `* con 2) (# 0 `* # 0)
    —→⟨ ξ-let δ-* ⟩
      `let (con 32) (# 0 `* # 0)
    —→⟨ β-let V-con ⟩
      con 32 `* con 32
    —→⟨ δ-* ⟩
      con 1024
    ∎

  swap× : ∀ {A B} → ∅ ⊢ A `× B ⇒ B `× A
  swap× = ƛ `⟨ `proj₂ (# 0) , `proj₁ (# 0) ⟩

  _ : swap× · `⟨ con 42 , `zero ⟩ —↠ `⟨ `zero , con 42 ⟩
  _ =
    begin
      swap× · `⟨ con 42 , `zero ⟩
    —→⟨ β-ƛ V-⟨ V-con , V-zero ⟩ ⟩
      `⟨ `proj₂ `⟨ con 42 , `zero ⟩ , `proj₁ `⟨ con 42 , `zero ⟩ ⟩
    —→⟨ ξ-⟨,⟩₁ (β-proj₂ V-con V-zero) ⟩
      `⟨ `zero , `proj₁ `⟨ con 42 , `zero ⟩ ⟩
    —→⟨ ξ-⟨,⟩₂ V-zero (β-proj₁ V-con V-zero) ⟩
      `⟨ `zero , con 42 ⟩
    ∎

  swap×-case : ∀ {A B} → ∅ ⊢ A `× B ⇒ B `× A
  swap×-case = ƛ case× (# 0) `⟨ # 0 , # 1 ⟩

  _ : swap×-case · `⟨ con 42 , `zero ⟩ —↠ `⟨ `zero , con 42 ⟩
  _ =
    begin
       swap×-case · `⟨ con 42 , `zero ⟩
     —→⟨ β-ƛ V-⟨ V-con , V-zero ⟩ ⟩
       case× `⟨ con 42 , `zero ⟩ `⟨ # 0 , # 1 ⟩
     —→⟨ β-case× V-con V-zero ⟩
       `⟨ `zero , con 42 ⟩
     ∎
```


#### Exercise `More` (recommended in part)

Formalise the remaining constructs defined in this chapter.
Evaluate each example, applied to data as needed,
to confirm it returns the expected answer.

  * sums (recommended)
  * unit type
  * an alternative formulation of unit type
  * empty type (recommended)
  * lists


