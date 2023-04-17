#lang racket
(require redex)

(define-language Base-Pika
  (bool ::= True False)
  (e ::= integer bool (λ x e))
  (A X x α a ::= variable-not-otherwise-mentioned)
  (τ ::= Bool Int α (τ → τ))
  (Δ ::= [] ((x type) Δ))
  (Γ ::= [] ((x : τ) Γ))
  (E C ::= [] ((a ~ (Layout X)) C))
  #;(E ::= [] ((A ~ (Layout X)) E)))

(define-extended-language Pika
  Base-Pika
  (e ::= .... (apply α e) (Λ (α ~ (Layout X)) e))
  (τ ::= .... (e e) ((α ~ (Layout X)) ⇒ τ)))

(define-extended-language Pika-Core
  Base-Pika
  (e ::= .... (e a ...) (layout (x ...) ((l : τ) ...) (h ...))
         (with ((x ...) := e) e))
  (a ::= (x ...) (integer) (bool))
  (h ::= (loc ↦ e))
  (loc ::= (x + number)))

(define-judgment-form Base-Pika
  #:mode (is-type I I)
  #:contract (is-type Δ τ)

  [------
   (is-type ((τ type) Δ) τ)]

  [(is-type Δ τ)
   ------
   (is-type ((τ_2 type) Δ) τ)])

(define-judgment-form Base-Pika
  #:mode (lookup-type I I O)
  #:contract (lookup-type Γ x τ)
  [
   -------
   (lookup-type ((x : τ) Γ) x τ)]

  [(lookup-type Γ x τ)
   -------
   (lookup-type ((x_2 : τ_2) Γ) x τ)])

(define-judgment-form Base-Pika
  #:mode (lookup-constraint I I O)
  #:contract (lookup-constraint C α X)

  [
   ------
   (lookup-constraint C Int Int)
   ]

  [
   -----
   (lookup-constraint C Bool Bool)]

  [
   ------
   (lookup-constraint ((α ~ (Layout X)) C) α X)]

  [(lookup-constraint C α X)
   ------
   (lookup-constraint ((α_2 ~ (Layout X_2)) C) α X)])

(define-judgment-form Base-Pika
  #:mode (type-wf-base I I)
  #:contract (type-wf-base Δ τ)

  [(is-type Δ α)
   -------
   (type-wf-base Δ α)]

  [
   -------
   (type-wf-base Δ Int)]

  [
   -------
   (type-wf-base Δ Bool)]

  [(type-wf-base Δ τ_1)
   (type-wf-base Δ τ_2)
   -------
   (type-wf-base Δ (τ_1 → τ_2))]

  [(type-wf-base ((α type) Δ) τ)
   -------
   (type-wf-base Δ ((α ~ (Layout X)) ⇒ τ))
   ])

(define-judgment-form Pika
  #:mode (has-type I I I O)
  #:contract (has-type Γ C e τ)

  [
   ------
   (has-type Γ C integer Int)]

  [
   ------
   (has-type Γ C bool Bool)]

  [(lookup-type Γ x τ)
   ------
   (has-type Γ C x τ)])
