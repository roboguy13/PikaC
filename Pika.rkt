#lang racket
(require redex)

(define-language Base-Pika
  (bool ::= True False)
  (e ::= integer bool x (λ (x : τ) e))
  (A X x α a Ctr ::= variable-not-otherwise-mentioned)
  (τ ::= Bool Int α (τ → τ) ((α ~ (Layout X)) ⇒ τ) (SSL T))
  (Δ ::= [] ((x type) ...))
  (Γ ::= [] ((x : τ) ...))
  (C ::= [] ((a ~ (Layout X)) ...))

  (layout-constraint ::= (a ~ (Layout X)))

  (layout-defs ::= (layout-def ...))
  (layout-def ::= ((A : (Layout X) [x ...]) layout-case ...))
  (layout-case ::= (pat := (h ...)))
  (pat ::= (Ctr x ...))
  (h ::= (loc ↦ e))
  #;(E ::= [] ((A ~ (Layout X)) E))
  (T ::= ((loc : τ) ...))
  (loc ::= x (x + number))
  #:binding-forms
  (λ (x : τ) e #:refers-to x))

(define-extended-language Pika
  Base-Pika
  (e ::= .... (e e) (apply α e) (Λ (α ~ (Layout X)) e))
  #:binding-forms
  (Λ (α ~ (Layout X)) e #:refers-to α)
  ((α ~ (Layout X)) ⇒ τ #:refers-to α))

(define-extended-language Pika-Core
  Base-Pika
  (e ::= .... (layout-arg x ...) (e arg ...) (layout (x ...) T (h ...))
         (with ((x ...) := e) e))
  (arg ::= (x ...) (integer) (bool))
  #:binding-forms
  (layout (x ...) ((l : τ) ...) (h ...) #:refers-to (shadow x ...))
  (with ((x ...) := e) e #:refers-to (shadow x ...)))

(define-union-language Full-Pika Pika Pika-Core)

(define-extended-language Full-Pika-Ctx
  Full-Pika
  (v ::= integer bool x (layout (x ...) T ((loc ↦ v) ...))
     (λ (x) v) (with ((x ...) := v) v))

  (E ::= (apply α E)
         (E e)
         (v E)
         (with ((x ...) := E) e)
         (with ((x ...) := v) E)
         (layout (x ...) T ((loc ↦ v) ... (loc ↦ E) (loc ↦ e) ...))
         (Λ (x ~ (Layout X)) E)
         (λ (x : τ) E)
         hole))

(define-judgment-form Base-Pika
  #:mode (is-type I I)
  #:contract (is-type Δ τ)

  [------
   (is-type ((τ type) (τ_2 type) ...) τ)]

  [(is-type ((τ_3 type) ...) τ)
   ------
   (is-type ((τ_2 type) (τ_3 type) ...) τ)])

#;(def-is-type Base-Pika)

(define-judgment-form Base-Pika
  #:mode (lookup-type I I O)
  #:contract (lookup-type Γ x τ)
  [
   -------
   (lookup-type ((x : τ) (x_2 : τ_2) ...) x τ)]

  [(lookup-type ((x_3 : τ_3) ...) x τ)
   -------
   (lookup-type ((x_2 : τ_2) (x_3 : τ_3) ...) x τ)])

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
   (lookup-constraint ((α ~ (Layout X)) (α_2 ~ (Layout X_2)) ...) α X)]

  [(lookup-constraint ((α_3 ~ (Layout X_3)) ...) α X)
   ------
   (lookup-constraint ((α_2 ~ (Layout X_2)) (α_3 ~ (Layout X_3)) ...) α X)])

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

  [(type-wf-base ((α type) . Δ) τ)
   -------
   (type-wf-base Δ ((α ~ (Layout X)) ⇒ τ))
   ])

(define-judgment-form Base-Pika
  #:mode (get-layout-def-type I I O O)
  #:contract (get-layout-def-type layout-defs A layout-constraint [x ...])

  [
   -----
   (get-layout-def-type (((A : (Layout X) [x ...]) layout-case ...) layout-def ...) A (A ~ (Layout X)) [x ...])]

  [(get-layout-def-type (layout-def ...) A layout-constraint [x_2 ...])
   -----
   (get-layout-def-type (((B : (Layout X) [x ...]) layout-case ...) layout-def ...) A layout-constraint [x_2 ...])])

(define-judgment-form Pika
  #:mode (has-type I I I O)
  #:contract (has-type Γ C e τ)

  [
   ------ ;[T-Int]
   (has-type Γ C integer Int)]

  [
   ------ ;[T-Bool]
   (has-type Γ C bool Bool)]

  [(lookup-type Γ x τ)
   ------ ;[T-Var]
   (has-type Γ C x τ)]

  [(has-type ((x : τ_1) . Γ) C e τ_2)
   ------ ;[T-Lambda]
   (has-type Γ C (λ (x : τ_1) e) (τ_1 → τ_2))]
  
  [(has-type Γ C e_1 (τ_1 → τ_2))
   (has-type Γ C e_2 τ_1)
   ------ ;[T-App]
   (has-type Γ C (e_1 e_2) τ_2)]

  [(has-type Γ ((α ~ (Layout X)) C) e τ)
   ----- ;[T-Layout-Lambda]
   (has-type Γ C (Λ (α ~ (Layout X)) e) ((α ~ (Layout X)) ⇒ τ))
   ]

  [(lookup-constraint C α_2 X_2)
   (has-type Γ C e #;(substitute e α_2 α) ((α ~ (Layout X)) ⇒ τ))
   ----- ;[T-Layout-App]
   (has-type Γ C (apply α_2 e) (substitute τ α α_2))]
  )

(define-judgment-form Pika-Core
  #:mode (get-T-vars I O)
  #:contract (get-T-vars T Γ)

  [(get-T-vars ((x_2 : τ_2) ...) Γ)
   -----
   (get-T-vars ((x : τ) (x_2 : τ_2) ...) ((x : τ) . Γ))]


  [(get-T-vars ((x_2 : τ_2) ...) Γ)
   -----
   (get-T-vars (((x + n) : τ) (x_2 : τ_2) ...) Γ)]

  [-----
   (get-T-vars [] [])])

(define-extended-judgment-form Full-Pika has-type #;Pika-Core
  #:mode (has-type-pc I I I O)
  #:contract (has-type-pc Γ C e τ)

;  [
;   ------ ;[T-Int]
;   (has-type-pc Γ C integer Int)]
;
;  [
;   ------ ;[T-Bool]
;   (has-type-pc Γ C bool Bool)]
;
;  [(lookup-type Γ x τ)
;   ------ ;[T-Var]
;   (has-type-pc Γ C x τ)]
;
;  [(has-type-pc ((x : τ_1) Γ) C e τ_2)
;   ------ ;[T-Lambda]
;   (has-type-pc Γ C (λ (x : τ_1) e) (τ_1 → τ_2))]
;  
;  [(has-type-pc Γ C e_1 (τ_1 → τ_2))
;   (has-type-pc Γ C e_2 τ_1)
;   ------ ;[T-App]
;   (has-type-pc Γ C (e_1 e_2) τ_2)]

  [(has-type-pc Γ C e τ_2) ...
   ------ ;[T-Layout]
   (has-type-pc Γ C (layout (x ...) ((l ↦ e) ...)) (SSL ((l : τ_2) ...)))
   ]

  [(has-type-pc Γ C e_1 (SSL T))
   (get-T-vars T ((x_3 : τ_3) ...))
   (where Γ_2 ((x_3 : τ_3) ...))
   (has-type-pc ,(append (term (substitute Γ_2 (x_3 x) ...)) (term Γ))
                C
                e_2 τ_2)
   ------ ;[T-With]
   (has-type-pc Γ C (with ((x ...) := e_1) e_2) τ_2)])


(define (-->PC defs Γ C)
  (reduction-relation
   Full-Pika-Ctx
   #:domain e

   (--> (in-hole E (e_1 (apply α e_2)))
        (in-hole E (with ((x ...) := e_3) (e_1 (layout-arg x ...))))
        
        (judgment-holds (has-type-pc ,Γ ,C (apply α e_2) τ))
        (judgment-holds (get-layout-def-type ,defs α layout-constraint [x ...]))
        "PC-With")

   (--> (apply α e)
        e
        "PC-Apply")))


;;; Tests

#;(layout-def ::= ((A : (Layout X) [x ...]) layout-case ...))
#;(layout-case ::= (pat := (h ...)))
(define Sll
  (term
   ((Sll : (Layout List) [x])
    ((Nil) := [])
    ((Cons head tail) := [(x ↦ head) ((x + 1) ↦ tail)]))))

(define globals `[,Sll])

#;(traces (-->PC globals (term [(f : ((a ~ (Layout List)) ⇒ (a → a))) (x : ((b ~ (Layout List)) ⇒ b))]) (term [(Sll ~ (Layout List))])) (term ((apply Sll f) (apply Sll x))))

(define test-fn-type
  (term
   ((α ~ (Layout A)) ⇒ (α → α))))

(judgment-holds
 (has-type-pc
  [(y : (SSL [(z : Int)]))]
  []
  (with ((x) := y)
        x)
  τ)
 τ)

(judgment-holds
  (has-type [] [] (λ (x : Int) x) τ) τ)

(judgment-holds
 (has-type
  [(x : Int) (left : Int)
             (Cons : ((α_1 ~ (Layout A)) ⇒ (Int → (α_1 → α_1))))
             (leftList : ((α_2 ~ (Layout A)) ⇒ α_2))]
  [(α ~ (Layout A))]
  (((apply α Cons) x) (apply α leftList))
  τ)
 τ)

#;(judgment-holds
 (has-type
  [(f : ((α_2 ~ (Layout A)) ⇒ (α_2 → α_2))) [(x : ((α_3 ~ (Layout A)) ⇒ α_3)) []]]
  [(α ~ (Layout A)) []]
  ((apply α f) (apply α x))
  τ)
 τ)
