#lang racket
(require redex)

(define-language Base-Pika
  (bool ::= True False)
  (e ::= integer bool x (λ (x : τ) e))
  (A X x α a Ctr ::= variable-not-otherwise-mentioned)
  (xs ::= (x ...))
  (τ ::= Bool Int α (τ → τ) ((α ~ (Layout X)) ⇒ τ) (SSL T))
  (Δ ::= [] ((x type) ...))
  (Γ ::= [] ((x : τ) ...))
  (C ::= [] ((a ~ (Layout X)) ...))

  (data-def ::= (data X := ctr-def ...))

  (layout-constraint ::= (a ~ (Layout X)))

  (ctr-def ::= (Ctr τ ...))

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
  (top-level-def ::= ((x : (layout-constraint ...) ⇒ τ) top-level-case ...))
  (top-level-case ::= ((pat) := e))
  #:binding-forms
  (Λ (α ~ (Layout X)) e #:refers-to α)
  ((α ~ (Layout X)) ⇒ τ #:refers-to α)
  ((Ctr x ...) := e #:refers-to (shadow x ...))
  (x : ((α ~ (Layout X)) ...) ⇒ τ #:refers-to (shadow α ...)))

(define-extended-language Pika-Core
  Base-Pika
  (e ::= .... (layout-arg x ...) (e arg ...) (layout (x ...) T (h ...))
         (with ((x ...) := e) e))
  (arg ::= (x ...) (integer) (bool))
  (ρ ::= [(x (x ...)) ...])
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



#;(layout-def ::= ((A : (Layout X) [x ...]) layout-case ...))
#;(layout-case ::= (pat := (h ...)))

(define-judgment-form Base-Pika
  #:mode (get-layout I I O)
  #:contract (get-layout layout-defs A (layout-case ...))

  [
   -----
   (get-layout (((A : (Layout α) [x ...]) layout-case ...) layout-def ...) A (layout-case ...))]

  [(get-layout  (layout-def ...) A (layout-case_2 ...))
   (where #f ,(equal? (term A) (term A_2)))
   -----
   (get-layout  (((A_2 : (Layout α) [x ...]) layout-case ...) layout-def ...) A (layout-case_2 ...))])

(define-judgment-form Base-Pika
  #:mode (get-layout-params I I O)
  #:contract (get-layout-params layout-defs A [x ...])

  [
   -----
   (get-layout-params (((A : (Layout α) [x ...]) layout-case ...) layout-def ...) A [x ...])]

  [(get-layout-params (layout-def ...) A (x_2 ...))
   (where #f ,(equal? (term A) (term A_2)))
   -----
   (get-layout-params (((A_2 : (Layout α) [x ...]) layout-case ...) layout-def ...) A [x_2 ...])])

(define-judgment-form Base-Pika
  #:mode (get-layout-branch-from-def I I O O)
  #:contract (get-layout-branch-from-def (layout-case ...) Ctr [x ...] [h ...])

  [
   -----
   (get-layout-branch-from-def (((Ctr x ...) := (h ...)) layout-case ...) Ctr [x ...] [h ...])]

  [(get-layout-branch-from-def (layout-case ...) Ctr [x ...] [h ...])
   (where #f ,(equal? (term Ctr_2) (term Ctr)))
   -----
   (get-layout-branch-from-def (((Ctr_2 x_2 ...) := (h_2 ...)) layout-case ...) Ctr [x ...] [h ...])]
  )

(define-judgment-form Base-Pika
  #:mode (get-layout-branch I I I O O)
  #:contract (get-layout-branch layout-defs A Ctr [x ...] [h ...])

  [(get-layout layout-defs A (layout-case ...))
   (get-layout-branch-from-def (layout-case ...) Ctr [x_2 ...] [h ...])
   -----
   (get-layout-branch layout-defs A Ctr [x_2 ...] [h ...])])

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

(define-judgment-form Pika
  #:mode (split-app I O O)
  #:contract (split-app e x [e ...])

  [
   ------
   (split-app x x [])]

  [
   ------
   (split-app (x e) x [e])]

  [(split-app e_1 x [e ...])
   ------
   (split-app (e_1 e_2) x (e ... e_2))])

(define-judgment-form Pika-Core
  #:mode (get-loc-types I I I O)
  #:contract (get-loc-types Γ C [h ...] T)

  [
   -----
   (get-loc-types Γ C [] [])]

  [(get-loc-types Γ C (h ...) T)
   (has-type-pc Γ C e τ)
   -----
   (get-loc-types Γ C ((loc ↦ e) h ...) ((loc : τ) . T))])

(define-judgment-form Base-Pika
  #:mode (lookup-ctr I I O)
  #:contract (lookup-ctr (ctr-def ...) Ctr (τ ...))

  [
   -----
   (lookup-ctr ((Ctr τ ...) ctr-def ...) Ctr (τ ...))]

  [(lookup-ctr (ctr-def_2 ...) Ctr (τ ...))
   (where #f ,(equal? (term Ctr_2) (term Ctr)))
   -----
   (lookup-ctr ((Ctr_2 τ_2 ...) ctr-def_2 ...) Ctr (τ ...))])

(define-judgment-form Base-Pika
  #:mode (data-name I O)
  #:contract (data-name data-def X)

  [
   -----
   (data-name (data X := ctr-def ...) X)])

(define-judgment-form Base-Pika
  #:mode (get-ctr-type I I O)
  #:contract (get-ctr-type layout-constraint (τ ...) τ)

  [
   -----
   (get-ctr-type (α ~ (Layout X)) [] α)]

  [(where #f ,(equal? (term X) (term τ_2)))
   (get-ctr-type (α ~ (Layout X)) (τ_3 ...) τ)
   -----
   (get-ctr-type (α ~ (Layout X)) (τ_2 τ_3 ...) (τ_2 → τ))]

  [(get-ctr-type (α ~ (Layout X)) (τ_2 ...) τ)
   -----
   (get-ctr-type (α ~ (Layout X)) (X τ_2 ...) (α → τ))])

(define-judgment-form Base-Pika
  #:mode (get-data-type I I I O)
  #:contract (get-data-type layout-constraint data-def Ctr τ)

  [(lookup-ctr (ctr-def ...) Ctr (τ_2 ...))
   #;(data-name (data X := ctr-def ...) X)
   (get-ctr-type layout-constraint (τ_2 ...) τ)
   -----
   (get-data-type layout-constraint (data X := ctr-def ...) Ctr τ)])

(define-judgment-form Base-Pika
  #:mode (lookup-data-def I I O)
  #:contract (lookup-data-def (data-def ...) X data-def)

  [
   -----
   (lookup-data-def ((data X := ctr-def ...) data-def ...) X (data X := ctr-def ...))]

  [(lookup-data-def (data-def_2 ...) X data-def)
   (where #f ,(equal? (term X) (term X_2)))
   -----
   (lookup-data-def ((data X_2 := ctr-def ...) data-def_2 ...) X data-def)])



;  [(lookup-ctr data-def Ctr (X_2 a ...))
;   (data-name data-def X)
;   (where #f ,(equal? (term X) (term X_2)))
;   (get-data-type (α ~ (Layout X)) (data X := 
;   -----
;   (get-data-type (α ~ (Layout X)) data-def Ctr (X_2 → τ))])

(define-judgment-form Full-Pika
  #:mode (lookup-renaming I I O)
  #:contract (lookup-renaming ρ x [x ...])

  [
   ----
   (lookup-renaming ((x (x_2 ...)) (x_3 (x_4 ...)) ...) x (x_2 ...))]

  [(lookup-renaming ((x_3 (x_4 ...)) ...) x (x_5 ...))
   ----
   (lookup-renaming ((x_0 (x_1 ...)) (x_3 (x_4 ...)) ...) x (x_5 ...))])

(define-judgment-form Full-Pika
  #:mode (==>PC I I I O)
  #:contract (==>PC layout-defs ρ e e)

  [(lookup-renaming ρ x [x_2 ...])
   ----
   (==>PC layout-defs ρ x (layout-arg x_2 ...))
   ]

  [(get-layout-params layout-defs A [x_2 ...])
   (fresh ([x_3 ...] [x_2 ...]))
   ----
   (==>PC layout-defs ρ (apply A x) (with ([x_2 ...] := x) (layout-arg x_2 ...)))
   ])

#;(define (-->PC defs Γ ρ)
  (reduction-relation
   Full-Pika
   #:domain e

   (--> x (layout-arg ,(ρ (term x))))
   
   (--> (apply A x) (with ([x_2 ...] := x) (layout-arg x_2 ...))
        (judgment-holds (get-layout-params ,defs A xs))
        (where [x_0 ...] xs)
        (fresh [[x_2 ...] [x_0 ...]])
   )))

#;(define (-->PC defs Γ C)
  (reduction-relation
   Full-Pika-Ctx
   #:domain e

   #;(--> (in-hole E x)
        (in-hole E (layout-arg x_1 ))

        (judgment-holds (has-type-pc ,Γ ,C x A))
        (judgment-holds (get-layout-def-type ,defs A layout-constraint xs))
        (fresh x_1)
        "PC-Var")

   ; (layout (x ...) T (h ...))
   ; (T ::= ((loc : τ) ...))
   (--> (in-hole E (apply A e_1))
        (in-hole E (layout [x ...] T (substitute [h ...] (a e) ...)))

        (judgment-holds (split-app e_1 Ctr [e ...]))
        (judgment-holds (get-layout-def-type ,defs A layout-constraint [x ...]))
        (judgment-holds (get-layout-branch ,defs A Ctr [a ...] [h ...]))  ; (Ctr a ...) := [h ...]
        (judgment-holds (get-loc-types ,Γ ,C (substitute [h ...] (a e) ...) T))
        "PC-Unfold-Layout-Ctr")

   (--> (in-hole E (e_1 (apply α e_2)))
        (in-hole E (with ((x ...) := e_3) (e_1 (layout-arg x ...))))
        
        (judgment-holds (has-type-pc ,Γ ,C (apply α e_2) τ))
        (judgment-holds (get-layout-def-type ,defs α layout-constraint [x ...]))
        "PC-With-Intro")

   (--> (in-hole E (e (with ((x ...) := e_1) e_2)))
        (in-hole E (with ((y ...) := e_1) (e (substitute e_2 (x y) ...))))

        (fresh ((y ...) (x ...)))
        "PC-With-App")
   
   (--> (in-hole E (with ((x ...) := e) (apply A e_2)))
        (in-hole E (apply A (with ((x ...) := e) e_2)))
        "PC-With-Layout-Apply")

   #;(--> (apply α e)
        e
        "PC-Apply")))



;;; Tests

(define List
  (term
   (data List := (Nil) (Cons Int List))))

(define Tree
  (term
   (data Tree := (Tip) (Node Int Tree Tree))))


#;(layout-def ::= ((A : (Layout X) [x ...]) layout-case ...))
#;(layout-case ::= (pat := (h ...)))
(define Sll
  (term
   ((Sll : (Layout List) [x])
    ((Nil) := [])
    ((Cons head tail) := [(x ↦ head) ((x + 1) ↦ tail)]))))

(define left-list
  (term
   ((left-list : ((α ~ (Layout Tree)) (β ~ (Layout List))) ⇒ (α → β))
    ((Tip) := (apply β Nil))
    ((Node x left right) := (apply β ((Cons x) ((apply β (apply α left-list)) left)))))))

(define globals `[,Sll])

(define global-data-defs `(,List ,Tree))

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

#;(traces (-->PC globals (term [(f : ((a ~ (Layout List)) ⇒ (a → a))) (y : Sll)]) (term [(Sll ~ (Layout List))])) (term (f (with ((x) := e_1) e_2))))
#;(traces (-->PC globals (term [(f : ((a ~ (Layout List)) ⇒ (a → a))) (y : Sll)]) (term [(Sll ~ (Layout List))])) (term (apply Sll ((Cons 1) y))))
