#lang fiddle

(require fiddle/prelude
         fiddle/stdlib/CoList
         fiddle/stdlib/Table
         )

;; RE-NF
;; 'empty
;; 'epsilon
;; (list 'alt (setof Char) (setof e)) where the list is sorted, has no duplicates and does not contain the empty language
;; (list 'cat e1 ...) where the list has at least 2 elements and does not contain a 'cat, a null language or an empty language
;; (list 'star re) where re is not a star, null or empty

;; (def-thunk (! char/e c)
;;   [s <- (! list->set (list c))]
;;   (! List 'alt s))

;; (def-thunk (! alt (rest es))
;;   [cs <- (! <<v 'o apply append )]
;;   )

(def/copat (! cat2)
  [('empty e2) (ret 'empty)]
  [(e1 'empty) (ret 'empty)]
  [('epsilon e2) (ret e2)]
  [(e1 'epsilon) (ret e1)]
  [((cons 'cat es1) (cons 'cat es2))
   (! idiom^ (~! Cons 'cat) (~! append es1 es2))]
  [((cons 'cat es1) e2)
   (! idiom^ (~! Cons 'cat) (~! append es1 (list e2)))]
  [(e1 (cons 'cat es2))
   (ret (cons 'cat (cons e1 es2)))]
  [(e1 e2)
   (ret (list 'cat e1 e2))])

(def-thunk (! cat/e (rest es))
  (! foldr es cat2 'epsilon))

(define! empty-set (! list->set '()))
(def-thunk (! union s1 s2)
  (! idiom^ list->set (~! idiom^ append (~! set->list s1) (~! set->list s2))))

(def-thunk (! set-cons x s)
  (! idiom^ list->set (~! idiom^ (~! Cons x) (~! set->list s))))

(def/copat (! alt2)
  [('empty e2) (ret e2)]
  [(e1 'empty) (ret e1)]
  [((list 'alt cs1 es1) (list 'alt cs2 es2))
   (! idiom^ (~! List 'alt) (~! union cs1 cs2) (~! union es1 es2))]
  [((list 'alt cs es) e)
   (! idiom^ (~! List 'alt cs) (~! set-cons e es))]
  [(e (list 'alt cs es))
   (! idiom^ (~! List 'alt cs) (~! set-cons e es))]
  [(e1 e2) (! idiom^ (~! List 'alt) (~! list->set '()) (~! list->set (list e1 e2)))])

(def-thunk (! alt/e (rest es))
  (! foldl es alt2 'empty))

(def-thunk (! char/e str)
  [s <- (! <<v list->set 'o string->list str)]
  (! List 'alt s empty-set))

(def-thunk (! colist<-set s)
  (! idiom^ colist<-list (~! set->list s)))

(def/copat (! star/e)
  [((list 'star e)) (ret (list 'star e))]
  [('empty)   (ret 'epsilon)]
  [('epsilon) (ret 'epsilon)]
  [(e) (ret (list 'star e))])

(def/copat (! null-accepting?)
  [('empty)   (ret #f)]
  [('epsilon) (ret #t)]
  [((list 'alt cs es)) (! any? (~! cl-map null-accepting? (~! colist<-set es)))]
  [((cons 'cat es))    (! all? (~! cl-map null-accepting? (~! colist<-list es)))]
  [((list 'star e)) (ret #t)])

(def-thunk (! intersect-epsilon e)
  (cond [(! null-accepting? e) (ret 'epsilon)]
        [else (ret 'empty)]))

;; cons c cs matches e iff cs matches deriv c e
(def/copat (! Deriv c)
  [('empty)   (ret 'empty)]
  [('epsilon) (ret 'empty)]
  [((list 'alt cs es))
   [cs-deriv <- (cond [(! member? c (~! colist<-set cs)) (ret 'epsilon)]
                      [else (ret 'empty)])]
   (! idiom^ (~! apply (~! alt/e cs-deriv)) (~! idiom^ (~! map (~! Deriv c)) (~! set->list es)))]
  [((cons 'cat (cons e es)))
   [cat-es <- (pat es [(list e^) (ret e^)] [es (ret (cons 'cat es))])]
   (! idiom^ alt2
      (~! idiom^ cat2 (~! Deriv c e)           (~ (ret cat-es)))
      (~! idiom^ cat2 (~! intersect-epsilon e) (~! Deriv c cat-es)))]
  [((list 'star e))
   (! idiom^ cat2 (~! Deriv c e) (~ (ret (list 'star e))))])

;; matches? : ParseExp Tok Tree -> Listof Tok -> F Bool
(def/copat (! matches?)
  [(e '()) (! null-accepting? e)]
  [(e (cons c cs))
   [de/dc <- (! Deriv c e)]
   (! matches? de/dc cs)])

;; compute the support of the expression on the alphabet
(def/copat (! support)
  [('empty) (ret '())]
  [('epsilon) (ret '())]
  [((list 'alt cs es))
   [es <- (! set->list es)]
   (! idiom^ (~! foldl^ union empty-set) (~! idiom^ (~! Cons cs) (~! map support es)))]
  [((cons 'cat es))
   (! idiom^ (~! foldl^ union empty-set) (~! map support es))]
  [((list 'star e))
   (! support e)])

;; A DFA is a pair of Regex and DFATable
;; A DFATable is a Table Regex (List Bool (Table Char Regex))

(def/copat (! cr-loop support tbl)
  [('()) (ret tbl)]
  [((cons new new-es))
   [new-table <- (! foldr
                    support
                    (~ (λ (c tbl) (! idiom^ (~! tbl 'set c) (~! Deriv c new))))
                    empty-table)]
   [new-es <-
     (! CBV (~! new-table 'to-list)
        % v> (~! map cdr)
        % v> (~! filter (~ (copat [(e) (! idiom^ not (~! tbl 'has-key? e))])))
        % v> (~! append new-es)
        % v$)]
   [new-null? <- (! null-accepting? new)]
   [tbl <- (! tbl 'set new (list new-null? new-table))]
   (! cr-loop support tbl new-es)])

(def-thunk (! compile-regex e)
  [supp <- (! idiom^ (~! set->list) (~! support e))]
  (! idiom^ (~! List e) (~! cr-loop supp empty-table (list e))))

(def/copat (! view-compiled-regex)
  [((list start tbl))
   (! idiom^ (~! List start)
      (~! idiom^
          (~! map (~ (copat [((list state accepting? transition))
                             (! idiom^ (~! List state accepting?) (~! transition 'to-list))])))
          (~! tbl 'to-list)))])

;; number
(define! digits
  (! idiom^ star/e (~! char/e "0123456789")))

(define! astar
  (! idiom^ star/e (~! char/e "a")))
(define! bstar
  (! idiom^ star/e (~! char/e "b")))
(define! astarbstar
  (! cat/e astar bstar))

(define! abstar
  (! idiom^ star/e (~! idiom^ cat/e (~! char/e "a") (~! char/e "b"))))
