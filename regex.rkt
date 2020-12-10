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
(define */e star/e)

(def-thunk (! +/e e)
  (! idiom^ (~! cat/e e) (~! star/e e)))

(def-thunk (! ?/e e)
  (! alt/e 'epsilon e))

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

(def-thunk (! num<-digits base ds)
  (! cl-foldl (~! colist<-list ds)
     (~ (copat [(acc x)
               (! idiom^ (~! + x) (~! * base acc))]))
     0))
(def/copat (! parse-num base)
  [((cons #\+ ds)) (! parse-num base ds)]
  [((cons #\- ds)) (! <<v * -1 'o parse-num base ds)]
  [(ds)
   (! CBN (~! colist<-list ds)
      % n> (~! cl-map (~! <<v swap - 48 'o char->integer))
      % n> (~! cl-foldl^ (~ (copat [(acc x) (! idiom^ (~! + x) (~! * base acc))])) 0)
      % n$)])

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

(def-thunk (! dfa-accepts-null? (list state tbl))
  (pat state
       ['empty (ret #f)]
       [_
        (cond [(! tbl 'has-key? state) (! idiom^ first (~! tbl 'get state 'error))]
              [else (! error 'dfa-doesnt-have-this-state)])]))

(def/copat (! dfa-Deriv c)
  [((list state tbl))
   (patc (! tbl 'get state #f) [(list accepts-null? trans-tbl)
    (cond [(! trans-tbl 'has-key? c) [state <- (! trans-tbl 'get c #f)]
           (! List state tbl)]
          [else (! List 'empty tbl)])])])

(def/copat (! empty-dfa?)
  [((list 'empty _)) (ret #t)]
  [(_)               (ret #f)])

(def/copat (! dfa-matches? dfa)
  [('()) (! dfa-accepts-null? dfa)]
  [((cons c cs))
   [ddfa/dc <- (! dfa-Deriv c dfa)]
   (cond [(! empty-dfa? ddfa/dc) (ret #f)]
         [else (! dfa-matches? ddfa/dc cs)])])

;; (def/copat (! long-match-loop dfa full-buf failK since-buf)
;;   [('())
;;    (! idiom^ failK (~! reverse since-buf))]
;;   [((cons c cs))
;;    [dfa <- (! dfa-Deriv c dfa)]
;;    (cond [(! dfa-accepts-null? dfa)
;;           [full-buf = (cons c full-buf)]
;;           [match <- (! reverse full-buf)]
;;           (! long-match-loop dfa full-buf (~! List match) '() cs)]
;;          [(! empty-dfa? dfa)
;;           [not-matched <- (! idiom^ append (~! reverse since-buf) (~! Cons c cs))]
;;           (! failK not-matched)]
;;          [else
;;           (! long-match-loop dfa (cons c full-buf) failK (cons c since-buf) cs)])])

(def-thunk (! long-match-loop dfa full-buf failK since-buf cs)
  (patc (! idiom^ view cs)
   ['()
    [since-buf <- (! reverse since-buf)]
    (! failK (~! colist<-list since-buf))]
   [(cons c cs)
    [dfa <- (! dfa-Deriv c dfa)]
    (cond [(! dfa-accepts-null? dfa)
           [full-buf = (cons c full-buf)]
           [match <- (! reverse full-buf)]
           (! long-match-loop dfa full-buf (~! List match) '() cs)]
          [(! empty-dfa? dfa)
           [not-matched = (~! cl-append (~! idiom^ colist<-list (~! reverse (cons c since-buf))) cs)]
           (! failK not-matched)]
          [else
           (! long-match-loop dfa (cons c full-buf) failK (cons c since-buf) cs)])]))

(def-thunk (! dfa-longest-match dfa s)
  [init-k <- (cond [(! dfa-accepts-null? dfa) (ret (~! List '()))]
                   [else (ret (~! abort 'no-match))])]
  (! long-match-loop dfa '() init-k '() s))

(def-thunk (! dfa-vec-Deriv c dfas)
  [car*deriv = (~ (copat [((list f dfa)) (! idiom^ (~! List f) (~! dfa-Deriv c dfa))]))]
  (! idiom^ (~! filter (~! <<v not 'o equal? 'empty 'o first 'o  second)) (~! map car*deriv dfas)))

(def-thunk (! first-null-acceptor dfas)
  (patc (! idiom^ view (~! cl-filter (~! <<v dfa-accepts-null? 'o second) (~! colist<-list dfas)))
    ['()        (ret #f)]
    [(cons x _)
     ; (! displayall 'first-null-acceptor x)
     (ret x)]))

(def-thunk (! vec-long-match-loop dfas full-buf failK since-buf cs)
  ;; (! displayall 'loop dfas full-buf failK since-buf cs)
  (patc (! idiom^ view cs)
   ['()
    [since-buf <- (! reverse since-buf)]
    (! failK (~! colist<-list since-buf))]
   [(cons c cs)
    ; (! displayall 'cons c)
    [dfas <- (! dfa-vec-Deriv c dfas)]
    ; (! displayall 'derivatives dfas)
    (patc (! first-null-acceptor dfas)
      [(list accept dfa)
       ; (! displayall 'acceptor)
       [full-buf = (cons c full-buf)]
       [match <- (! idiom^ accept (~! reverse full-buf))]
       (! vec-long-match-loop dfas full-buf (~! List match) '() cs)]
      [#f
       ; (! displayall 'none-accept)
       (cond [(! null? dfas)
              ; (! displayall 'finally)
              [not-matched = (~! cl-append (~! idiom^ colist<-list (~! reverse (cons c since-buf))) cs)]
              (! failK not-matched)]
             [else
              ; (! displayall 'still-hope dfas)
              (! vec-long-match-loop dfas (cons c full-buf) failK (cons c since-buf) cs)])])]))

;; Lexing time

;; A lexSignature iTok oTok (aka LexSig) is a Listof (List (U (Listof iTok -> F oTok)) DFA)

;; lex : LexSig iTok oTok -> U(CoList iTok) -> F((List oTok U(CoList iTok)) \/ 'no-match)
(def-thunk (! lex sig itoks)
  [init-k <- (patc (! first-null-acceptor sig)
    [(list accept _)
     [match <- (! accept '())]
     (ret (~! List match))]
    [#f (ret (~! abort 'no-match))])]
  (! vec-long-match-loop sig '() init-k '() itoks))

;; number
;; (define! 3digits
;;   (! idiom^ compile-regex (~! idiom^ cat/e  (~! char/e "0123456789") (~! char/e "0123456789") (~! char/e "0123456789"))))

(define! digit/e (! char/e "0123456789"))
;; (define! astar
;;   (! idiom^ compile-regex (~! idiom^ star/e (~! char/e "a"))))
;; (define! bstar
;;   (! idiom^ compile-regex (~! idiom^ star/e (~! char/e "b"))))
;; (define! astarbstar
;;   (! idiom^ compile-regex (~! idiom^ cat/e (~! idiom^ star/e (~! char/e "a")) (~! idiom^ star/e (~! char/e "b")))))

;; (define! abstar
;;   (! idiom^ compile-regex (~! idiom^ star/e (~! idiom^ cat/e (~! char/e "a") (~! char/e "b")))))
(define parse-decimal (~! parse-num 10))
(define parse-binary  (~! parse-num 2))
(define! number/e   (! idiom^ cat/e
                       (~! idiom^ ?/e (~! char/e "+-"))
                       (~! +/e digit/e)))
(define! number/lex (! idiom^ (~! List parse-decimal) (~! compile-regex number/e)))

(define! unsigned-number/lex (! idiom^ (~! List parse-decimal) (~! idiom^ compile-regex (~! +/e digit/e))))

(define! bin/e   (! idiom^ cat/e (~! idiom^ ?/e (~! char/e "+-")) (~! idiom^ +/e (~! char/e "01"))))
(define! bin/lex (! idiom^ (~! List parse-binary) (~! compile-regex bin/e)))

(def-thunk (! string<-char c) (! list->string (list c)))
(def-thunk (! exact-string/e s)
  [cs <- (! <<v map char/e 'o map string<-char 'o string->list s)]
  (! apply cat/e cs))

(def/copat (! exact-string/lex s)
  [(#:bind) (! exact-string/lex s s)]
  [(val #:bind)
   [dfa <- (! idiom^ compile-regex (~! exact-string/e s))]
   (! List (~! abort val) dfa)])

(def-thunk (! fold-lex sig c)
  [step = (~ (λ (cs)
              (patc (! idiom^ view cs)
               ['() (! cl-nil)]
               [(cons c cs)
                (patc (! lex sig (~! cl-cons c cs))
                 ['no-match
                  [l <- (! list<-colist (~! cl-cons c cs))]
                  (! error "lexer didn't match anything" sig l)]
                 [(list otok cs)
                  (! Cons otok cs)])])))]
  (! cl-unfold step c))

(provide
 ;; regex stuff
 cat/e alt/e
 char/e
 star/e */e
 +/e ?/e
 digit/e
 number/e
 bin/e
 exact-string/e

 compile-regex

 ;; lexing stuff
 lex fold-lex
 bin/lex
 number/lex
 unsigned-number/lex
 exact-string/lex)
(! displayall 'end-of-regex.rkt)
