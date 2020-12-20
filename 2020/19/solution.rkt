#lang fiddle

(require fiddle/prelude
         fiddle/stdlib/CoList
         fiddle/stdlib/Eff
         fiddle/stdlib/Table
         fiddle/stdlib/IO
         "../../regex.rkt"
         "../../graph.rkt"
         )

(define! newline/lex (! char/lex "\n" (~! abort 'newline)))
;; (define! pipe (! <<v list->string 'o List 'o integer->char 124))

(define! pipe/lex (! char/lex "|" (~! abort 'or)))
(define! space/lex (! char/lex ": " (~! abort 'space)))
(define! quotes/e (! char/e "\""))
(define! a/e (! char/e "a"))
(define! b/e (! char/e "b"))
(define! ab/e (! alt/e a/e b/e))
(define! terminal/e
  (! idiom^ compile-regex (~! cat/e quotes/e ab/e quotes/e)))
(define terminal/lex (list second terminal/e))
(define! input/lex
  (! <<v (~! List Ret) 'o compile-regex 'o star/e ab/e))

(define sig (list newline/lex unsigned-number/lex pipe/lex space/lex terminal/lex input/lex))

(def-thunk (! tokenize f)
   (! CBN (~! <<v fold-lex-string sig 'o (~! read-all-string f))
      % n> (~! cl-filter (~! <<v not 'o equal? 'space))
      % n> (~! sep-by 'newline)
      % n> (~! sep-by '())
      % n> list<-colist
      % n$))

;; Let's solve this using our regex engine.
;
;; First, topologically sort the rules (find it from a previous year lol)

;; Then, make 1 big regex? Might take a longo time


;; (define! 5/e (! char/e "b"))
;; (define! 4/e (! char/e "a"))
;; (define! 3/e
;;   (! idiom^ alt/e (~! cat/e 4/e 5/e) (~! cat/e 5/e 4/e)))
;; (define! 2/e
;;   (! idiom^ alt/e (~! cat/e 4/e 4/e) (~! cat/e 5/e 5/e)))
;; (define! 1/e
;;   (! idiom^ alt/e (~! cat/e 2/e 3/e) (~! cat/e 3/e 2/e)))
;; (define! 0/e (! cat/e 4/e 1/e 5/e))
;; (define! 0/ce (! compile-regex 0/e))


;; (def-thunk (! parse))
(def-thunk (! dependency-graph es)
  (! foldl es
     (~ (copat [(tbl (cons from exp))
                [tos <- (! filter number? exp)]
                (! foldl tos
                   (~ (λ (tbl to) (! tbl 'set (list from to) #t)))
                   tbl)]))
     empty-table))

(def-thunk (! to-regex cs e-tbl)
  [terminal/e = (~ (copat
                    [(#\a) (ret a/e)]
                    [(#\b) (ret b/e)]
                    [(n)   (! e-tbl 'get n 'missing)]))]
  (! CBN  (~! colist<-list cs)
     % n> (~! sep-by 'or)
     % n> (~! cl-map (~! map terminal/e))
     % n> (~! cl-map (~! apply cat/e))
     % n> (~! cl-foldl^ alt/e empty/e)
     % n$))

(def-thunk (! mk-regexes e-tbl order)
  (! foldl order
     (~ (λ (regex-tbl n)
          (do [exp <- (! e-tbl 'get n 'missingno)]
              [regex <- (! to-regex exp regex-tbl)]
            (! regex-tbl 'set n regex))))
     empty-table))

;; 
(def-thunk (! main-a f)
  (patc (! tokenize f)
   [(list exps strs)
    [deps <- (! dependency-graph exps)]
    [order <- (! topo-sort deps)]
    [exp-tbl <- (! table<-list exps)]
    [regex-tbl <- (! mk-regexes exp-tbl order)]
    [0/e <- (! <<v regex-tbl 'get 0 'missingno)]
    (! displayall 'made-regex)
    (! CBN (~! colist<-list strs)
       % n> (~! cl-map first)
       % n> (~ (λ (c)
                 (! cl-foldr c
                    (~ (λ (str tl state)
                         (do 
                             (! displayall 'trying str)
                             (patc (! stateful-lazy-compiling-regex-matches? state 0/e str)
                                   [(list b state)
                                    (if b
                                        (do (! displayall 'matched: str)
                                            (! cl-cons str (~! tl state)))
                                        (! tl state))]))))
                    (~ (λ (_) (! cl-nil)))
                    init-lcr-state)))
       % n> cl-length
       % n$)]))

(def/copat (! e1ne2n-loop e1 e2 last acc)
  [(0) (! apply (~! alt/e last) acc)]
  [(n)
   [n <- (! - n 1)]
   [next <- (! cat/e e1 last e2)]
   (! e1ne2n-loop e1 e2 next (cons last acc) n)])

;; Finite unrolling of e1^n e2^n
;; so e1 e2 | e1 e1 e2 e2 | e1 e1 e1 e2 e2 e2 | ...
(def-thunk (! e1n-e2n e1 e2 m)
  [e1e2 <- (! cat/e e1 e2)]
  (! e1ne2n-loop e1 e2 e1e2 '() m))

(def-thunk (! mk-regexes-b e-tbl order)
  (! foldl order
     (~ (λ (regex-tbl n)
          (cond [(! = n 8) ;; make 8: 42 | 42 8
                 [e42 <- (! regex-tbl 'get 42 'missingno)]
                 [e8 <- (! +/e e42)]
                 (! regex-tbl 'set n e8)]
                [(! = n 11) ;; 11: 42 31 | 42 11 31
                 ;; this isn't regular, but........
                 ;; we also have bounded output
                 ;; the longest line is 100 long
                 ;; so we can do a finite unfolding of this...
                 [e42 <- (! regex-tbl 'get 42 'missingno)]
                 [e31 <- (! regex-tbl 'get 31 'missingno)]
                 [e42+ <- (! +/e e42)]
                 [UNFOLD-COUNT = 4] ;; this is the lowest number that works (also the first one I tried :))
                 [e11 <- (! e1n-e2n e42 e31 UNFOLD-COUNT)]
                 (! displayall 'unfolded UNFOLD-COUNT)
                 ;; (! displayall e11)
                 (! regex-tbl 'set 11 e11)]
                [else
                 (do [exp <- (! e-tbl 'get n 'missingno)]
                     [regex <- (! to-regex exp regex-tbl)]
                   (! regex-tbl 'set n regex))])))
     empty-table))

(def-thunk (! main-b f)
  (patc (! tokenize f)
   [(list exps strs)
    [deps <- (! dependency-graph exps)]
    [order <- (! topo-sort deps)]
    (! displayall 'order: order)
    [exp-tbl <- (! table<-list exps)]
    [regex-tbl <- (! mk-regexes-b exp-tbl order)]
    [0/e <- (! <<v regex-tbl 'get 0 'missingno)]
    (! displayall 'made-regex)
    (! CBN (~! colist<-list strs)
       % n> (~! cl-map first)
       % n> (~ (λ (c)
                 (! cl-foldr c
                    (~ (λ (str tl state)
                         (do 
                             (! displayall 'trying str)
                             (patc (! stateful-lazy-compiling-regex-matches? state 0/e str)
                                   [(list b state)
                                    (if b
                                        (do (! displayall 'matched: str)
                                            (! cl-cons str (~! tl state)))
                                        (! tl state))]))))
                    (~ (λ (_) (! cl-nil)))
                    init-lcr-state)))
       % n> cl-length
       % n$)])

  )

(provide main-a main-b)
