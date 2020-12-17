#lang fiddle

(require fiddle/prelude
         fiddle/stdlib/CoList
         fiddle/stdlib/Eff
         fiddle/stdlib/Table
         fiddle/stdlib/IO
         "../../regex.rkt"
         )

(define! dot/lex (! char/lex "." (~! abort '_)))
(define! hash/lex (! char/lex "#" (~! abort '+)))
(define! space/lex (! char/lex "\n" (~! abort 'space)))
(define sig (list dot/lex hash/lex space/lex))

(def-thunk (! parse f)
   (! CBN (~! <<v fold-lex-string sig 'o (~! read-all-string f))
     % n> (~! sep-by 'space)
     % n$))

(def-thunk (! neighbor-vecsn n)
  [origin <- (! list<-colist (~! repeat 0 n))]
  [rng = (~! range -1 2)]
  [ranges <- (! map (~! abort rng) origin)]
  (! CBN (~! apply cartesian-product ranges)
     % n> (~! cl-filter (~! <<v not 'o equal? origin))
     % n> list<-colist
     % n$))

(define! neighbor-vecs3 (! neighbor-vecsn 3))
(define! neighbor-vecs4 (! neighbor-vecsn 4))

(def/copat (! init-tbl zeros tbl y)
  [((cons row rows))
   [tbl <- (! cl-foldl (~! cl-zipwith (~! colist<-list row) (~! range 0))
        (~ (copat [(tbl (list '+ x))
                   [coord <- (! append (list x y) zeros)]
                   (! tbl 'set coord #t)]
                  [(tbl (list '_ x)) (ret tbl)]))
        tbl)]
   [y <- (! + y 1)]
   (! init-tbl zeros tbl y rows)]
  [('()) (ret tbl)])

(def-thunk (! coord-add pt1 pt2)
  (cond [(! null? pt1) (ret '())]
        [else
         [x1 <- (! first pt1)]
         [rest1 <- (! rest pt1)]
         [x2 <- (! first pt2)]
         [rest2 <- (! rest pt2)]
         [x3    <- (! + x1 x2)]
         [rest3 <- (! coord-add rest1 rest2)]
         (ret (cons x3 rest3))]))

(def-thunk (! neighbors-of dim pt)
  [neighbor-vecs <- (pat dim [3 (ret neighbor-vecs3)] [4 (ret neighbor-vecs4)])]
  (! cl-map (~! coord-add pt) (~! colist<-list neighbor-vecs)))

(def-thunk (! keys t)
  (! <<v map car 'o t 'to-list))

(def-thunk (! pt<lr xs1 xs2)
  (cond [(! null? xs1) (ret #f)]
        [else
         [x1 <- (! first xs1)]
         [xs1 <- (! rest xs1)]
         [x2 <- (! first xs2)]
         [xs2 <- (! rest xs2)]
         (! or (~! < x1 x2)
            (~! and (~! = x1 x2) (~! pt<lr xs1 xs2)))]))

(def-thunk (! pt< xs1 xs2)
  [sx1 <- (! reverse xs1)]
  [sx2 <- (! reverse xs2)]
  (! pt<lr sx1 sx2))

(def-thunk (! coords t)
  (! <<v swap sort (~! pt<) 'o keys t))

;; Listof Coord -> F (Listof Coord)
(def-thunk (! all-neighbors dim cs)
  [t <- (! foldl cs
      (~ (λ (t c) (! cl-foldl (~! neighbors-of dim c)
                     (~ (λ (t c) (! t 'set c #t)))
                     t)))
      empty-table)]
  (! keys t))

;; Point -> Space -> F (2 or 3 or 'other)
(def-thunk (! look-around dim pt tbl)
  (patc (! CBN (~! neighbors-of dim pt)
       % n> (~! cl-filter (~! tbl 'has-key?))
       % n> (~! take 4)
       % n$)
        [nearby
         [l <- (! length nearby)]
         (cond [(! = l 2) (ret 2)]
               [(! = l 3) (ret 3)]
               [else (ret 'other)])]))

(def-thunk (! next-generation dim last-gen)
  [active-pts <- (! coords last-gen)]
  [l <- (! length active-pts)]
  (! displayall 'this-gen-size: l)
  [possible-new-pts <- (! <<v all-neighbors dim active-pts)]
  ;; (! displayall 'possible-new-pts: possible-new-pts)
  (! foldl possible-new-pts
     (~ (λ (t pt)
          (do [nearby <- (! look-around dim pt last-gen)]
              (cond [(! last-gen 'has-key? pt)
                     (pat nearby
                          ['other (ret t)]
                          [_ (! t 'set pt #t)])]
                    [else
                     (pat nearby
                          [3 (! t 'set pt #t)]
                          [_ (ret t)])]))))
     empty-table))

(def-thunk (! parse-table dim f)
  [inp <- (! list<-colist (~! parse f))]
  [dim-2 <- (! - dim 2)]
  [zeros <- (! list<-colist (~! repeat 0 dim-2))]
  (! init-tbl zeros empty-table 0 inp))

(def-thunk (! main dim f)
  [t <- (! parse-table dim f)]
  [generations = (~! iterate (~! next-generation dim) t)]
  (! <<v length 'o coords 'o nth 6 generations))

(def-thunk (! main-a) (! main 3))

(def-thunk (! main-b) (! main 4))

(provide main-a main-b)
