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

(define! neighbor-vecs
  (! CBN (~! cartesian-product (~! range -1 2) (~! range -1 2) (~! range -1 2))
     % n> (~! cl-filter (~! <<v not 'o equal? (list 0 0 0)))
     % n> list<-colist
     % n$))

(def/copat (! init-tbl tbl y)
  [((cons row rows))
   [tbl <- (! cl-foldl (~! cl-zipwith (~! colist<-list row) (~! range 0))
        (~ (copat [(tbl (list '+ x))
                   (! tbl 'set (list x y 0) #t)]
                  [(tbl (list '_ x)) (ret tbl)]))
        tbl)]
   [y <- (! + y 1)]
   (! init-tbl tbl y rows)]
  [('()) (ret tbl)])

(def-thunk (! coord-add xyz1 xyz2)
  [x1 <- (! first xyz1)]
  [y1 <- (! second xyz1)]
  [z1 <- (! third xyz1)]
  [x2 <- (! first xyz2)]
  [y2 <- (! second xyz2)]
  [z2 <- (! third xyz2)]
  [x <- (! + x1 x2)]
  [y <- (! + y1 y2)]
  [z <- (! + z1 z2)]
  (ret (list x y z)))

(def-thunk (! neighbors-of pt)
  (! cl-map (~! coord-add pt) (~! colist<-list neighbor-vecs)))

(def-thunk (! keys t)
  (! <<v map car 'o t 'to-list))

(def-thunk (! pt< xyz1 xyz2)
  [x1 <- (! first xyz1)]
  [y1 <- (! second xyz1)]
  [z1 <- (! third xyz1)]
  [x2 <- (! first xyz2)]
  [y2 <- (! second xyz2)]
  [z2 <- (! third xyz2)]
  (! or (~! < z1 z2)
     (~! and (~! = z1 z2)
         (~! or (~! < y1 y2)
            (~! and (~! = y1 y2) (~! < x1 x2))))))

(def-thunk (! coords t)
  (! <<v swap sort pt< 'o keys t))

;; Listof Coord -> F (Listof Coord)
(def-thunk (! all-neighbors cs)
  [t <- (! foldl cs
      (~ (λ (t c) (! cl-foldl (~! neighbors-of c)
                     (~ (λ (t c) (! t 'set c #t)))
                     t)))
      empty-table)]
  (! keys t))

;; Point -> Space -> F (2 or 3 or 'other)
(def-thunk (! look-around pt tbl)
  (patc (! CBN (~! neighbors-of pt)
       % n> (~! cl-filter (~! tbl 'has-key?))
       % n> (~! take 4)
       % n$)
        [nearby
         [l <- (! length nearby)]
         (cond [(! = l 2) (ret 2)]
               [(! = l 3) (ret 3)]
               [else (ret 'other)])]))

(def-thunk (! next-generation last-gen)
  [active-pts <- (! coords last-gen)]
  (! displayall 'this-gen: active-pts)
  [possible-new-pts <- (! all-neighbors active-pts)]
  (! foldl possible-new-pts
     (~ (λ (t pt)
          (do [nearby <- (! look-around pt last-gen)]
              (cond [(! last-gen 'has-key? pt)
                     (pat nearby
                          ['other (ret t)]
                          [_ (! t 'set pt #t)])]
                    [else
                     (pat nearby
                          [3 (! t 'set pt #t)]
                          [_ (ret t)])]))))
     empty-table))

(def-thunk (! main-a f)
  [inp <- (! list<-colist (~! parse f))]
  [t <- (! init-tbl empty-table 0 inp)]
  ;; to make 
  [init-pts <- (! coords t)]
  [generations = (~! iterate next-generation t)]
  (! <<v length 'o coords 'o nth 6 generations))

(def-thunk (! main-b f)
  (! error 'nyi)
  )

(provide main-a main-b)
