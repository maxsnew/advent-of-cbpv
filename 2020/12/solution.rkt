#lang fiddle

(require fiddle/prelude
         fiddle/stdlib/CoList
         fiddle/stdlib/IO
         "../../regex.rkt")

(define! sig (! idiom^ List
                (~! exact-string/lex "N" 'N)
                (~! exact-string/lex "S" 'S)
                (~! exact-string/lex "E" 'E)
                (~! exact-string/lex "W" 'W)
                (~! exact-string/lex "L" 'L)
                (~! exact-string/lex "R" 'R)
                (~! exact-string/lex "F" 'F)
                (~! exact-string/lex "\n" 'space)
                (~ (ret unsigned-number/lex))))

(def-thunk (! parse c)
  (! CBN (~! <<v fold-lex-string sig 'o list->string 'o list<-colist c)
     % n> (~! cl-filter (~! <<v not 'o equal? 'space ))
     % n> (~! chunks 2)
     ;; % n> (~! cl-map debug)
     % n$))

;; A Dir is 'N 'S 'E or 'W
;; A Pt is a (List Nat Nat)
;; A State is a (List Pt Dir)

(def-thunk (! scale (list x y) s) (! idiom^ List (~! * s x) (~! * s y)))
(def-thunk (! add-pt (list x1 y1) (list x2 y2))
  (! idiom^ List (~! + x1 x2) (~! + y1 y2)))
(def-thunk (! add-x pt x) (! add-pt pt (list x 0)))
(def-thunk (! add-y pt y) (! add-pt pt (list 0 y)))

(def-thunk (! rotate ang1 ang2) (! <<v swap modulo 360 'o + ang1 ang2))

(def/copat (! vec<-dir)
  [  (0) (ret (list  1  0))]
  [ (90) (ret (list  0  1))]
  [(180) (ret (list -1  0))]
  [(270) (ret (list  0 -1))])

(def/copat (! follow-instruction-a)
  [((list loc dir) (list 'N n)) [loc <- (! add-y loc n)]
   (ret (list loc dir))]
  [((list loc dir) (list 'S n)) [loc <- (! <<v add-y loc 'o * -1 n)]
   (ret (list loc dir))]
  [((list loc dir) (list 'E n)) [loc <- (! add-x loc n)]
   (ret (list loc dir))]
  [((list loc dir) (list 'W n)) [loc <- (! <<v add-x loc 'o * -1 n)]
   (ret (list loc dir))]
  [((list loc dir) (list 'L n)) [dir <- (! rotate dir n)]
   (ret (list loc dir))]
  [((list loc dir) (list 'R n)) [dir <- (! <<v rotate dir 'o * -1 n)]
   (ret (list loc dir))]
  [((list loc dir) (list 'F n)) [loc <- (! <<v add-pt loc 'o swap scale n 'o vec<-dir dir)]
   (ret (list loc dir))])

(def-thunk (! follow-instructions step is start-pt start-dir)
  (! cl-foldl is
     (~ (λ (st i) (do (! displayall 'update st i) (! step st i))))
     (list start-pt start-dir)))

(def-thunk (! main-a f)
  (patc (! follow-instructions follow-instruction-a (~! parse (~! read-all-chars f)) (list 0 0) 0)
        [(list (list x y) dir)
         (! displayall 'loc: (list x y))
         (! displayall 'dir: dir)
         (! idiom^ + (~! abs x) (~! abs y))]))

(def-thunk (! complex-* (list a1 b1) (list a2 b2))
  (! idiom^ List
     (~! idiom^ - (~! * a1 a2) (~! * b1 b2))
     (~! idiom^ + (~! * a1 b2) (~! * b1 a2))))

(def/copat (! follow-instruction-b)
  [((list s-loc wp-loc) (list 'N n)) [wp-loc <- (! add-y wp-loc n)]
   (ret (list s-loc wp-loc))]
  [((list s-loc wp-loc) (list 'S n)) [wp-loc <- (! <<v add-y wp-loc 'o * -1 n)]
   (ret (list s-loc wp-loc))]
  [((list s-loc wp-loc) (list 'E n)) [wp-loc <- (! add-x wp-loc n)]
   (ret (list s-loc wp-loc))]
  [((list s-loc wp-loc) (list 'W n)) [wp-loc <- (! <<v add-x wp-loc 'o * -1 n)]
   (ret (list s-loc wp-loc))]
  [((list s-loc wp-loc) (list 'F n)) [s-loc <- (! <<v add-pt s-loc 'o scale wp-loc n)]
   (ret (list s-loc wp-loc))]
  [((list s-loc wp-loc) (list 'L n)) [wp-loc <- (! <<v complex-* wp-loc 'o vec<-dir n)]
   (ret (list s-loc wp-loc))]
  [((list s-loc wp-loc) (list 'R n)) [wp-loc <- (! <<v complex-* wp-loc 'o vec<-dir 'o rotate 0 'o * -1 n)]
   (ret (list s-loc wp-loc))])

(def-thunk (! main-b f)
  (patc (! follow-instructions follow-instruction-b (~! parse (~! read-all-chars f)) (list 0 0) (list 10 1))
        [(list (list x y) (list wx wy))
         (! displayall 'loc: (list x y))
         (! displayall 'wp-loc: (list wx wy))
         (! idiom^ + (~! abs x) (~! abs y))]))

(provide main-a
         main-b)
