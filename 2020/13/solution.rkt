#lang fiddle

(require fiddle/prelude
         fiddle/stdlib/CoList
         fiddle/stdlib/IO
         "../../regex.rkt"
         )

(define! comma/lex (! exact-string/lex "," 'comma))
(define! newline/lex (! exact-string/lex "\n" 'newline))
(define! x/lex (! exact-string/lex "x" 'x))

(define sig (list unsigned-number/lex comma/lex x/lex newline/lex))

(def-thunk (! parse inp)
  (! CBN (~! <<v fold-lex-string sig 'o list->string 'o list<-colist inp)
     % n> (~! cl-filter (~! <<v not 'o equal? 'comma))
     % n> (~! sep-by 'newline)
     % n> list<-colist
     % n$))

(def-thunk (! remainder earliest time)
  (! idiom^ (~! - time) (~! modulo earliest time)))

(def-thunk (! part-a earliest times)
  [id <- (! CBN (~! colist<-list times)
       % n> (~! minimum-by1 (~! remainder earliest))
       % n$)]
  (! <<v * id 'o remainder earliest id ))

;; for part b:

;; we have some inputs (p1 r1) (p2 r2) (p3 r3) ...
;; we want to find the least number n such that
;;   rem n p1 = r1
;;   rem n p2 = r2
;;   ...
;
;; This is exactly the chinese remainder theorem!

;; First, we need to implement the bezout algorithm (aka ext euclidean algo)
;; that given two rel prime numbers a and b finds integer coefficients m and n such that
;
;; am + bn = 1
;
;; Given this, to find x mod p1 = r1 and x mod p2 = r2
;; first find m1p1 + m2p2 = 1, then
;
;; x = r1 m2 m2 + r2 m1 p1

;;   = r1 (1 - m1 p1) + r2 m1 p1
;;   = r1 + (r2 - r1)m1 p1
;; so x mod p1 = r1


(def-thunk (! bezout-loop ri si ti ri+1 si+1 ti+1)
  [qi+1 <- (! quotient ri ri+1)]
  [ri+2 <- (! <<v - ri 'o * qi+1 ri+1)]
  [si+2 <- (! <<v - si 'o * qi+1 si+1)]
  [ti+2 <- (! <<v - ti 'o * qi+1 ti+1)]
  (cond [(! = 0 ri+2) (ret (list ri+1 si+1 ti+1))]
        [else (! bezout-loop ri+1 si+1 ti+1 ri+2 si+2 ti+2)]))

(def-thunk (! bezout n1 n2)
  (! bezout-loop n1 1 0 n2 0 1))


(def-thunk (! constraints<-inp xs acc i)
  (pat xs
       ['() (! reverse acc)]
       [(cons x xs)
        [i+1 <- (! - i 1)]
        (! constraints<-inp xs (cons (list 'cong i 'mod x) acc) i+1)]))

;; chinese remainder theorem

(def-thunk (! crm2 (list 'cong a1 'mod n1) (list 'cong a2 'mod n2))
  (patc (! bezout n1 n2) [(list 1 m1 m2)
    (! idiom^ + (~! * a1 m2 n2) (~! * a2 m1 n1))]))

(def-thunk (! crm (cons cr cs))
  [crm-step = (~ (λ (c1 c2)
                   (do [n1 <- (! fourth c1)]
                       [n2 <- (! fourth c2)]
                       [a12 <- (! crm2 c1 c2)]
                       [n12 <- (! * n1 n2)]
                       (ret (list 'cong a12 'mod n12)))))]
  (! foldl cs crm-step cr))

(def-thunk (! main-a f)
  (patc (! parse (~! read-all-chars f))
        [(list (list earliest) ids)
         [ids <- (! filter number? ids)]
         (! part-a earliest ids)]))

(def-thunk (! main-b f)
  (patc (! parse (~! read-all-chars f))
        [(list (list earliest) ids)
         [constraints <- (! <<v filter (~! <<v number? 'o fourth) 'o constraints<-inp ids '() 0)]
         (patc (! crm constraints)
               [(list 'cong a 'mod p)
                (! modulo a p)])]))

(provide main-a main-b)
