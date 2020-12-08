#lang fiddle

(require fiddle/prelude
         fiddle/stdlib/Eff
         "../../eff-parse.rkt")

(provide inputp linep)

(def-thunk (! <$ e1 (rest es))
  (! apply (~! mapE (~ (copat [(x (rest _)) (ret x)])) e1) es))


(define DIGITS (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
(define-rec-thunk (! digit<-char-loop c n ds)
  (do [d <- (! car ds)]
      [ds <- (! cdr ds)]
    (cond
      [(! equal? c d) (ret n)]
      [#:else
       (do [n <- (! + 1 n)]
           (! digit<-char-loop c n ds))])))

(def-thunk (! digit<-char c)
  (! digit<-char-loop c 0 DIGITS))

(def-thunk (! parse-num-loop acc)
  (copat
   [(#:bind) (ret acc)]
   [(d) [n <- (! digit<-char d)]
        [acc <- (! <<v + n 'o * 10 acc '$)]
        (! parse-num-loop acc)]))

(def/copat (! parse-num^)
  [((= #\+)) (! parse-num-loop 0)]
  [((= #\-)) (! <<v - 'o parse-num-loop 0)]
  [() (! parse-num-loop 0)])

;; Parses a list of characters into a natural number
;; Listof Char -> F Nat
(define-thunk (! parse-num ds)
  (! apply parse-num^ ds))

(def-thunk (! spacep) (! exactp #\space))

(def-thunk (! nump)
  [not-hyphen-or-space = (~ (λ (x)
                             (! and (~! CBV (~! equal? x #\-)     % v> not % v$)
                                    (~! CBV (~! equal? x #\space) % v> not % v$))))]
  (! mapE parse-num (~! manyp (~! matches?p not-hyphen-or-space))))

(def-thunk (! Range) (! List 'range))
(def-thunk (! rangep)
  (! mapE Range (~! <$ nump (~! exactp #\-)) nump))

(def-thunk (! charp) (! matches?p (~! <<v not 'o equal? #\:)))
(def-thunk (! Policy) (! List 'policy))
(def-thunk (! policyp)
  (! mapE Policy
     (~! <$ rangep spacep)
     (~! <$ charp (~! exactp #\:) (~! exactp #\space))))

(def-thunk (! letterp)
  (! matches?p (~! <<v not 'o equal? #\newline)))

(def-thunk (! passp)
  (! mapE list->string (~! manyp letterp)))

(def-thunk (! linep)
  (! mapE List policyp passp))

(def-thunk (! inputp)
  (! manyp (~! <$ linep (~! exactp #\newline))))


