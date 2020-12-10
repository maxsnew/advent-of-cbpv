#lang fiddle

(require fiddle/prelude
         fiddle/stdlib/CoList
         "../../regex.rkt")

(provide parse)
;; A line looks like
;; (0-9)*-(0-9)* (a-z): (a-z)*\n


(define! space/lex
  (! idiom^ debug
   (~! idiom^ (~! List (~! abort 'space))
      (~! idiom^ compile-regex (~! char/e "-: \n")))))

(define! lower-case/e (! char/e "abcdefghijklmnopqrstuvwxyz"))

(define! lower-case/lex
  (! idiom^ debug (~! idiom^ (~! List first) (~! compile-regex lower-case/e))))

(define! lower-chars/lex
  (! idiom^ debug (~! idiom^ (~! List list->string) (~! idiom^ compile-regex (~! star/e lower-case/e)))))


(define sig (list space/lex lower-case/lex lower-chars/lex unsigned-number/lex))

(def-thunk (! parse s)
  (! CBN (~! colist<-string s)
     % n> (~! fold-lex sig)
     % n> (~! cl-filter (~! <<v not 'o equal? 'space))
     % n> (~! chunks 4)
     % n> (~! cl-map (~ (copat [((list lo hi c s)) (ret (list (list 'policy (list 'range lo hi) c) s))])))
     % n> list<-colist
     % n$))

;; (define! worde
;;   (! idiom^ (~! mape list->string) (~! stare lower-casee)))

;; (define! policye
;;   (! idiom^ (~! mape (~ (λ (lo _ hi _ c _ _) (ret (list 'policy (list 'range lo hi) c)))))
;;      (~ (ret nume))
;;      (~! chare #\-)
;;      (~ (ret nume))
;;      (~! chare #\space)
;;      (~ (ret lower-casee))
;;      (~! chare #\:)
;;      (~! chare #\space)))

;; (define! linee (! liste policye worde))

;; (define! inpute
;;   (! idiom^ stare (~! idiom^ (~! <$ linee) (~! chare #\newline))))
