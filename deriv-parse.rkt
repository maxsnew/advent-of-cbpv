#lang fiddle

(require fiddle/prelude)

;; A ParseExp Tok Tree is
;;   'empty       : ParseExp Tok A
;;   'epsilon     : ParseExp Tok '()
;;   (list 'char tok) : ParseExp Tok Tok
;;   (list 'alt (ParseExp Tok A) (ParseExp Tok A))
;;   (list 'cat (ParseExp Tok A) (ParseExp Tok B)) : ParseExp Tok (Cons A B)
;;   (list 'star (ParseExp Tok A)) : ParseExp Tok (Listof A)
;;   (list 'map U(A -> F B) (ParseExp Tok A)) : ParseExp Tok B

(def/copat (! null-accepting?)
  [('empty)   (ret #f)]
  [('epsilon) (ret #t)]
  [((list 'char c)) (ret #f)]
  [((list 'alt e1 e2)) (! or  (~! null-accepting? e1) (~! null-accepting? e2))]
  [((list 'cat e1 e2)) (! and (~! null-accepting? e1) (~! null-accepting? e2))]
  [((list 'star e)) (ret #t)])

;; cons c cs matches e iff cs matches deriv c e
(def/copat (! Deriv c)
  [('empty)   (ret 'empty)]
  [('epsilon) (ret 'empty)]
  [((list 'char (= c))) (ret 'epsilon)]
  [((list 'char d))     (ret 'empty)]
  [((list 'alt e1 e2)) (! idiom^ (~! List 'alt) (~! Deriv c e1) (~! Deriv c e2))]
  [((list 'cat e1 e2))
   [de1/dc*e2 <- (! idiom^ (~! List 'cat) (~! Deriv c e1) (~ (ret e2)))]
   (cond [(! null-accepting? e1)
          (! idiom^ (~! List 'alt de1/dc*e2) (~! Deriv c e2))]
         [else (ret de1/dc*e2)])]
  [((list 'star e))
   (! idiom^ (~! List 'cat) (~! Deriv c e) (~ (ret (list 'star e))))])

;; matches? : ParseExp Tok Tree -> Listof Tok -> F Bool
(def/copat (! matches?)
  [(e '()) (! null-accepting? e)]
  [(e (cons c cs))
   [de/dc <- (! Deriv c e)]
   (! matches? de/dc cs)])

