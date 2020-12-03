#lang fiddle

(require fiddle/prelude
         fiddle/stdlib/CoList)

;; A ParseExp Tok Tree is
;;   'empty       : ParseExp Tok A
;;   'epsilon     : ParseExp Tok '()
;;   (list 'epsilon (listof A)) : ParseExp Tok A
;;   (list 'char tok) : ParseExp Tok Tok
;;   (list 'alt (ParseExp Tok A) (ParseExp Tok A))
;;   (list 'cat (ParseExp Tok A) (ParseExp Tok B)) : ParseExp Tok (Cons A B)
;;   (list 'star (ParseExp Tok A)) : ParseExp Tok (Listof A)
;;   (list 'map U(A -> F B) (ParseExp Tok A)) : ParseExp Tok B
;;   (list 'intersect-eps (ParseExp Tok A)) : ParseExp Tok A

(def/copat (! null-accepting?)
  [('empty)   (ret #f)]
  [('epsilon) (ret #t)]
  [((list 'char c)) (ret #f)]
  [((list 'alt e1 e2)) (! or  (~! null-accepting? e1) (~! null-accepting? e2))]
  [((list 'cat e1 e2)) (! and (~! null-accepting? e1) (~! null-accepting? e2))]
  [((list 'star e)) (ret #t)]
  [((list 'intersect-eps e)) (! null-accepting? e)])

;; cons c cs matches e iff cs matches deriv c e
(def/copat (! Deriv c)
  [('empty)   (ret 'empty)]
  [('epsilon) (ret 'empty)]
  [((list 'intersect-eps e)) (ret 'empty)]
  [((list 'char (= c))) (ret 'epsilon)]
  [((list 'char d))     (ret 'empty)]
  [((list 'alt e1 e2)) (! idiom^ (~! List 'alt) (~! Deriv c e1) (~! Deriv c e2))]
  [((list 'cat e1 e2))
   (! idiom^ (~! List 'alt)
      (~! idiom^ (~! List 'cat) (~! Deriv c e1) (~ (ret e2)))
      (~! idiom^ (~! List 'cat) (~ (ret (list 'intersect-eps e1))) (~! Deriv c e2)))]
  [((list 'star e))
   (! idiom^ (~! List 'cat) (~! Deriv c e) (~ (ret (list 'star e))))])

;; matches? : ParseExp Tok Tree -> Listof Tok -> F Bool
(def/copat (! matches?)
  [(e '()) (! null-accepting? e)]
  [(e (cons c cs))
   [de/dc <- (! Deriv c e)]
   (! matches? de/dc cs)])

;; Normalizing constructors

(def-thunk (! emptye) (ret 'empty))
(def/copat (! epsilone)
  [(#:bind) (ret (list 'epsilon '()))]
  [((rest xs) #:bind) (ret (list 'epsilon xs))])

(def-thunk (! char c) (ret (list 'char c)))

(def/copat (! alte)
  [('empty e) (ret e)]
  [(e 'empty) (ret e)]
  [(e1 e2) (ret (list 'alt e1 e2))])

(def-thunk (! alte/s s)
  [cs <- (! string->list s)]
  (! cl-foldl (~! colist<-list cs) (~ (λ (e c) (! alte e (list 'char c)))) 'empty))

(def-thunk (! altchar s)
  [cs <- (! string->list)]
  (! cl-foldl (~! colist<-list cs) (~ (λ (e c) (! alte e (list 'char c)))) 'empty))

(def/copat (! cate)
  [('empty p) (ret 'empty)]
  [(p 'empty) (ret 'empty)]
  [((list 'epsilon (list x)) p)
   (ret (list 'map (~! Cons x) p))]
  [(p (list 'epsilon (list x)))
   (ret (list 'map (~ (λ (y) (! Cons y x))) p))]
  [(p1 p2) (ret (list 'cat p1 p2))])

(def/copat (! mape f)
  [((list 'epsilon xs))
   [ys <- (! map f xs)]
   (! List 'epsilon ys)]
  ;; [((list 'cat (list 'epsilon (list x)) p))
  ;;  ]
  [((list 'map g p))
   (! List 'map (~ (λ (x) (do [y <- (! g x)] (! f y)))) p)]
  [(p) (! List 'map f p)])

(def-thunk (! stringe s)
  [cs <- (! string->list s)]
  [lse <- (! cl-foldl (~! colist<-list cs) (~ (λ (e c) (! cate (list 'char c) e))) (list 'epsilon '()))]
  (! mape list->string lse))

(def/copat (! intersect-eps)
  [((list 'epsilon xs)) (ret (list 'epsilon xs))]
  [((list 'intersect-eps e)) (ret (list 'intersect-eps e))]
  )

(def/copat (! stare)
  [('empty) (! epsilone '())]
  [(p) (ret (list 'star p))])

(def-thunk (! cart-prod l1 l2)
  (! list<-colist
     (~! cl-map (~ (copat [((list x y)) (! Cons x y)])) (~! cartesian-product (~! colist<-list l1) (~! colist<-list l2)))))

;; ParseExp Tok Tree -> F (Listof Tree)
(def/copat (! parse-null)
  [('empty) (ret '())]
  [('epsilon) (ret '(()))]
  [((list 'epsilon xs)) (ret xs)]
  [((list 'char c)) (ret '())]
  [((list 'alt e1 e2))
   (! idiom^ append (~! parse-null e1) (~! parse-null e2))]
  [((list 'cat e1 e2))
   (! idiom^ cart-prod (~! parse-null e1) (~! parse-null e2))]
  [((list 'star e)) (ret '(()))]
  [((list 'map f e)) (! idiom^ (~! map f) (~! parse-null e))]
  [((list 'intersect-eps e)) (! parse-null e)])

(def/copat (! pDeriv c)
  [('empty)   (ret 'empty)]
  [('epsilon) (ret 'empty)]
  [((list 'epsilon x)) (ret 'empty)]
  [((list 'char (= c))) (! epsilone c)]
  [((list 'char d)) (ret 'empty)]
  [((list 'alt e1 e2)) (! idiom^ alte (~! pDeriv c e1) (~! pDeriv c e2))]
  [((list 'cat e1 e2))
   (! idiom^ alte
      (~! idiom^ cate (~! pDeriv c e1) (~ (ret e2)))
      (~! idiom^ cate (~ (ret (list 'intersect-eps e1))) (~! pDeriv c e2)))]
  [((list 'star e)) (! idiom^ cate (~! pDeriv c e) (~ (ret (list 'star e))))]
  [((list 'map f e)) (! idiom^ (~! mape f) (~! pDeriv c e))]
  [((list 'intersect-eps e)) (ret 'empty)])

(def/copat (! parse p)
  [('())
   ;; (! displayall 'mt p)
   (! parse-null p)]
  [((cons c cs))
   ;; (! displayall 'char c p)
   [dp/dc <- (! pDeriv c p)]
   (! parse dp/dc cs)])


(define! digite (! alte/s "0123456789"))
