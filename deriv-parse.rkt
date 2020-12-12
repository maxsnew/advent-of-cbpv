#lang fiddle

(require fiddle/prelude

         fiddle/stdlib/CoList
         fiddle/stdlib/Table

         "generic-effects.rkt")

(provide parse
         emptye epsilone chare
         alte
         cate liste
         stare
         mape

         digite nume lower-casee)

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

(def/copat (! size-loop acc)
  [((cons x y))
   [acc <- (! + 1 acc)]
   (! size-loop acc x y)]
  [(x) (! size-loop acc)]
  [(#:bind) (ret acc)])

(def-thunk (! size x #:bind) (! size-loop 0 x))

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

(def-thunk (! chare c) (ret (list 'char c)))

(def/copat (! alt2e)
  [('empty e) (ret e)]
  [(e 'empty) (ret e)]
  [(e1 e2) (ret (list 'alt e1 e2))])

(def/copat (! cate)
  [('empty p) (ret 'empty)]
  [(p 'empty) (ret 'empty)]
  [((list 'epsilon (list x)) p)
   (ret (list 'map (~! Cons x) p))]
  [(p (list 'epsilon (list x)))
   (ret (list 'map (~ (λ (y) (! Cons y x))) p))]
  [(p1 p2) (ret (list 'cat p1 p2))])

(def/copat (! map1e f)
  [('empty) (ret 'empty)]
  [((list 'epsilon xs))
   [ys <- (! map f xs)]
   (! List 'epsilon ys)]
  ;; [((list 'cat (list 'epsilon (list x)) p))
  ;;  ]
  [((list 'map g p))
   (! List 'map (~ (λ (x) (do [y <- (! g x)] (! f y)))) p)]
  [(p) (! List 'map f p)])

(def/copat (! intersect-eps)
  [((list 'epsilon xs)) (ret (list 'epsilon xs))]
  [((list 'intersect-eps e)) (ret (list 'intersect-eps e))]
  [(e) (ret (list 'intersect-eps e))])

(def/copat (! stare)
  [('empty) (! epsilone '())]
  [(p) (ret (list 'star p))])

(def-thunk (! cart-prod l1 l2)
  (! list<-colist
     (~! cl-map (~ (copat [((list x y)) (! Cons x y)])) (~! cartesian-product (~! colist<-list l1) (~! colist<-list l2)))))

;; memo : Exp ~> ID
;; stored? : ID char ~> ('yes Exp) U ('no Exp)
;; store   : ID char Exp ~> '()

(def-thunk (! memo e)    (! raiseE (list 'memo e)))
(def-thunk (! stored? id c)   (! raiseE (list 'stored? id c)))
(def-thunk (! store   id c e) (! raiseE (list 'store id c e)))

;; ParseExp Tok Tree -> F (Listof Tree)
(def/copat (! parse-null)
  [('empty) (! retE '())]
  [('epsilon) (! retE '(()))]
  [((list 'epsilon xs)) (! retE xs)]
  [((list 'char c)) (! retE '())]
  [((list 'alt e1 e2))
   (! mapE append (~! parse-null e1) (~! parse-null e2))]
  [((list 'cat e1 e2))
   (! mapE cart-prod (~! parse-null e1) (~! parse-null e2))]
  [((list 'star e))  (! retE '(()))]
  [((list 'map f e)) (! mapE (~! map f) (~! parse-null e))]
  [((list 'intersect-eps e)) (! parse-null e)]
  [((list 'memo id))
   (! bindE (~! stored? id #f) (~ (copat [((list 'no e))
      (! parse-null e)])))])

(def/copat (! pDeriv c)
  [('empty)   (! retE 'empty)]
  [('epsilon) (! retE 'empty)]
  [((list 'epsilon x)) (! retE 'empty)]
  [((list 'char (= c))) (! mapE (~! epsilone c))]
  [((list 'char d)) (! retE 'empty)]
  [((list 'alt e1 e2)) (! mapE alt2e (~! pDeriv c e1) (~! pDeriv c e2))]
  [((list 'cat e1 e2))
   (! mapE alt2e
      (~! mapE cate (~! pDeriv c e1)                   (~! retE e2))
      (~! mapE cate (~! mapE (~! intersect-eps e1)) (~! pDeriv c e2)))]
  [((list 'star e))
   (! mapE cate (~! pDeriv c e) (~! retE (list 'star e)))]
  [((list 'memo id))
   (! bindE (~! stored? id c)
      (~ (copat
          [((list 'yes de/dc))
           ;; (! displayall 'yes de/dc)
           (! retE de/dc)]
          [((list 'no  e))
           (! bindE (~! pDeriv c e) (~ (λ (de/dc)
           (! bindE (~! store id c de/dc) (~ (λ (_)
           (! retE de/dc)))))))])))]
  [((list 'map f e)) (! mapE (~! map1e f) (~! pDeriv c e))]
  [((list 'intersect-eps e)) (! retE 'empty)])

(def/copat (! populate-table)
  [((list 'alt e1 e2))
   (! mapE alt2e (~! populate-table e1) (~! populate-table e2))]
  [((list 'cat e1 e2))
   (! mapE cate (~! memo e1) (~! memo e2))]
  [((list 'star e)) (! mapE stare (~! memo e))]
  [((list 'map f e)) (! mapE (~! map1e f) (~! populate-table e))]
  [((list 'intersect-eps e)) (! mapE (~! intersect-eps) (~! populate-table e))]
  [(e) (! retE e)])

(def/copat (! parse-loop p)
  [('())
   (! displayall 'mt p)
   [sz <- (! size p)]
   (! displayall 'psize: sz)
   (! parse-null p)]
  [((cons c cs))
   (! displayall 'char c p)
   [sz <- (! size p)]
   (! displayall 'psize: sz)
   (! appE^ (~! swap parse-loop cs)
            (~! pDeriv c p))])

(def-thunk (! parse p xs)
  (! stateAlg
     (list 0 empty-table)
     (~ (copat
        [((list 'memo e) (list next tbl))
         [tbl <- (! tbl 'set next (list e empty-table))]
         [next+1 <- (! + 1 next)]
         (! List (list 'memo next) (list next+1 tbl))]
        [((list 'stored? id c) (list next tbl))
         (patc (! tbl 'get id #f)
           [(list e e-tbl)
            (patc (! e-tbl 'get c #f)
              [#f     (! List (list 'no e)      (list next tbl))]
              [de/dc (! List (list 'yes de/dc) (list next tbl))])])]
        [((list 'store id c de/dc) (list next tbl))
         (patc (! tbl 'get id #f)
           [(list e e-tbl)
            [e-tbl <- (! e-tbl 'set c de/dc)]
            [tbl <- (! tbl 'set id (list e e-tbl))]
            (! List '() (list next tbl))])]))
     (~! appE^ (~! swap parse-loop xs) (~! populate-table p))))

;; The real shit

(def-thunk (! alte (rest ps))
  (! cl-foldr (~! colist<-list ps)
     (~ (copat [(p tp) [lp <- (! tp)]
                (! alt2e p lp)]))
     (~! emptye)))

(def-thunk (! liste (rest ps))
  (! cl-foldr (~! colist<-list ps)
     (~ (copat [(p tp) [lp <- (! tp)]
                (! cate p lp)]))
     (~! epsilone '())))

(def-thunk (! mape f (rest ps))
  (! idiom^ (~! map1e (~! apply f)) (~! apply liste ps)))

;; (def-thunk (! stringe s)
;;   [cs <- (! string->list s)]
;;   [lse <- (! cl-foldl (~! colist<-list cs) (~ (λ (e c) (! cate (list 'char c) e))) (list 'epsilon '()))]
;;   (! map1e list->string lse))

(define! digite
  (do [mk-digite = (~ (copat [((list n c)) (! idiom^ (~! mape (~! const n)) (~! chare c))]))]
      [digites <- (! map mk-digite '((0 #\0) (1 #\1) (2 #\2) (3 #\3) (4 #\4) (5 #\5) (6 #\6) (7 #\7) (8 #\8) (9 #\9)))]
    (! apply alte digites)))

(def-thunk (! num<-digits ds)
  (! cl-foldl (~! colist<-list ds)
     (~ (copat [(acc x)
               (! idiom^ (~! + x) (~! * 10 acc))]))
     0))

(define! nume
  (! idiom^ (~! mape num<-digits) (~! stare digite)))

(define! lower-casee
  (do [ls <- (! string->list "abcdefghijklmnopqrstuvwxyz")]
      [lowercasees <- (! map chare ls)]
      (! apply alte lowercasees)))
(define! digit-letter
  (! liste digite lower-casee))

(define! dlddllld
  (! liste digite lower-casee digite digite lower-casee lower-casee lower-casee digite))

