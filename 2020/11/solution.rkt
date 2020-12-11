#lang fiddle

(require fiddle/prelude
         fiddle/stdlib/CoList
         fiddle/stdlib/IO
         )

(define neighbor-vecs (list (list +1 +1)
                            (list +1  0)
                            (list +1 -1)
                            (list  0 +1)
                            (list  0 -1)
                            (list -1 +1)
                            (list -1  0)
                            (list -1 -1)))

;; CellState is either
;;   #\# occupied seat
;;   #\L empty    seat
;;   #\. floor

;; String -> F (List w:Nat h:Nat (Vec h (Vec w CellState)))

;; we'll add rows and columns of empty seats around the edge to
;; simplify the problem
(def-thunk (! parse lines-str)
  [lines <- (! map string->list lines-str)]
  [h <- (! <<v + 2 'o length lines)]
  [w <- (! <<v + 2 'o length 'o first lines)]
  [empty-row <- (! <<v list->vector 'o list<-colist (~! repeat #\. w))]
  [mk-row = (~ (! <<v list->vector 'o Cons #\. 'o swap append '(#\.)))]
  [non-empty-rows <- (! map mk-row lines)]
  [v <- (! <<v list->vector 'o Cons empty-row 'o append non-empty-rows (list empty-row))]
  (! List w h v))

(def-thunk (! grid-ref g (list x y))
  [row <- (! vector-ref g y)]
  (! vector-ref row x))

(def-thunk (! grid-set! g (list x y) val)
  [row <- (! vector-ref g y)]
  (! vector-set! row x val))

(def-thunk (! display-grid g)
  (! cl-foreach displayall (~! colist<-vector g)))

(def-thunk (! list<-vector v)
  (! list<-colist (~! colist<-vector v)))

(def-thunk (! copy-vec v)
  (! <<v list->vector 'o list<-vector v))

(def-thunk (! copy-grid g)
  (! <<v list->vector 'o map copy-vec 'o list<-vector g))

(def-thunk (! add-pt (list x1 y1) (list x2 y2))
  (! idiom^ List (~! + x1 x2) (~! + y1 y2)))

(def-thunk (! neighbors g pt)
  (! cl-map (~! <<v grid-ref g 'o add-pt pt) (~! colist<-list neighbor-vecs)))

(def-thunk (! step-life g-from g-to seats)
  ;; update g-to with the new state,
  ;; return #t if you changed
  [live = (~ (λ (b pt)
               (! or (~ (patc (! grid-ref g-from pt)
                          [#\L ;; alive if 0 occupied neighbors
                           (cond [(! <<v null? 'o filter (~! equal? #\#) 'o list<-colist (~! neighbors g-from pt))
                                  (! grid-set! g-to pt #\#)
                                  (ret #t)]
                                 [else
                                  (! grid-set! g-to pt #\L)
                                  (ret #f)])]
                          [#\# ;; dead if 4+ occupied neighbors
                           (cond [(! <<v <= 4 'o length 'o filter (~! equal? #\#) 'o list<-colist (~! neighbors g-from pt))
                                  (! grid-set! g-to pt #\L)
                                  (ret #t)]
                                 [else
                                  (! grid-set! g-to pt #\#)
                                  (ret #f)])]))
                  (~ (ret b)))))]
  (! cl-foldl (~! colist<-list seats) live #f))

(def-thunk (! life-loop g-cur g-next seats generation)
  (! displayall 'generation: generation)
  (! display-grid g-cur)
  [generation <- (! + 1 generation)]
  (cond [(! step-life g-cur g-next seats) (! life-loop g-next g-cur seats generation)]
        [else
         (! CBN (~! colist<-list seats)
            % n> (~! cl-map (~! grid-ref g-cur))
            % n> (~! cl-filter (~! equal? #\#))
            % n> cl-length
            % n$)]))

(def-thunk (! main-a f)
  (patc (! idiom^ parse (~! slurp-lines! f)) [(list w h grid)
   [ixs <- (! list<-colist (~! cartesian-product (~! range 0 w) (~! range 0 h)))]
   [seats <- (! <<v filter (~! <<v not 'o equal? #\. 'o grid-ref grid) ixs)]
   [g2 <- (! copy-grid grid)]
   (! life-loop grid g2 seats 0)]))

(provide main-a)
