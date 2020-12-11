#lang fiddle

(require fiddle/prelude
         fiddle/stdlib/CoList
         fiddle/stdlib/IO
         fiddle/stdlib/Table)

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

(def-thunk (! neighbor-ixs pt)
  (! cl-map (~! add-pt pt) (~! colist<-list neighbor-vecs)))
(def-thunk (! neighbors g pt)
  (! cl-map (~! grid-ref g) (~! neighbor-ixs pt)))

;; Vec w (Vec h Char) -> Listof Pt -> Vec w (Vec h (Listof Pt))
(def-thunk (! neighbors-grid grid seats)
  [gpr <- (! copy-grid grid)]
  [seat-set <- (! table-set<-list seats)]
  [insert-neighbors = (~ (λ (pt)
                           (do [ixs <- (! <<v filter (~! seat-set 'has-key?) 'o list<-colist (~! neighbor-ixs pt))]
                               (! grid-set! gpr pt ixs))))]
  (! cl-foreach insert-neighbors (~! colist<-list seats))
  (ret gpr))

;; The initial version was too slow
;; I have two optimizations in mind that should help
;;
;; 1. We can make a pre-compute a grid of neighbor indices (this is definitely necessary for part b). This should make the loop faster.

;; 2. Rather than just getting a boolean of whether or not anything
;;    changed, we can return the set of seats whose neighbor has
;;    changed. This should vastly speed up the later iterations since
;;    parts of the map stabilize each round.

(def-thunk (! step-life g-neighbors g-from g-to seats)
  ;; update g-to with the new state,
  ;; return #t if you changed
  [live = (~ (λ (changed pt)
               (do 
                   [nearby-seats <- (! grid-ref g-neighbors pt)]
                   (! idiom^ (~! swap append changed)
                      (~ (patc (! grid-ref g-from pt)
                               [#\L ;; alive if 0 occupied neighbors
                                (cond [(! <<v null? 'o filter (~! equal? #\#) 'o map (~! grid-ref g-from) nearby-seats)
                                       (! grid-set! g-to pt #\#)
                                       (ret nearby-seats)]
                                      [else
                                       (! grid-set! g-to pt #\L)
                                       (ret '())])]
                               [#\. (ret '())]
                               [#\# ;; dead if 4+ occupied neighbors
                                (cond [(! <<v <= 4 'o length 'o filter (~! equal? #\#) 'o map (~! grid-ref g-from) nearby-seats)
                                       (! grid-set! g-to pt #\L)
                                       (ret nearby-seats)]
                                      [else
                                       (! grid-set! g-to pt #\#)
                                       (ret '())])]))))))]
  (! CBV (~! cl-foldl (~! colist<-list seats) live '())
     % v> list->set
     % v> set->list
     % v$))

(def-thunk (! life-loop all-seats g-neighbors g-cur g-next seats generation)
  [l <- (! length seats)]
  (! displayall 'generation: generation)
  ;; (! displayall 'seats: l)
  ;; (! display-grid g-cur)
  [generation <- (! + 1 generation)]
  (patc (! step-life g-neighbors g-cur g-next seats)
        ['() (! CBN (~! colist<-list all-seats)
            % n> (~! cl-map (~! grid-ref g-cur))
            % n> (~! cl-filter (~! equal? #\#))
            % n> cl-length
            % n$)]
        [seats (! life-loop all-seats g-neighbors g-next g-cur seats generation)]))

(def-thunk (! main-a f)
  (patc (! idiom^ parse (~! slurp-lines! f)) [(list w h grid)
   [ixs <- (! list<-colist (~! cartesian-product (~! range 0 w) (~! range 0 h)))]
   [seats <- (! <<v filter (~! <<v not 'o equal? #\. 'o grid-ref grid) ixs)]
   [g2 <- (! copy-grid grid)]
   [g-neighbors <- (! neighbors-grid grid seats)]
   ;; (! display-grid g-neighbors)
   (! life-loop seats g-neighbors grid g2 seats 0)]))

(provide main-a)
