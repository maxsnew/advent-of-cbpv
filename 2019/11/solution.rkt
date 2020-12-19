#lang fiddle

(require fiddle/prelude)
(require fiddle/stdlib/IO)
(require fiddle/stdlib/CoList)
(require fiddle/stdlib/Eff)
(require "../Parse.rkt")
(require "../Effcode.rkt")
(require "../Coordinates.rkt")

(provide main-a main-b)


;; 0 means "black"
;; 1 means "white"

;; A dir is one of 'N 'S 'E 'W
;; A turn is one of 'L 'R

(def/copat (! vec<-dir)
  [((= 'N)) (! mk-coord  0 -1)]
  [((= 'S)) (! mk-coord  0  1)]
  [((= 'E)) (! mk-coord  1  0)]
  [((= 'W)) (! mk-coord -1  0)])

(def/copat (! turn)
  [((= 'N) (= 'L)) (ret 'W)]
  [((= 'N) (= 'R)) (ret 'E)]
  [((= 'S) (= 'L)) (ret 'E)]
  [((= 'S) (= 'R)) (ret 'W)]
  [((= 'E) (= 'L)) (ret 'N)]
  [((= 'E) (= 'R)) (ret 'S)]
  [((= 'W) (= 'L)) (ret 'S)]
  [((= 'W) (= 'R)) (ret 'N)])

(def/copat (! LR<-num)
  [((= 0)) (ret 'L)]
  [((= 1)) (ret 'R)])

(def/copat (! a-driver canvas loc dir painted)
  [((= 'input) k)
   [color <- (! canvas 'read loc)]
   (! k color (~ (! a-driver canvas loc dir painted)))]
  [((= 'output) color k)
   ;; (! displayall 'paint loc color)
   [painted <- (! Cons loc painted)]
   (! canvas 'write loc color)
   (! k (~ (copat
   [((= 'output) num-turn k)
    [lr <- (! LR<-num num-turn)]
    [dir <- (! turn dir lr)]
    [loc <- (! <<v coord-add loc 'o vec<-dir dir '$)]
    (! k (~ (! a-driver canvas loc dir painted)))])))]
  [((= 'halt))
   (! <<v List 'num-ever-painted: 'o length 'o set->list 'o list->set painted '$)])

(def-thunk (! handle-a t canvas loc dir painted)
  (! shallow-handle t error (~ (copat
    [('input resume)
     [color <- (! canvas 'read loc)]
     (! handle-a (~! resume color) canvas loc dir painted)]
    [((list 'output color) resume)
     [painted = (cons loc painted)]
     (! canvas 'write loc color)
     (! shallow-handle (~! resume '()) error (~ (copat
     [((list 'output num-turn) resume)
     [lr <- (! LR<-num num-turn)]
     [dir <- (! turn dir lr)]
     [loc <- (! <<v coord-add loc 'o vec<-dir dir '$)]
     (! handle-a (~! resume '()) canvas loc dir painted)])))]
    [('halt _)
     (! <<v displayall 'num-ever-painted: 'o length 'o set->list 'o list->set painted '$)]))))

(def-thunk (! main-a)
  [syn <- (! parse-intcode-program)]
  [len = 200]
  [len/2 <- (! / len 2)]
  [c <- (! mk-square-canvas len)]
  [origin <- (! mk-coord len/2 len/2)]
  (! displayall 'hi)
  (! handle-a (~! effcode syn) c origin 'N '()))

(def/copat (! paint-color)
  [((= 0)) (ret #\space)]
  [((= 1)) (ret #\*)])

(def/copat (! b-driver canvas loc dir)
  [((= 'input) k)
   [color <- (! canvas 'read loc)]
   (! k color (~ (! b-driver canvas loc dir)))]
  [((= 'output) color k)
   (! canvas 'write loc color)
   (! k (~ (copat
   [((= 'output) num-turn k)
    [lr <- (! LR<-num num-turn)]
    [dir <- (! turn dir lr)]
    [loc <- (! <<v coord-add loc 'o vec<-dir dir '$)]
    (! k (~ (! b-driver canvas loc dir)))])))]
  [((= 'halt))
   (! canvas 'paint paint-color)])

(def-thunk (! handle-b t canvas loc dir)
  (! shallow-handle t error (~ (copat
  [((= 'input) resume)
   [color <- (! canvas 'read loc)]
   (! handle-b (~! resume color) canvas loc dir)]
  [((list (= 'output) color) resume)
   (! canvas 'write loc color)
   (! shallow-handle (~! resume '()) error (~ (copat
   [((list 'output num-turn) resume)
    [lr <- (! LR<-num num-turn)]
    [dir <- (! turn dir lr)]
    [loc <- (! <<v coord-add loc 'o vec<-dir dir '$)]
    (! handle-b (~! resume '()) canvas loc dir)])))]
  [('halt _)
   (! canvas 'paint paint-color)]))))

(def-thunk (! main-b)
  [syn <- (! parse-intcode-program)]
  [len = 80]
  [len/2 <- (! / len 2)]
  [c <- (! mk-square-canvas len)]
  [origin <- (! mk-coord len/2 len/2)]
  (! c 'write origin 1)
  (! cl-foreach displayall (~! handle-b (~! effcode syn) c origin 'N)))

     
