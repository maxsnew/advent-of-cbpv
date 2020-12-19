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

(def/copat (! paint-color)
  [((= 0)) (ret #\space)]
  [((= 1)) (ret #\*)])

;; Effcode -> F(Natural)
(def-thunk (! intcode-turtle prog c origin)
  (! handle (~! buffer-output (~! effcode prog) 2) error
     (~ (copat
         [('input resume loc)
          [color <- (! c 'read loc)]
          (! resume color loc)]
         [((list 'output color num-turn) resume loc dir painted)
          [painted = (cons loc painted)]
          (! c 'write loc color)
          [lr <- (! LR<-num num-turn)]
          [dir <- (! turn dir lr)]
          [loc <- (! <<v coord-add loc 'o vec<-dir dir '$)]
          (! resume '() loc dir painted)]
         [('halt _ _ _ painted) (ret painted)]))
     origin 'N '()))

(def-thunk (! main-a)
  [syn <- (! parse-intcode-program)]
  [len = 200]
  [len/2 <- (! / len 2)]
  [c <- (! mk-square-canvas len)]
  [origin <- (! mk-coord len/2 len/2)]
  (! displayall 'hi)
  (! cl-foreach displayall (~! c 'paint paint-color))
  [painted <- (! intcode-turtle syn c origin)]
  (! cl-foreach displayall (~! c 'paint paint-color))
  (! <<v displayall 'num-ever-painted: 'o length 'o set->list 'o list->set painted '$))

(def-thunk (! main-b)
  [syn <- (! parse-intcode-program)]
  [len = 80]
  [len/2 <- (! / len 2)]
  [c <- (! mk-square-canvas len)]
  [origin <- (! mk-coord len/2 len/2)]
  (! c 'write origin 1)
  (! cl-foreach displayall (~! c 'paint paint-color))
  (! intcode-turtle syn c origin)
  (! cl-foreach displayall (~! c 'paint paint-color))
  )

     
