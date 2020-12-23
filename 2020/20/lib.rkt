#lang fiddle

(require fiddle/prelude
         fiddle/stdlib/CoList
         )

(def/copat (! invert-rot)
  [('pi/2) (ret 'minus-pi/2)]
  [('minus-pi/2) (ret 'pi/2)]
  [(theta) (ret theta)])

(def/copat (! add-rot)
  [(0 theta) (ret theta)]
  [(theta 0) (ret theta)]
  [('pi 'pi) (ret 0)]
  [('pi 'pi/2) (ret 'minus-pi/2)]
  [('pi 'minus-pi/2) (ret 'pi/2)]
  [(theta 'pi) (! add-rot 'pi theta)]
  [('pi/2 'pi/2) (ret 'pi)]
  [('pi/2 'minus-pi/2) (ret 0)]
  [('minus-pi/2 'minus-pi/2) (ret 'pi)]
  [('minus-pi/2 'pi/2) (ret 0)])

(def/copat (! add-flips)
  [('r f) (ret f)]
  [(f 'r) (ret f)]
  [('l 'l) (ret 'r)])

(def-thunk (! flip-rot f theta)
  (pat f 
    ['l (! invert-rot theta)]
    ['r (ret theta)]))

;; First apply the right symmetry and then the left
;; The flip anti-commutes with rotations
;; l theta = -theta l
(def-thunk (! add-orientation (list theta1 flip1) (list theta2 flip2))
  [theta2 <- (! flip-rot flip1 theta2)]
  [theta <- (! add-rot theta1 theta2)]
  [flip <- (! add-flips flip1 flip2)]
  (ret (list theta flip)))

(def/copat (! top-edge field)
  [((list 0 f))
   [edge <- (! first field)]
   (pat f
        ['r (ret edge)]
        ['l (! reverse edge)])]
  [((list 'pi f))
   [edge <- (! last field)]
   (pat f
        ['l (ret edge)]
        ['r (! reverse edge)])]
  [((list 'pi/2 'r)) (! map last field)]
  [((list 'pi/2 'l)) (! map first field)]
  [((list 'minus-pi/2 'r)) (! <<v reverse 'o map first field)]
  [((list 'minus-pi/2 'l)) (! <<v reverse 'o map last field)])


(def-thunk (! orient-piece o (list id o-id))
  [o <- (! add-orientation o o-id)]
  (ret (list id o)))

(def/copat (! flip-list)
  [('r) (! Ret)]
  [('l) (! reverse)])


(def-thunk (! multi-map f xss)
  [xss~ <- (! map (~ (λ (xs) (ret (~! colist<-list xs)))) xss)]
  (! list<-colist (~! apply (~! cl-map f) xss~)))

(def-thunk (! cols rows) (! multi-map List rows))

;; Orientation -> Listof (Listof String) -> Listof (Listof String)
(def-thunk (! orient-rows o rows)
  (pat o
   [(list 0 'r)                            (ret rows)]
   [(list 0 'l)                  (! map reverse rows)]
   [(list 'pi 'r) (! <<v map reverse 'o reverse rows)]
   [(list 'pi 'l)                    (! reverse rows)]

   [(list 'pi/2 'l)                                     (! cols rows)]
   [(list 'minus-pi/2 'r)            (! <<v map reverse 'o cols rows)]
   [(list 'pi/2 'r)                      (! <<v reverse 'o cols rows)]
   [(list 'minus-pi/2 'l) (! <<v map reverse 'o reverse 'o cols rows)]))

(define! all-orientations
  (! list<-colist (~! cartesian-product
                      (~! colist<-list (list 0 'pi/2 'pi 'minus-pi/2))
                      (~! colist<-list (list 'r 'l)))))


(provide top-edge all-orientations orient-piece orient-rows multi-map)
