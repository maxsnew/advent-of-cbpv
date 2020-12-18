#lang fiddle

(require fiddle/prelude)
(require fiddle/stdlib/IO)
(require fiddle/stdlib/CoList)
(require fiddle/stdlib/Eff)
(require "../Parse.rkt")
(require "../Effcode.rkt")

(provide main-a main-b)

(def-thunk (! remove-loop sigil prev xs)
  [x <- (! first xs)] [xs <- (! rest xs)]
  (cond [(! equal? x sigil) (! append prev xs)]
        [else [prev <- (! Cons x prev)]
              (! remove-loop sigil prev xs)]))

(def-thunk (! remove x xs) (! remove-loop x '() xs))

;; Listof A -> CoList (List A (Listof A))
;; lazily generate all pairs of an element of a list and the list with that element removed
(def-thunk (! plucks xs)
  [with-removed
    = (~ (λ (x) (! <<v List x 'o remove x xs '$)))]
  (! cl-map with-removed (~ (! colist<-list xs))))

;; Listof A -> CoList (Listof A)
;; lazily generate all of the different permutations of a list
(def/copat (! permutations)
  [((= '())) (! cl-cons '() cl-nil)]
  [(xs)
   (! cl-bind (~ (! plucks xs))
      (~ (λ (x*xs)
           (do [x <- (! first x*xs)] [xs <- (! second x*xs)]
             (! cl-map (~ (! Cons x)) (~ (! permutations xs)))))))])

(def/copat (! run-intcode-flow prog input)
  [((= '()) #:bind) (ret input)]
  [(phase-settings #:bind)
   [cur-setting <- (! first phase-settings)]
   [phase-settings <- (! rest phase-settings)]
   [cur-inputs <- (! List cur-setting input)]
   
   [output <- (! handle (~! effcode prog) error
                 (~ (copat
                     [('input resume (cons inp inps)) (! resume inp inps)]
                     [((list 'output o) resume _) (ret o)]))
                 cur-inputs)]
   (! run-intcode-flow prog output phase-settings)])

(def-thunk (! main-a)
  [prog <- (! parse-intcode-program)]
  [combos = (~ (! permutations '(0 1 2 3 4)))]
  (! <<n (~! <<v displayall 'max-thruster-signal: 'o maximum) 'o cl-map (~ (! run-intcode-flow prog 0)) combos '$))

(def-thunk (! shallow-handle0 t k)
  (! shallow-handle t (~! error "you weren't supposed to return but you did ~v") k))

;; Thread := U(Int -> Eff { Output >> Input U Halt })
;; HaltingThread := U(Int -> Eff { Output >> Halt })
;; 'halt-loop : Int -> Listof HaltingThread -> F Int
(def/copat (! feedback-loop)
  ;; 'input-loop : Int -> Listof Thread -> Listof Thread -> F Int
  [((= 'input-loop) inp old-threads (cons thread threads))
   (! shallow-handle0 (~! thread inp) (~ (copat
   [((list 'output o) resume)
   (! shallow-handle0 (~! resume '()) (~ (copat
     [('halt _)
      (cond [(! empty? old-threads) (! feedback-loop 'halt-loop o threads)]
            [else (! error 'protocol-violation!)])]
     [('input thread)
      (! feedback-loop 'input-loop o (cons thread old-threads) threads)])))])))]

  [((= 'input-loop) inp old-threads '()) ;; end of the line, start it over
   [old-threads <- (! reverse old-threads)]
   (! feedback-loop 'init inp old-threads)]

  [((= 'halt-loop) inp (cons thread threads))
   (! shallow-handle0 (~! thread inp) (~ (copat
   [((list 'output o) resume)
   (! shallow-handle0 (~! resume '()) (~ (copat
   [('halt _)
    (! feedback-loop 'halt-loop o threads)])))])))]

  [((= 'halt-loop) inp '()) (ret inp)]
  
  [('init inp threads) (! feedback-loop 'input-loop inp '() threads)])

(def-thunk (! one-input t inp)
  (! shallow-handle0 t (~ (copat
  [('input resume)
   (! shallow-handle0 (~! resume inp) (~ (copat
  [('input thread) (ret thread)])))]))))

(def-thunk (! main-b)
  [prog <- (! parse-intcode-program)]
  [t = (~! effcode prog)]
  [combos = (~ (! permutations ))]
  (! CBN  (~! permutations '(5 6 7 8 9))
     % n> (~! cl-map (~ (do (! displayall 'launching) (! debug))))
     % n> (~! cl-map (~! <<v feedback-loop 'init 0 'o map (~! one-input t)))
     % n> (~! <<v displayall 'maxsignal: 'o maximum)
     % n$
     ))
