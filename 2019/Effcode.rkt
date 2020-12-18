#lang fiddle

(require fiddle/prelude
         fiddle/stdlib/IO
         fiddle/stdlib/CoList
         fiddle/stdlib/Eff

         "Memory.rkt"
         "Parse.rkt"
         )

(provide effcode parse-intcode-program)

;; PARSING
;
;; An Intcode-Program is a Listof Number

;; Char ->* F Intcode-Program)
(def/copat (! parse-chars)
  [((= 'loop) nums digits (= #\newline))
   [num <- (! <<v parse-num 'o reverse digits '$)]
   (! <<v reverse 'o Cons num nums '$)]
  [((= 'loop) nums digits (= #\,))
   [num <- (! <<v parse-num 'o reverse digits '$)]
   [nums <- (! Cons num nums)]
   (! parse-chars 'loop nums '())]
  [((= 'loop) nums digits c)
   [digits <- (! Cons c digits)]
   (! parse-chars 'loop nums digits)])

;; Parses Intcode program 
(def-thunk (! parse-intcode-program (rest args))
  [chars <- (! <<n list<-colist 'o apply read-all-chars args '$)]
  (! apply (~ (! parse-chars 'loop '() '())) chars))

;; A Parameter-Mode is one of
;;   - 0 , representing position mode
;;   - 1 , representing immediate mode
(define-thunk (! grab-args mem ptr n)
  (cond [(! = n 0) (ret (list ptr '()))]
        [(! = n 1)
         [x <- (! memory-get mem ptr)]
         [ptr <- (! + ptr 1)]
         (ret (list ptr (list x)))
         ]
        [(! = n 2)
         [x <- (! memory-get mem ptr)]
         [ptr <- (! + ptr 1)]
         [y <- (! memory-get mem ptr)]
         [ptr <- (! + ptr 1)]
         (ret (list ptr (list x y)))
         ]
        [(! = n 3)
         [x <- (! memory-get mem ptr)]
         [ptr <- (! + ptr 1)]
         [y <- (! memory-get mem ptr)]
         [ptr <- (! + ptr 1)]
         [z <- (! memory-get mem ptr)]
         [ptr <- (! + ptr 1)]
         (ret (list ptr (list x y z)))]))

;; parse-opcode : Number -> Cons Opcode (Listof Parameter-Modes)
(def-thunk (! parse-opcode n)
  [code <- (! modulo n 100)]
  [modes-num <- (! quotient n 100)]
  [mode1 <- (! modulo modes-num 10)]
  [modes-num <- (! quotient modes-num 10)]
  [mode2 <- (! modulo modes-num 10)]
  [modes-num <- (! quotient modes-num 10)]
  [mode3 <- (! modulo modes-num 10)]
  (ret (list code mode1 mode2 mode3)))

;; read-parameter
;; Memory -> Nat -> Parameter-Mode -> F Nat
(def/copat (! read-parameter mem rbase param)
  [((= 0)) (! memory-get mem param)]
  [((= 1)) (ret param)]
  [((= 2))
   [ptr <- (! + rbase param)]
   (! memory-get mem ptr)]
  [() (! error "read-parameter got an invalid parameter mode")])

(def/copat (! compute-dest rbase ptr)
  [((= 0)) (ret ptr)]
  [((= 2)) (! + ptr rbase)])

(def/copat (! op->num-params)
  [((= 1))  (ret 3)]
  [((= 2))  (ret 3)]
  [((= 3))  (ret 1)]
  [((= 4))  (ret 1)]
  (((= 5))  (ret 2))
  (((= 6))  (ret 2))
  (((= 7))  (ret 3))
  (((= 8))  (ret 3))
  (((= 9))  (ret 1))
  [((= 99)) (ret 0)])

;; An Intcode-Driver I:v O:v R:c is a computation supporting
;;  . 'input  U(I -> U(Intcode-Driver I O R) -> R) -> R
;;  . 'output O -> U(U(Intcode-Driver I O R) -> R) -> R
;;  . 'halt   -> R

(def-thunk (! arith f mem iptr rbase params modes)
  [param1 <- (! first params)] [param2 <- (! second params)] [param3 <- (! third params)] 
  [mode1 <- (! first modes)] [mode2 <- (! second modes)] [mode3 <- (! third modes)]
  [val1 <- (! read-parameter mem rbase param1 mode1)]
  [val2 <- (! read-parameter mem rbase param2 mode2)]
  [dest <- (! compute-dest rbase param3 mode3)]
  [result <- (! f val1 val2)]
  (! memory-set! mem dest result)
  (! retE (list iptr rbase)))

(def-thunk (! jif p? mem iptr rbase params modes)
  [p1 <- (! first params)] [m1 <- (! first modes)]
  [p2 <- (! second params)] [m2 <- (! second modes)]
  [discrim <- (! read-parameter mem rbase p1 m1)]
  [dest <- (! read-parameter mem rbase p2 m2)]     
  [iptr <- (ifc (! p? discrim) (ret iptr) (ret dest))]
  (! retE (list iptr rbase))  )

(def-thunk (! cmp >< mem iptr rbase params modes)
  [p1 <- (! first params)]  [m1 <- (! first modes)]
  [p2 <- (! second params)] [m2 <- (! second modes)]
  [p3 <- (! third params)]  [m3 <- (! third modes)]
  [v1 <- (! read-parameter mem rbase p1 m1)]
  [v2 <- (! read-parameter mem rbase p2 m2)]
  [dest <- (! compute-dest rbase p3 m3)]
  [result <- (ifc (! >< v1 v2) (ret 1) (ret 0))]
  (! memory-set! mem dest result)
  (! retE (list iptr rbase)))

;; Mem -> Ptr -> ? -> Op -> Params -> Modes -> EffCode (List Ptr Base)
(def-thunk (! effcode-step mem iptr rbase op params modes)
  (pat op
   [(= 99) (! raiseE 'halt)]
   [(= 1) ;; + x1 x2 ~> x3
    (! arith + mem iptr rbase params modes)]
   [(= 2) ;; * x1 x2 ~> x3
    (! arith * mem iptr rbase params modes)]
   [(= 3) ;; input ~> x1
    (! bindE (~! raiseE 'input)
       (~ (λ (inp)
            (do ;; (! displayall 'input-received: inp)
                [p1 <- (! first params)] [m1 <- (! first modes)]
                [dest <- (! compute-dest rbase p1 m1)]
                (! memory-set! mem dest inp)
              (! retE (list iptr rbase))))))]
   [(= 4) ;; x1 ~> output
    [param <- (! first params)] [mode <- (! first modes)]
    [outp <- (! read-parameter mem rbase param mode)]
    ;; (! displayall 'output: outp)
    (! bindE (~! raiseE (list 'output outp))
       (~ (λ (_) (! retE (list iptr rbase)))))]
   [(= 5) ;; jump-not-zero
    (! jif zero? mem iptr rbase params modes)]
   [(= 6) ;; jump-if-zero
    (! jif (~! <<v not 'o zero?) mem iptr rbase params modes)]
   [(= 7) ;; <
    (! cmp <  mem iptr rbase params modes)]
   [(= 8) ;; =
    (! cmp = mem iptr rbase params modes)]
   [(= 9) ;; rbase += arg
    [p1 <- (! first params)]  [m1 <- (! first modes)]
    [v1 <- (! read-parameter mem rbase p1 m1)]
    [rbase <- (! + rbase v1)]
    (! retE (list iptr rbase))]))

(def-thunk (! effcode-loop mem iptr rbase)
  [code*modes <- (! <<v parse-opcode 'o memory-get mem iptr '$)]
  [iptr <- (! + iptr 1)]
  [op <- (! first code*modes)]
  [modes <- (! rest code*modes)]
  [n <- (! op->num-params op)]
  (patc (! grab-args mem iptr n)
        [(list iptr params)
         ;; (! displayall 'letsstep iptr rbase op params modes)
         (! appE^ (~! apply (~! effcode-loop mem)) (~! effcode-step mem iptr rbase op params modes))]))

;; A Semantic-Intcode-Prog is a forall R. U(Intcode-Driver I O R) -> R

;; interp-intcode-prog : Intcode-Program -> U(Intcode-Driver Number Number R) -> R
#;
(def-thunk (! interp-intcode-program prog)
  [memory <- (! initialize-memory prog)]
  (! intcode-prog-loop memory 0 0))


;; DRIVERS


;; (Listof Number) -> Number -> Intcode-Driver Number Number (F Number)
;; feeds the list as consecutive inputs
;; returns the last output when it halts, using the second arg as default
#;
(def/copat (! static-input-return-last-output)
  [((= '()) last-output (= 'input)) (! error "ran out of inputs")]
  [(inps last-output (= 'input) k)
   [inp1 <- (! first inps)] [inps <- (! rest inps)]
   (! k inp1 (~ (! static-input-return-last-output inps last-output)))]
  [(inps last-output (= 'output) new-output k)
   (! k (~ (! static-input-return-last-output inps new-output)))]
  [(inps last-output (= 'halt)) (ret last-output)])

;; Effs
;; 'halt : _ ~> 0
;; 'input : _ ~> Int
;; 'output : Int ~> 1

(def-thunk (! effcode prog)
  [mem <- (! initialize-memory prog)]
  (! effcode-loop mem 0 0))
