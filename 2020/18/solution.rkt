#lang fiddle

(require fiddle/prelude
         fiddle/stdlib/CoList
         fiddle/stdlib/Eff
         fiddle/stdlib/Table
         fiddle/stdlib/IO
         "../../regex.rkt"
         "../../eff-parse.rkt")


(define! lit/lexes
  (! map (~ (copat [((list s v))
                    (! char/lex s (~! abort v))]))
     '(("+" +) ("*" *) ("(" lparen) (")" rparen) (" " space))))

(define sig (cons unsigned-number/lex lit/lexes))

(def-thunk (! tokenize f)
  (! CBN  (~! slurp-lines~ f)
     % n> (~! cl-map (~! <<n list<-colist 'o fold-lex-string sig))
     % n> (~! cl-map (~! filter (~! <<v not 'o equal? 'space)))
     % n$))

(def-thunk (! parensp p)
  (! betweenp (~! exactp 'lparen) p (~! exactp 'rparen)))

(def-thunk (! numberp) (! mapE (~! List 'number) (~! matches?p number?)))
(def-thunk (! operatorp)
  (! matches?p (~ (λ (x) (! or (~! equal? '+ x) (~! equal? '* x))))))

(def-thunk (! expp)
  [pexpp = (~! parensp expp)]
  (! mapE Cons
      (~! choosep numberp pexpp)
      (~! choosep (~! mapE Cons operatorp expp) (~! retE '()))))

(def-thunk (! parse toks)
  (! firstParseAll expp toks))

;; An Expression is a
;; Num or (Cons Expression (AltList Op Expression))

(def/copat (! interp-op)
  [('+ m n) (! + m n)]
  [('* m n) (! * m n)])

(def/copat (! interp)
  [('exp (list 'number n)) (ret n)]
  [('exp (cons e1 ops))
   [n <- (! interp 'exp e1)]
   (! interp 'opseq n ops)]
  [('opseq n '()) (ret n)]
  [('opseq n (cons op (cons e opseq)))
   [m <- (! interp 'exp e)]
   [p <- (! interp-op op n m)]
   ;; (! displayall 'nopm=p m op n p)
   (! interp 'opseq p opseq)])

;; This is the version that never makes a non-tail recursive call
(def/copat (! abs-machine)
  [('ret n #:bind) (ret n)]
  [('ret n 'thenops ops) (! abs-machine 'opseq n ops)]
  [('ret n 'flopseq m op opseq)
   [p <- (! interp-op op m n)]
   ;; (! displayall 'nopm=p m op n p)
   (! abs-machine 'opseq p opseq)]

  [('exp (list 'number n)) (! abs-machine 'ret n)]
  [('exp (cons e ops)) (! abs-machine 'exp e 'thenops ops)]

  [('opseq n '())                      (! abs-machine 'exp (list 'number n))]
  [('opseq n (cons op (cons e opseq))) (! abs-machine 'exp e 'flopseq n op opseq)])

(def-thunk (! unyes (list 'yes x)) (ret x))

(def-thunk (! main-a f)
  (! CBN  (~! tokenize f)
     ;; % n> (~! cl-map debug)
     % n> (~! cl-map parse)
     % n> (~! cl-map unyes)
     ;; % n> (~! cl-map debug)
     % n> (~! cl-map (~! abs-machine 'exp))
     % n> (~! cl-map debug)
     % n> (~! cl-foldl^ + 0)
     % n$))


(def/copat (! am-associate)
  [('ret)
   (copat
    [(nf #:bind)            (ret nf)]
    [(nf 'op op e ops)      (! am-associate 'exp e 'flop nf op ops)]
    [(nf2 'flop nf1 op ops) (! am-associate 'opseq nf1 op nf2 ops)]
    [(nf3 'r2op nf1 op1 nf2 op2 ops)
     (pat (list op1 op2)
          [(list '* '+) (! am-associate 'opseq nf2 '+ nf3 ops 'r* nf1)]
          [_            (! am-associate 'opseq (list nf1 op1 nf2) op2 nf3 ops)])]
    [(nf2 'r* nf1) (! am-associate 'ret (cons nf1 (cons '* nf2)))])]
  [('exp)
   (copat
    [((list 'number n))                  (! am-associate 'ret (list 'number n))]
    [((cons e1 (cons op (cons e2 ops)))) (! am-associate 'exp e1 'op op e2 ops)])]  
  
  [('opseq nf1 op nf2)
   (copat
    [('())                     (! am-associate 'ret (list (list nf1 op nf2)))]
    [((cons op2 (cons e ops))) (! am-associate 'exp e 'r2op nf1 op nf2 op2 ops)])])

(def/copat (! associate)
  [('exp (list 'number n)) (ret (list 'number n))]
  [('exp (cons e1 (cons op1 (cons e2 ops))))
   [e1 <- (! associate 'exp e1)]
   [e2 <- (! associate 'exp e2)]
   (! associate 'opseq e1 op1 e2 ops)]
  [('opseq e1 op e2 '()) (ret (list (list e1 op e2)))]
  [('opseq e1 op1 e2 (cons op2 (cons e3 ops)))
   [e3 <- (! associate 'exp e3)]
   (pat (list op1 op2)
        [(list '* '+)
         [e23 <- (! associate 'opseq e2 '+ e3 ops)]
         (ret (cons e1 (cons '* e23)))]
        [_
         (! associate 'opseq (list e1 op1 e2) op2 e3 ops)])])



(def-thunk (! main-b f)
  (! CBN  (~! tokenize f)
     ;; % n> (~! cl-map debug)
     % n> (~! cl-map parse)
     % n> (~! cl-map unyes)
     ;; % n> (~! cl-map debug)
     % n> (~! cl-map (~! am-associate 'exp))
      ;; % n> (~! cl-map debug)
     % n> (~! cl-map (~! abs-machine 'exp))
     % n> (~! cl-map debug)
     % n> (~! cl-foldl^ + 0)
     % n$)
  )

(provide main-a main-b)
