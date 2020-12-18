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
;; Num or OpSeq

;; An OpSeq is a
;; Cons Expression (AltList Operator Expression)

(def/copat (! interp-op)
  [('+ m n) (! + m n)]
  [('* m n) (! * m n)])

(def/copat (! interp)
  [('exp (list 'number n)) (ret n)]
  [('exp e) (! interp 'opseq e)]
  [('opseq (cons (list 'number n) '())) (ret n)]
  [('opseq (cons (list 'number n) (cons op (cons e opseq))))
   [m <- (! interp 'exp e)]
   [n <- (! interp-op op n m)]
   (! interp 'opseq (cons (list 'number n) opseq))]
  [('opseq (cons e opseq))
   [n <- (! interp 'exp e)]
   (! interp 'opseq (cons (list 'number n) opseq))]
  )

(def-thunk (! unyes (list 'yes x)) (ret x))

(def-thunk (! main-a f)
  (! CBN  (~! tokenize f)
     ;; % n> (~! cl-map debug)
     % n> (~! cl-map parse)
     % n> (~! cl-map unyes)
     ;; % n> (~! cl-map debug)
     % n> (~! cl-map (~! interp 'exp))
     % n> (~! cl-map debug)
     % n> (~! cl-foldl^ + 0)
     % n$))

(def-thunk (! main-b f)
  (! error 'nyi)
  )

(provide main-a main-b)
