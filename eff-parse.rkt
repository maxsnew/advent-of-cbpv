#lang fiddle

(require fiddle/prelude
         fiddle/stdlib/CoList

         "generic-effects.rkt")

(provide failp choosep
         exactp matches?p manyp betweenp
         anyp
         firstParseAll)

;; Our Parser effect

;; Parser tok = Eff { next : () ~> tok , fail : () ~> 0, choose : () ~> bool }

(def-thunk (! fail) (! raiseE 'fail))
(def-thunk (! failp) (! raiseE 'fail))
(def-thunk (! next)  (! raiseE 'next))
(def-thunk (! anyp)  (! raiseE 'next))
(def-thunk (! choose) (! raiseE 'choose))

(def-thunk (! choose2 t1 t2) (! bindE choose (~ (copat [(#t) (! t1)] [(#f) (! t2)]))))
(def-thunk (! choosep (rest ts))
  (! cl-foldr (~! colist<-list ts) choose2 fail))


(def-thunk (! matches?p p?)
  (! bindE next (~ (λ (x)
                     (cond [(! p? x) (! retE x)]
                           [else (! fail)])))))
(def-thunk (! exactp c)
  (! matches?p (~! equal? c)))

(def-thunk (! betweenp pl p pr)
  (! mapE (~ (λ (l m r) (ret m))) pl p pr))

(def-thunk (! manyp p)
  (! choosep (~! mapE Cons p (~! manyp p))
             (~! retE '())))

;; firstParseAll : Parser tok v -> Listof tok -> F (('yes v) U ('no parse-error))
(def-thunk (! firstParseAll p toks)
  (! handle p
     (~ (copat
         [(x        '()) (! List 'yes x)]
         [(x (cons _ _)) (! List 'no 'unconsumed-tail)]))
     (~ (copat
         [('next resume '()) (! List 'no 'ran-out-of-stuff)]
         [('next resume (cons tok toks)) (! resume tok toks)]
         [('fail resume toks) (! List 'no 'unspecified-parse-error)]
         [('choose resume toks)
          (patc (! resume #t toks)
            [(list 'yes v) (! List 'yes v)]
            [(list 'no _)  (! resume #f toks)])]))
     toks))
