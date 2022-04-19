#lang racket

;;;; Some macros which are useful
;;;

(provide with-open-input-file
         --> -->*)

(define-syntax-rule (with-open-input-file (in file) form ...)
  (call-with-input-file file
    (λ (in) form ...)))

(define-syntax -->
  (syntax-rules ()
    [(_ v)
     v]
    [(_ v f)
     (f v)]
    [(_ v f ... g)
     (g (--> v f ...))]))

(define-syntax -->*
  (syntax-rules ()
    [(_ (v ...))
     (values v ...)]
    [(_ (v ...) f)
     (call-with-values (thunk (values v ...)) f)]
    [(_ (v ...) f ... g)
     (call-with-values (thunk (-->* (v ...) f ...)) g)]))
