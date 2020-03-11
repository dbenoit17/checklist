#lang at-exp racket/base

(require racket/string
         syntax/strip-context
         stella/stella
         stella/lang/parse)

(provide (rename-out [stella-read read]
                     [stella-read-syntax read-syntax]))

(define (stella-read in) 
  (stella-read-syntax #f in))

(define (unpack req)
  (define len (string-length req))
  (cond [(or (< len 2) (not (equal? (string-ref req 0) #\")))
         (string->symbol req)]
        [(string-trim req "\"")]))

(define (build-require-spec lst)
  (define reqs
    (for/list ([r lst])
      (unpack r)))
  (cons 'require reqs))

(define (build-expressions lst)
  (cons 'begin
    (for/list ([e lst])
      (cons (unpack (at-exp-name e)) (map unpack (at-exp-fields e))))))

(define (is-require? node)
  (and (at-exp? node) (string=? (at-exp-name node) "require")))

(define (stella-read-syntax src in)
  (define ast-complete
    (stella-parser (λ () (stella-lexer in))))
  
  (define all-requires
    (filter is-require? ast-complete))
  
  (define ast-reqs-lifted
    (filter (compose not is-require?) ast-complete))

  (define stellae  (filter stella-task? ast-reqs-lifted))
  (define at-exps (filter at-exp? ast-reqs-lifted))
  
  (define user-requires
    (build-require-spec
        (apply append (map at-exp-fields all-requires))))

 (with-syntax ([stellae stellae]
               [user-requires user-requires]
               [expressions (build-expressions at-exps)])
  (strip-context
  #'(module stella-task racket/base
          #;(provide #%stellae at-exps)
          user-requires
          (define _stellae 'stellae)
          expressions))))

