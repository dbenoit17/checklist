#lang racket/base

(require racket/string
         syntax/strip-context
         checklist/checklist
         checklist/lang/parse)

(provide (rename-out [checklist-read read]
                     [checklist-read-syntax read-syntax]))

(define (checklist-read in) 
  (checklist-read-syntax #f in))

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
      (if (racket-obj? e) (cons 'begin (racket-obj-syntax e))
        (cons (unpack (checklist-at-exp-name e)) (map unpack (checklist-at-exp-fields e)))))))

(define (is-require? node)
  (and (checklist-at-exp? node) (string=? (checklist-at-exp-name node) "require")))

(define (checklist-read-syntax src in)
  (define ast-complete
    (checklist-parser (λ () (checklist-lexer in))))
  
  (define all-requires
    (filter is-require? ast-complete))

  (define ast-reqs-lifted
    (filter (compose not is-require?) ast-complete))

  (define checklist-items
    (filter checklist-item? ast-reqs-lifted))

  (define at-exps
    (filter 
      (λ (x) (or (checklist-at-exp? x) (racket-obj? x)))
      ast-reqs-lifted))
  
  (define user-requires
    (build-require-spec
        (apply append (map checklist-at-exp-fields all-requires))))

 (with-syntax ([checklist-items checklist-items]
               [current-checklist current-checklist]
               [user-requires user-requires]
               [expressions (build-expressions at-exps)])

  (strip-context
  #'(module checklist racket/base
          (require checklist) 
          (provide #%checklist)
          user-requires
          (define #%checklist 'checklist-items)
          (parameterize ([current-checklist 'checklist-items])
            expressions
            (void))))))

