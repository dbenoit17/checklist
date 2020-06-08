#lang racket/base

(provide (all-defined-out))

(struct checklist-at-exp [name fields] #:transparent)
(struct checklist-pct-exp [name fields] #:transparent)
(struct checklist-word [str] #:transparent)


(struct checklist-item [name desc stat at-vals] #:transparent
  #:guard (λ (n de st av x)
             ;; todo guard for at-vals : hash : (string, listof string)
             (values n de st av)))

(define current-checklist
  (make-parameter (list)))

(define checklist-item-complete? checklist-item-stat)
(define checklist-item-incomplete?
  (compose not checklist-item-complete?))

(define (checklist-has-key? st k)
  (hash-has-key? (checklist-item-at-vals st) k))

(define (checklist-item-key-ref st k)
  (define at-hash (checklist-item-at-vals st))
  (hash-ref at-hash  k))

