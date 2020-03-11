#lang racket/base

(provide (all-defined-out))

(struct at-exp [name fields] #:transparent)
(struct pct-exp [name fields] #:transparent)
(struct word [str] #:transparent)


(struct stella-task [name desc stat at-vals] #:transparent
  #:guard (λ (n de st av x)
             ;; todo guard for at-vals : hash : (string, listof string)
             (values n de st av)))
(define stella-task-complete? stella-task-stat)
(define stella-task-incomplete?
  (compose not stella-task-complete?))

(define (stella-has-key? st k)
  (hash-has-key? (stella-task-at-vals st) k))

(define (stella-key-ref st k)
  (define at-hash (stella-task-at-vals st))
  (hash-ref at-hash  k))


