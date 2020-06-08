#lang racket/base

(require checklist/checklist
         checklist/format)

(provide (all-defined-out))

(define (print-checklist-items [checklist-items (current-checklist)])
  (printf "~a" (format-checklist-items checklist-items)))

(define (print-checklist-item i)
  (printf "~a" (format-checklist-item i)))

(define (print-checklist-incomplete [checklist-items (current-checklist)])
   (define filtered
     (filter checklist-item-incomplete? checklist-items))
   (print-checklist-items filtered))

(define (print-checklist-complete [checklist-items (current-checklist)])
   (define filtered
     (filter (compose not checklist-item-incomplete?) checklist-items))
   (print-checklist-items filtered))


