#lang racket/base

(require racket/date
         racket/match
         racket/string
         stella/stella)

(provide (all-defined-out))

(define (invert-hash h)
  (define ks (hash-keys h))
  (define vs (hash-values h))
  (make-hash (map cons  vs ks)))

(define months
  (hash   1 'Jan    2 'Feb    3 'Mar    4 'Apr
          5 'May    6 'Jun    7 'Jul    8 'Aug
          9 'Sep    10 'Oct   11 'Nov   12 'Dec))

(define (stella-task-due s)
  (define av (stella-task-at-vals s))
  (define due-fields 
    (if (hash-has-key? av 'due)
        (hash-ref av 'due)
        'never))
  (when (not (eq? 1  (length due-fields)))
    (error "@due: expected one argument"))
  (car due-fields))

(define inverted-months
  (invert-hash months))

(define (seconds->date-string seconds)
  (match-define (date* _ _ _ d m y _ _ _ _ _ _)
    (seconds->date seconds))
  (format "~a-~a-~a" y (hash-ref months m) d))

(define (today->date-string)
  (seconds->date-string (current-seconds)))

(define (yesterday->date-string)
  (seconds->date-string (- (current-seconds) 86400)))

(define (date-string->filename ds)
  (format "~a.st" ds))

(define (today-file)
  (date-string->filename (today->date-string)))

(define (yesterday-file)
  (date-string->filename (yesterday->date-string)))

(define (date-str->seconds str)
  (define-values (_y _m _d)
    (apply values (string-split str "-")))
  (define-values (y maybe-m d)
    (apply values (map string->number (list _y _m _d))))
  ;; accept month as numeric string or month abbreviation
  (define m (if maybe-m maybe-m
              (hash-ref inverted-months (string->symbol _m))))
  (define seconds
    (date->seconds
      ;; last 4 args ignored by date->seconds
      (date 0 0 0 d m y 0 0 #f 0)))
  seconds)
  
(define (stellae-date< d0 d1)
  (< (date-str->seconds (stella-task-due d0))
     (date-str->seconds (stella-task-due d1))))
