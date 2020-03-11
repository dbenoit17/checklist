#lang racket/base

(require
  racket/match
  racket/file
  racket/port
  racket/format
  racket/string
  stella/date
  "stella.rkt")

(provide (all-defined-out))


(define (indented-line indent line)
  (~a line "\n"
     #:align 'right
     #:min-width (+ (string-length line) indent)))

(define (indent-lines indent str)
  (define lines (string-split str "\n"))
  (string-append*
    (for/list ([l lines])
      (indented-line indent l))))

(define (newline-terminated? str)
  (define len (string-length str))
  (when (<= len 0)
    (error "expected string of non-zero length"))
  (equal? #\newline (string-ref str (- len 1))))

(define (format-at-exp at)
  (match at
    [(at-exp n f)
     (let ([field-str (if (= (length f) 0) ""
                          (format "{~a}" (string-join f " ")))]) 
       (format "@~a~a" n field-str))]))

(define (format-desc-elem e)
  (cond [(word? e) (word-str e)]
    [(pct-exp? e) (format-pct-exp e)]))

(define (format-pct-exp at)
  (match at
    [(pct-exp n f)
     (let ([field-str (if (= (length f) 0) ""
                          (format "{~a}" (string-join f " ")))]) 
       (format "%~a~a" n field-str))]))


(define (format-desc de max-width)
  (define (join-desc max-width cur-width result word-lst)
    (cond 
      [(null? word-lst) result]
      [else
        (define next-word (format-desc-elem (car word-lst)))
        (define len (string-length next-word))
        (cond 
          [(<= (+ cur-width len 1) max-width)
           (join-desc
             max-width
             (+ cur-width len 1)
             (string-append* result 
               (list next-word 
                 (if (newline-terminated? next-word) "" " ")))
             (cdr word-lst))]
          [else
           (join-desc
             max-width
             len
             (string-append* result "\n" (list next-word))
             (cdr word-lst))])]))
  (define desc-string (join-desc max-width 0 "" de))
  (indent-lines 5 desc-string))

(define (format-status st)
  (if st "[x]" "[ ]"))

(define (format-stella-task s)
  (match s
    [(stella-task n ds st av)
     (format "~a ~a\n~a\n" 
       (format-status st)
       n
       (format-desc ds 65))]))

(define (format-stellae stellae)
  (with-output-to-string
    (λ ()
      (for ([s stellae])
        (printf "~a" (format-stella-task s))))))

(define (print-stella-tasks st)
  (printf "~a" (format-stellae st)))

