#lang racket/base

(require racket/port
         racket/match
         stella/stella
         stella/format
         stella/date)

(provide (all-defined-out))

(define (print-incomplete stellae) 
   (define filtered
     (filter stella-task-incomplete? stellae))
   (print-stella-tasks filtered))

(define (print-complete stellae) 
   (define filtered
     (filter (compose not stella-task-incomplete?) stellae))
   (print-stella-tasks filtered))

(define (stella-sort stellae) 
  (define sorted
    (sort stellae stellae-date<))
  (print-stella-tasks sorted))

(define (tag-started s)
  (define ds (today->date-string))
  (match s
    [(stella-task n d st at) 
     (stella-task n (cons (word (format "%started{~a}\n" ds)) d) st
       (hash-set at "started" (list ds)))]))

(define (stella-auto-stamp stellae)
  (define tagged
    (for/list ([s stellae])
      (if (hash-has-key? (stella-task-at-vals s) 'started )
          s
          (tag-started s))))
   (print-stella-tasks tagged))

(define (stella-reveal stellae) 
  (define tf (today-file))
   (define stellae-t
     (filter stella-task-incomplete? stellae))
   (when (file-exists? tf)
     (raise-user-error "today's file already exists"))
   (with-output-to-file tf
     (λ ()
       (printf "#lang stella\n\n")
       (printf "@require{stella/examples}\n")
       (printf "@stella-reveal{_stellae}\n\n")
       (print-stella-tasks stellae-t))))

