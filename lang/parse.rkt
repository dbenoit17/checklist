#lang racket/base

(require parser-tools/lex
         parser-tools/yacc
         racket/string
         racket/format
         racket/port
         racket/match
         "../checklist.rkt")

(provide (all-defined-out))

(define-tokens words (WORD STRING AT-RACKET))
(define-empty-tokens delims
  (newline OCB CCB OSB CSB AT PCT EOF eOCB eCCB eOSB eCSB eAT ePCT))

(struct acc [desc at-e])

(struct racket-obj [syntax])

(define (checklist-pct-exp-list->hash at-list)
  (for/hash ([a at-list])
    (values (string->symbol (checklist-pct-exp-name a))
            (checklist-pct-exp-fields a))))

(define (acc+desc desc a)
  (match a
    [(acc de at-e)
     (acc (cons (checklist-word desc ) de) at-e)]))

(define (format-name n)
  (string-join n " "))

(define (acc+at ex a)
  (match a
    [(acc de at-e)
     (acc (cons ex de)
          (cons ex at-e))]))

(define (unget-c input-port)
  (file-position input-port (- (file-position input-port) 1))
  "")

(define checklist-lexer
  (lexer
    [(eof) 'EOF]
    [#\newline (token-newline)]
    [(union #\tab #\space) (checklist-lexer input-port)]
    ["{" (token-OCB)]
    ["\\{" (token-eOCB)]
    ["}" (token-CCB)]
    ["[" (token-OSB)]
    ["]" (token-CSB)]
    ["@racket" (token-AT-RACKET (chomp-ocb-racket input-port))]
    ["@" (token-AT)]
    ["%" (token-PCT)]
    ["\\]" (token-eCSB)]
    ["\\}" (token-eCCB)]
    ["\\{" (token-eOCB)]
    ["\\[" (token-eOSB)]
    ["\\@" (token-eAT)]
    ["\\%" (token-ePCT)]
    [";" (lex-comment input-port)]
    ["\"" (token-STRING (string-append lexeme (lex-string input-port)))]
    [any-char (token-WORD (string-append lexeme (lex-word input-port)))]))

(define lex-comment
  (lexer
    [(eof) 'EOF]
    [#\newline (checklist-lexer input-port)]
    [any-char (lex-comment input-port)]))

(define lex-word
  (lexer
    [(eof) ""]
    [(union "\\{" "\\}" "\\[" "\\]" "\\@" "\\%")
     (string-append (string-trim lexeme #:left "\\") (lex-word input-port))]
    [(union #\newline #\tab #\space #\\ "{" "}" "[" "]" "@" "%")
      (unget-c input-port)]
    [any-char (string-append lexeme (lex-word input-port))]))

(define chomp-ocb-racket
  (lexer
    [(eof) ""]
    [#\{ (lex-racket input-port)]
    [any-char (chomp-ocb-racket input-port)]))

(define lex-racket
  (lexer
    [(eof) ""]
    [#\} ""]
    [(union "\\{" "\\}")
     (string-append (string-trim lexeme #:left "\\") (lex-racket input-port))]
    [any-char (string-append lexeme (lex-racket input-port))]))

(define lex-string
  (lexer
    [(eof) ""]
    [#\" "\""]
    ["\\n" (string-append "\n" (lex-string input-port))]
    ["\\t" (string-append "\t" (lex-string input-port))]
    [any-char (string-append lexeme (lex-string input-port))]))

(define (read-racket input-port)
  (define result (read input-port))
  (if (eq? result eof)
    '()
    (cons result (read-racket input-port))))

(define checklist-parser
  (parser
    (start start)
    (end EOF)
    (tokens words delims)
    (error (lambda (a b c . d)
             (error "checklist-item parser error: unexpected token:" b c d)))
    (precs
      (left newline))
    (grammar
      (start 
        [() #f]
        [(new-lines start) $2]
        [(entry-list) $1])
      (entry-list
        [(entry) (cons $1 '())]
        [(at-expr) (cons $1 '())]
        [(racket-expr) (cons $1 '())]
        [(racket-expr entry-list) (cons $1 $2)]
        [(entry entry-list) (cons $1 $2)]
        [(at-expr entry-list) (cons $1 $2)])
      (new-lines
        [(newline) "\n"]
        [(newline new-lines) (string-append "\n" $2)])
      (racket-expr
        [(AT-RACKET) (racket-obj (read-racket (open-input-string $1)))]
        [(racket-expr new-lines) $1])
      (at-expr
        [(AT WORD OCB at-data CCB)
         (checklist-at-exp $2 $4)]
        [(AT WORD) (checklist-at-exp $2 '())]
        [(at-expr new-lines) $1])
      (at-data
        [(newline) '()]
        [(WORD) (cons $1 '())]
        [(STRING) (cons $1 '())]
        [(WORD at-data) (cons $1 $2)]
        [(STRING at-data) (cons $1 $2)]
        [(new-lines at-data) $2])
      (entry
        [(OSB CSB name newline desc) 
         (match $5
           [(acc de at)
            (checklist-item (format-name $3) de #f
               (checklist-pct-exp-list->hash at))])]
        [(OSB WORD CSB name newline desc)
         (match $6
           [(acc de at)
            (checklist-item (format-name $4) de #t
               (checklist-pct-exp-list->hash at))])]
        [(OSB CSB name newline) 
          (checklist-item (format-name $3) '() #f
            (checklist-pct-exp-list->hash '()))]
        [(OSB WORD CSB name newline) 
          (checklist-item (format-name $4) '() #t
            (checklist-pct-exp-list->hash '()))]
)
      (name
        [(WORD) (cons $1 '())]
        [(WORD name)(cons $1 $2)]
        [(eOSB name) (cons "[" $2)]
        [(eCSB name) (cons "]" $2)]
        [(AT name) (cons "@" $2)])
      (desc
        [(newline) (acc '() '())]
        [(new-lines desc) (acc+desc $1 $2)]
        [(WORD desc)(acc+desc $1 $2)]
        [(eOSB desc) (acc+desc "[" $2)]
        [(eCSB desc) (acc+desc "]" $2)]
        [(eAT desc) (acc+desc "@" $2)]
        [(pct-expr desc) (acc+at $1 $2)])
      (pct-expr
        [(PCT WORD OCB pct-data CCB)
         (checklist-pct-exp $2 $4)]
        [(PCT WORD) (checklist-pct-exp $2 '())])
      (pct-data
        [(newline) '()]
        [(WORD) (cons $1 '())]
        [(WORD pct-data) (cons $1 $2)]
        [(new-lines pct-data) $2]))))


