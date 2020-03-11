#lang at-exp racket/base

(require parser-tools/lex
         parser-tools/yacc
         racket/string
         racket/format
         racket/port
         racket/match
         "../stella.rkt")

(provide (all-defined-out))

(define-tokens words (WORD STRING))
(define-empty-tokens delims
  (newline OCB CCB OSB CSB AT PCT EOF eOCB eCCB eOSB eCSB eAT ePCT))

(struct acc [desc at-e])

(define (pct-exp-list->hash at-list)
  (for/hash ([a at-list])
    (values (string->symbol (pct-exp-name a))
            (pct-exp-fields a))))

(define (acc+desc desc a)
  (match a
    [(acc de at-e)
     (acc (cons (word desc ) de) at-e)]))

(define (format-name n)
  (string-join n " "))


(define (acc+at ex a)
  (match a
    [(acc de at-e)
     (acc (cons ex de)
          (cons ex at-e))]))

(define stella-lexer
  (lexer
    [(eof) 'EOF]
    [#\newline (token-newline)]
    [(union #\tab #\space) (stella-lexer input-port)]
    ["{" (token-OCB)]
    ["\\{" (token-eOCB)]
    ["}" (token-CCB)]
    ["[" (token-OSB)]
    ["]" (token-CSB)]
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
    [#\newline (stella-lexer input-port)]
    [any-char (lex-comment input-port)]))

(define lex-word
  (lexer
    [(eof) ""]
    [(union "\\{" "\\}" "\\[" "\\]" "\\@" "\\%")
     (string-append (string-trim lexeme #:left "\\") (lex-word input-port))]
    [(union #\newline #\tab #\space #\\ "{" "}" "[" "]" "@" "%")
      (begin
        (file-position input-port (- (file-position input-port) 1))
        "")]
    [any-char (string-append lexeme (lex-word input-port))]))

(define lex-string
  (lexer
    [(eof) ""]
    [#\" "\""]
    ["\\n" (string-append "\n" (lex-string input-port))]
    ["\\t" (string-append "\t" (lex-string input-port))]
    [any-char (string-append lexeme (lex-string input-port))]))

(define stella-parser
  (parser
    (start start)
    (end EOF)
    (tokens words delims)
    (error (lambda (a b c . d)
             (error "stella-task parser error: unexpected token:" b c d)))
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
        [(entry entry-list) (cons $1 $2)]
        [(at-expr entry-list) (cons $1 $2)])
      (new-lines
        [(newline) "\n"]
        [(newline new-lines) (string-append "\n" $2)])
      (at-expr
        [(AT WORD OCB at-data CCB)
         (at-exp $2 $4)]
        [(AT WORD) (at-exp $2 '())]
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
            (stella-task (format-name $3) de #f 
               (pct-exp-list->hash at))])]
        [(OSB WORD CSB name newline desc)
         (match $6
           [(acc de at)
            (stella-task (format-name $4) de #t
               (pct-exp-list->hash at))])])
      (name
        [(WORD) (cons $1 '())]
        [(WORD name)(cons $1 $2)]
        [(eOSB name) (cons "[" $2)]
        [(eCSB name) (cons "]" $2)]
        [(AT name) (cons "@" $2)])
      (desc
        [(newline) (acc '() '())]
        [(WORD desc)(acc+desc $1 $2)]
        [(new-lines desc) (acc+desc $1 $2)]
        [(eOSB desc) (acc+desc "[" $2)]
        [(eCSB desc) (acc+desc "]" $2)]
        [(eAT desc) (acc+desc "@" $2)]
        [(pct-expr desc) (acc+at $1 $2)])
      (pct-expr
        [(PCT WORD OCB pct-data CCB)
         (pct-exp $2 $4)]
        [(PCT WORD) (pct-exp $2 '())])
      (pct-data
        [(newline) '()]
        [(WORD) (cons $1 '())]
        [(WORD pct-data) (cons $1 $2)]
        [(new-lines pct-data) $2]))))


