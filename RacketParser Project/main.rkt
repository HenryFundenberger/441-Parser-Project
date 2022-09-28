#lang racket
(require 2htdp/batch-io)
(require "scanner.rkt")

(define (parse input)
  (define tokens (scan(remove-whitespace(tokenize (read-1strings input)))))
  (program tokens)
  )


(define (program tokens)
  ; check if tokens is empty
  (if (null? tokens)
      (error "program: unexpected end of input")
      (let ((stmts (stmt-list tokens)))
        (if (null? (cdr stmts))
            (error "program: unexpected end of input")
            (if (eq? (car (cdr stmts)) '$$)
                (car stmts)
                (error "program: expected $$"))))))