#lang racket
(require 2htdp/batch-io)



(define (print-list lst)
  (cond [(null? lst) '()]
        [else (display (car lst))
              (newline)
              (print-list (cdr lst))]))




; Look out table = [ " ", "\n", +, -, *, /, (, )]
; Function that takes in a list of strings that are 1 character long
; for loop through the list, combinging the characters into a single string every step
; if the next string is in the look out table, then add the current string to the tokens list
; Add the next string to the tokens list
; return the tokens list
(define (tokenize input)
  (define tokens '())
  (define current "")
  (for ([i (in-range (length input))])
    (define next (list-ref input i))
    (if (member next '(" " "\n" "+" "-" "*" "/" "(" ")" ))
        (begin
          (set! tokens (cons current tokens))
          (set! tokens (cons next tokens))
          (set! current ""))
        (set! current (string-append current next))))
        ; add last token
        (set! tokens (cons current tokens))
  (reverse tokens))



(define (remove-whitespace tokens)
    (filter (lambda (x) (not (member x '(" " "")))) tokens))



(define x (read-1strings "input.txt"))
(remove-whitespace (tokenize x))