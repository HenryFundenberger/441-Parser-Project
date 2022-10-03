#lang racket
(require 2htdp/batch-io)


; Converts everything in a list to a string
;--------------------------------------------------------------------------------------------------
(define (list->string lst)
  (cond
    [(null? lst) '()]
    [else (cons (symbol->string (car lst)) (list->string (cdr lst)))]))
(provide list->string)


; Removes all white space from list
;--------------------------------------------------------------------------------------------------
(define (remove-whitespace tokens)
    (filter (lambda (x) (not (member x '(" " "")))) tokens))
(provide remove-whitespace)



; Scan call function from parser
;--------------------------------------------------------------------------------------------------
(define (scan tokens)
    (cond
      [(null? tokens) '()]
      [else
        (cons (scan-helper (car tokens))
            (scan (cdr tokens)))]))
(provide scan)


(define (scan-helper token)
    (cond
    [(string=? token "read") 'read]
    [(string=? token "write") 'write]
    [(string=? token "+") 'add_op]
    [(string=? token "-") 'add_op]
    [(string=? token "*") 'mul_op]
    [(string=? token "/") 'mul_op]
    [(string=? token "(") 'l_paren]
    [(string=? token ")") 'r_paren]
    [(string=? token ":=") 'assign_op]
    [(string=? token "\n") 'E]
    [(string=? token "$$") 'end]
    [(number? (string->number token)) 'number]
    [else 'id]))
(provide scan-helper)
;--------------------------------------------------------------------------------------------------

; Tokenizer
;--------------------------------------------------------------------------------------------------
(define (tokenize input)
  (define (tokenize-helper input current tokens)
    (cond [(null? input)
    (reverse (cons current tokens))]
          [(string=? (car input) " ") (tokenize-helper (cdr input) "" (cons current tokens))]
          [(member (car input) '( "" "\n" "+" "-" "*" "/" "(" ")" "$$" )) (tokenize-helper (cdr input) "" (cons (car input) (cons current tokens)))]
          [(string=? (car input) " ") (tokenize-helper (cdr input) "" (cons current tokens))]
          [(member (car input) '( "" "\n" "+" "-" "*" "/" "(" ")" "$$" )) (tokenize-helper (cdr input) "" (cons (car input) (cons current tokens)))]
          [else (tokenize-helper (cdr input) (string-append current (car input)) tokens)]))
  (tokenize-helper input "" '()))
(provide tokenize)
;--------------------------------------------------------------------------------------------------

; Removes anything after first set of $$, as everything past the end of file marker should be ignored
;--------------------------------------------------------------------------------------------------
(define (check-file-end lst)
  (cond
    [(null? lst) '()]
    [(string=? (car lst) "end") (cons (car lst) '())]
    [else (cons (car lst) (check-file-end (cdr lst)))]))
(provide check-file-end)
