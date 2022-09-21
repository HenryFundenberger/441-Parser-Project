#lang racket
(require 2htdp/batch-io)


; Simple function to print every element in the list
; Just for testing purposes durring live running
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
          ; Build string from tokens currently in list
          (set! tokens (cons current tokens))
          (set! tokens (cons next tokens))
          (set! current ""))
        ;  adds newest symbol to current list
        (set! current (string-append current next))))
        ; add last token
        (set! tokens (cons current tokens))
  (reverse tokens))



(define (remove-whitespace tokens)
    (filter (lambda (x) (not (member x '(" " "")))) tokens))



; Scans through list of white space removed tokens
; Calls scan-helper function to find and assign correct token to put in new list
; Puts token from scan-helper into new list that is returned
; Can be updated to only go through one at a time
(define (scan tokens)
    (cond
      [(null? tokens) '()]
      [else
        (cons (scan-helper (car tokens))
            (scan (cdr tokens)))]))
; From Scan function
; Checks to see if string being read in is any of the following knowns
; If it is then return correct token
; If not then check if it's a number
; If it is a number return number
; If it is not a number then return id
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

(define (parse input)
  (define tokens (tokenize (read-1strings input)))
  (define tokens-no-whitespace (remove-whitespace tokens))
  (define tokens-scanned (scan tokens-no-whitespace))
  (display (remove-whitespace (tokenize (read-1strings input))))
  (display "\n")
  tokens-scanned)

