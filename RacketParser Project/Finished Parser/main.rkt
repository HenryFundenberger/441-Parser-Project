#lang racket
(require 2htdp/batch-io)
(require "scanner.rkt")



; Match function described to me by classmate
;--------------------------------------------------------------------------------------------------
(define (match tokens token)
  (if (equal? (first tokens) token)
      (rest tokens)
      (list (countE tokens))))

; Used to count number of new line characters to determine error line number
;--------------------------------------------------------------------------------------------------
(define (countE tokens)
  (if (number? (first tokens))
      (first tokens)
      (countEHelper tokens 1)
      )
  )
(define (countEHelper tokens count)
  (if (equal? tokens '())
      count
      (if (equal? (first tokens) "E")
          (countEHelper (rest tokens) (+ count 1))
          (countEHelper (rest tokens) count)
          )
      )
  )



; Parser built off textbook model of a parser
; Programming Language Pragmatics 4th Edition Page 76-77
;--------------------------------------------------------------------------------------------------

(define  (program tokens)
  (define numNewLinesOriginal (countE tokens))

                  (case (first tokens)
                    [(or "id" "read" "write" "end") (begin
                                                     (if (equal? (match (stmt_list tokens) "end") '())
                                                         (display "Accept\n")
                                                         (begin
                                                           (display "Syntax Error on line: ")
                                                           (+(- numNewLinesOriginal (countE (stmt_list tokens)))1))
                                                     ))]
                    [else (display "Syntax Error")]
                    )
)
   
(define (stmt_list tokens)
  (case (first tokens)
    [(or "id" "read" "write") (begin
                                (stmt_list (stmt tokens)))]
    [("E") (begin (stmt_list(match tokens "E")))]
    [("end") tokens]
    [else tokens]
    )  
  )

(define (stmt tokens)
  (case (first tokens)
    [("id") (begin
              (expr(match (match tokens "id") "assign_op")))]
    [("read") (match(match tokens "read") "id")]
    [("write") (expr(match tokens "write"))]
    [else (list (countE tokens))]
    )
  )

(define (expr tokens)
  (case (first tokens)
    [(or "id" "number" "l_paren") (begin
                                    (term_tail (term tokens))
                                    )]
    [else (list (countE tokens))]
    )
  )

(define (term_tail tokens)
  (case (first tokens)
    [(or "add_op") (begin
                     (term_tail(term(add_op tokens)))
                     )]
    [(or "r_paren" "id" "write" "end" "E")(begin
                                        tokens)
                                      ]
    [else (list (countE tokens))]
    )
                                       
 )
 
 
(define (term tokens)
  (case (first tokens)
    [(or "id" "number" "l_paren") (begin
                                    (factor_tail(factor tokens))
                                    )]

    [else (list (countE tokens))]
    )
  )

(define (factor_tail tokens)
  (case (first tokens)
    [(or "mul_op") (begin
                     (factor_tail(factor(mul_op tokens)))
                     )]
    [(or "add_op" "r_paren" "id" "write" "end" "E") (begin
                                                  tokens)
                                                ]
    [else (list (countE tokens))]
    )
  )                             
 
 

(define (factor tokens)
  (case (first tokens)
    [("id") (begin
              (match tokens "id")
              )]
    [("number") (begin
                  (match tokens "number")
                  )]
    [("l_paren") (begin
                   (match (expr(match tokens "l_paren")) "r_paren")
                     )]

    [else (list (countE tokens))]
    ))



(define (add_op tokens)
  (case (first tokens)
    [("add_op") (begin
                  (match tokens "add_op"))
                ]
    [else (list (countE tokens))]
    )
  )


(define (mul_op tokens)
  (case (first tokens)
    [("mul_op") (begin
                   (match tokens "mul_op"))
                 ]
    [else (list (countE tokens))]
    )
  )


;--------------------------------------------------------------------------------------------------


;Parser Call
;--------------------------------------------------------------------------------------------------
(define (parse input)
  (define tokens (list->string(scan(remove-whitespace(tokenize (read-1strings input))))))
  (define programTokens (check-file-end tokens))
  (if (equal? programTokens '())
      (display "Syntax Error, No Input")
      (program programTokens)
      )
  )


