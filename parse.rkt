#lang racket/base

(provide port->ejsexprs)

(require (only-in racket/port
                  call-with-input-string)
         racket/contract
         (file "grammar.rkt")
         (file "tokenizer.rkt")
         (file "evaluator.rkt")
         (file "value.rkt"))

(module+ test
  (require rackunit))

(define/contract (port->ejsexprs port)
  (input-port? . -> . (listof ejsexpr?))
  (define parsed (parse (make-tokenizer port)))
  (evaluate (syntax->datum parsed)))

(define/contract (port->ejsexpr port)
  (input-port? . -> . ejsexpr?)
  (define parsed (parse (make-tokenizer port)))
  (evaluate/1 (syntax->datum parsed)))

(define/contract (string->ejsexprs str)
  (string? . -> . (listof ejsexpr?))
  (call-with-input-string str port->ejsexprs))

(define/contract (string->ejsexpr str)
  (string? . -> . ejsexpr?)
  (call-with-input-string str port->ejsexpr))

(module+ test
  (check-equal? "\"hi!\""
                (string->ejsexpr "\"hi!\""))
  (check-equal? 'null
                (string->ejsexpr "null"))
  (check-equal? #t
                (string->ejsexpr "true"))
  (check-equal? #f
                (string->ejsexpr "false"))
  (check-equal? 4
                (string->ejsexpr "4"))
  (check-equal? #e4.5
                (string->ejsexpr "4.5"))
  (check-equal? #e-3.1415926
                (string->ejsexpr "-3.1415926"))
  (check-equal? #e1.000000000000000000000000000003
                (string->ejsexpr "1.000000000000000000000000000003"))
  (check-equal? #e-1.000000000000000000000000000003
                (string->ejsexpr "-1.000000000000000000000000000003"))
  (check-equal? (list)
                (string->ejsexpr "[]"))
  (check-equal? (list)
                (string->ejsexpr " [ ] "))
  (check-equal? (hasheq 'hi 'null)
                (string->ejsexpr "{ \"hi\": null}")))
