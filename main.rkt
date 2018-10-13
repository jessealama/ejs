#lang racket/base

(provide port->ejsexpr
         string->ejsexpr
         bytes->ejsexpr
         ejsexpr->string
         ejsexpr->bytes
         ejsexpr?
         ejs-array?
         ejs-object?
         ejs-null?
         ejs-number?
         ejs-integer?
         ejs-string?
         ejs-boolean?
         equal-ejsexprs?)

(require racket/contract
         racket/function
         racket/match
         racket/port
         racket/list
         (file "./value.rkt")
         (file "./equal.rkt")
         (file "./util.rkt")
         (only-in (file "./render.rkt")
                  ejsexpr->bytes
                  ejsexpr->string))

(module+ test
  (require rackunit))

(define digits
  (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

(define (digit-char? x)
  (and (char? x)
       (list? (member x digits char=?))))

(define/contract (parse/digits port digits)
  (input-port? (listof digit-char?) . -> . (listof char?))
  (define char (peek-char/error port))
  (cond [(char? char)
         (match char
           [(? digit-char?)
            (parse/digits port
                          (cons (read-char port)
                                digits))]
           [else
            (reverse digits)])]
        [(eof-object? char)
         (reverse digits)]
        [#f
         (error "Invalid UTF-8 byte sequence encountered!")]))

(define/contract (parse/non-negative-number port before-dot-digits)
  (input-port? (listof digit-char?) . -> . (and/c ejs-number?
                                                  (not/c negative?)))
  (define digits (parse/digits port before-dot-digits))
  (define char (peek-char/safe port))
  (cond [(char? char)
         (match char
           [#\.
            (read-char port)
            (define after-decimal (parse/digits port (list)))
            (define formatted
              (format "~a.~a"
                      (list->string digits)
                      (list->string after-decimal)))
            (parameterize ([read-decimal-as-inexact #f])
              (string->number formatted))]
           [else
            (string->number (list->string digits))])]
        [(eof-object? char)
         (string->number (list->string digits))]
        [#f
         (error "Invalid UTF-8 byte sequence encountered!")]))

(define/contract (parse/negative-number port)
  (input-port? . -> . (and/c ejs-number?
                             (not/c positive?)))
  (- (parse/non-negative-number port (list))))

(define/contract (parse/object port properties values)
  (input-port? (listof string?) (listof ejsexpr?) . -> . ejs-object?)
  (consume-whitespace! port)
  (match (peek-char/safe port)
    [#\}
     (read-char port)
     (make-hasheq (map (lambda (p v)
                         (cons (string->symbol p) v))
                       properties
                       values))]
    [else
     (define c1 (read-char port))
     (unless (char=? c1 #\")
       (error (format "While reading an object, we expected to find a \";\"; we got ~a instead." c1)))
     (define property (parse/string port (list)))
     (consume-whitespace! port)
     (define c2 (read-char port))
     (unless (char=? c2 #\:)
       (error (format "While reading an object, we expected to find a \":\"; we got ~a instead." c2)))
     (define value (port->ejsexpr port))
     (consume-whitespace! port)
     (define c3 (read-char/safe port))
     (match c3
       [#f
        (error "While parsing an object, an invalid UTF-8 byte sequence was encountered.")]
       [(? eof-object?)
        (error "While parsing an object, we encountered an unexpected end-of-file.")]
       [#\}
        (make-hasheq (map (lambda (p v)
                            (cons (string->symbol p) v))
                          (cons property properties)
                          (cons value values)))]
       [#\,
        (parse/object port
                      (cons property properties)
                      (cons value values))]
       [else
        (error (format "While parsing an object, we expected to find either \"}\" or \",\"; got \"~a\" instead." c3))])]))

(define/contract (parse/array port values)
  (input-port? (listof ejsexpr?) . -> . ejs-array?)
  (consume-whitespace! port)
  (match (peek-char/safe port)
    [#\]
     (read-char port)
     (reverse values)]
    [else
     (define value (port->ejsexpr port))
     (consume-whitespace! port)
     (define c (read-char/safe port))
     (match c
       [#f
        (error "While parsing an array, an invalid UTF-8 byte sequence was encountered.")]
       [(? eof-object?)
        (error "While parsing an array, we encountered an unexpected end-of-file.")]
       [#\]
        (reverse (cons value values))]
       [#\,
        (parse/array port
                     (cons value values))]
       [else
        (error (format "While parsing an array, we expected to find either \"]\" or \",\"; got \"~a\" instead." c))])])
  )

(define/contract (parse/expecting port previous-char expecting success-value)
  (input-port? (or/c false/c char?) (listof char?) ejsexpr? . -> . ejsexpr?)
  (cond [(empty? expecting)
         success-value]
        [else
         (define e (first expecting))
         (define c (read-char/error port))
         (cond [(eof-object? c)
                (cond ([char? previous-char]
                       (error "Unexpected end of file encountered (just read \"~a\")." previous-char))
                      (else
                       (error "Unexpected end of file encountered.")))]
               [(char? c)
                (unless (char=? c e)
                  (cond [(char? previous-char)
                         (error (format "Unexpected character encountered (read \"~a\" after \"~a\", expecting \"~a\"." c previous-char e))]
                        [else
                         (error (format "Unexpected character encountered (read \"~a\", expecting \"~a\"." c e))]))]
               [(eq? c #f)
                (cond [(char? previous-char)
                       (error "Invalid UTF-8 character encountered (last legal character read was \"~a\")." previous-char)]
                      [else
                       (error "Invalid UTF-8 character encountered.")])])
         (parse/expecting port
                          c
                          (rest expecting)
                          success-value)]))

(define/contract (parse/true port)
  (input-port? . -> . ejs-boolean?)
  (parse/expecting port #f (list #\r #\u #\e) #t))

(define/contract (parse/false port)
  (input-port? . -> . ejs-boolean?)
  (parse/expecting port #f (list #\a #\l #\s #\e) #f))

(define/contract (parse/null port)
  (input-port? . -> . ejs-null?)
  (parse/expecting port #f (list #\u #\l #\l) 'null))

(define/contract (peek-char/error port)
  (input-port? . -> . (or/c eof-object? char? false/c))
  (with-handlers ([exn:fail? (const #f)])
    (peek-char port)))

(define/contract (read-char/error port)
  (input-port? . -> . (or/c eof-object? char? false/c))
  (with-handlers ([exn:fail? (const #f)])
    (read-char port)))

(define/contract (ensure-string chars)
  ((listof char?) . -> . ejs-string?)
  (define s (list->string (reverse chars)))
  (parameterize ([current-input-port (open-input-string s)])
    (port->string)))

(define/contract (parse/string port preceding-chars)
  (input-port? (listof char?) . -> . ejs-string?)
  (define c (read-char/error port))
  (match c
    [(? eof-object?)
     (error "End of input encountered while reading a string!")]
    [#f
     (error "Invalid UTF-8 byte sequence encountered while reading a string!")]
    [#\\
     (define c2 (read-char/safe port))
     (match c2
       [(? eof-object?)
        (error "End of input encountered while reading a string, after having read \"\\\"!")]
       [#f
        (error "Invalid UTF-8 byte sequence encountered while reading a string, after having read \"\\\"!")]
       [#\"
        (define c3 (read-char/safe port))
        (match c3
          [#f
           (error "Invalid UTF-8 byte sequence encountered while reading a string, after having read \"\\\" and \" (double quote)!")]
          [(? eof-object?)
           (ensure-string (cons #\\ preceding-chars))]
          [#\"
           (ensure-string (cons #\" preceding-chars))]
          [else
           (parse/string port (cons c3 (cons #\" preceding-chars)))])]
       [else
        (parse/string port (cons c2 (cons #\\ preceding-chars)))])]
    [#\"
     (ensure-string preceding-chars)]
    [else
     (parse/string port (cons c preceding-chars))]))

(define whitespace-chars
  (list #\newline
        #\tab
        #\space))

(define (whitespace? x)
  (list? (member x whitespace-chars char=?)))

(define (consume-whitespace! port)
  (define c (peek-char/safe port))
  (cond [(whitespace? c)
         (read-char port)
         (add1 (consume-whitespace! port))]
        [else
         0]))

(define/contract (port->ejsexpr port)
  (input-port? . -> . ejsexpr?)
  (consume-whitespace! port)
  (define c (read-char/safe port))
  (match c
    [#f
     (error "After consuming whitespace, read an invalid UTF-8 character!")]
    [(? eof-object?)
     (error "After consuming whitespace, encountered end-of-file!")]
    [#\n
     (parse/null port)]
    [#\"
     (parse/string port (list))]
    [#\{
     (parse/object port (list) (list))]
    [#\t
     (parse/true port)]
    [#\f
     (parse/false port)]
    [#\[
     (parse/array port (list))]
    [#\-
     (parse/negative-number port)]
    [(? digit-char?)
     (parse/non-negative-number port (list c))]
    [else
     (error (format "Unknown character: ~a" c))]))

(define/contract (string->ejsexpr str)
  (string? . -> . ejsexpr?)
  (call-with-input-string str port->ejsexpr))

(define/contract (bytes->ejsexpr bstr)
  (bytes? . -> . ejsexpr?)
  (call-with-input-bytes bstr port->ejsexpr))

(module+ test
  (check-equal? "hi!"
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
  (check-true (equal-ejsexprs?
               (hasheq)
               (string->ejsexpr "{}")))
  (check-equal? (list)
                (string->ejsexpr " [ ] "))
  (check-true (equal-ejsexprs?
               (list 4 1987654321/1000000000)
               (string->ejsexpr "[4 , 1.987654321]")))
  (check-true (equal-ejsexprs?
               (hasheq 'hi 'null)
               (string->ejsexpr "{ \"hi\": null}")))
  (check-true (equal-ejsexprs?
               (hasheq 'hi "there!")
               (string->ejsexpr "{\"hi\": \"there!\"}")))
  (check-equal? "what is \"this\"?"
                (string->ejsexpr "\"what is \\\"this\\\"?\""))
  (check-equal? "slash\\"
                (string->ejsexpr "\"slash\\\""))
  (check-equal? "\\"
                (string->ejsexpr "\"\\\""))
  (check-equal? "\\\\"
                (string->ejsexpr "\"\\\\\""))
  (check-equal? "\""
                (string->ejsexpr "\"\\\"\"")))

;; tests copied from argo:
(module+ test
  (define geo/str #<<SCHEMA
{
    "id": "http://json-schema.org/geo",
    "$schema": "http://json-schema.org/draft-06/schema#",
    "description": "A geographical coordinate",
    "type": "object",
    "properties": {
        "latitude": { "type": "number" },
        "longitude": { "type": "number" }
    }
}
SCHEMA
  )
)

(module+ test
  (check-true (ejs-object? (string->ejsexpr geo/str))))

;; another test from argo:

(module+ test
  (define additionalProperties/str #<<SCHEMA
{
    "additionalItems": []
}
SCHEMA
  )
)

(module+ test
  (check-true (ejs-object? (string->ejsexpr additionalProperties/str))))

(module+ test
  (define non-unique/str #<<SCHEMA
{
    "enum": [ {}, {} ]
}
SCHEMA
  )
)

(module+ test
  (check-true (ejs-object? (string->ejsexpr non-unique/str))))
