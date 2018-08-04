#lang racket/base

(provide peek-char/safe
         read-char/safe)

(require racket/contract
         racket/function
         racket/port)

(define/contract (peek-char/safe port)
  (input-port? . -> . (or/c eof-object? char? false/c))
  (with-handlers ([exn:fail? (const #f)])
    (peek-char port)))

(define/contract (read-char/safe port)
  (input-port? . -> . (or/c eof-object? char? false/c))
  (with-handlers ([exn:fail? (const #f)])
    (read-char port)))
