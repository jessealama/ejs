g#lang info

(define collection "ejs")

(define version "0.1.0")

(define deps
  '("base"
    "rackunit-lib"
    "beautiful-racket-lib"))

(define build-deps
  '("scribble-lib"
    "racket-doc"
    "rackunit-lib"
    "beautiful-racket-lib"))

(define pkg-desc "Exact-precision JSON library")

(define pkg-authors '("jesse@lisp.sh"))
