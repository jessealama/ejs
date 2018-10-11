#lang info

(define collection "ejs")

(define version "1.0.0")

(define deps
  '("base"
    "rackunit-lib"
    "brag"
    "beautiful-racket-lib"))

(define build-deps
  '("scribble-lib"
    "racket-doc"
    "brag"
    "rackunit-lib"
    "beautiful-racket-lib"))

(define pkg-desc "Exact-precision JSON library")

(define pkg-authors '("jesse@lisp.sh"))
