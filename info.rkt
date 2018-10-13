#lang info

(define collection "ejs")

(define version "0.13.0")

(define deps
  '("base"
    "rackunit-lib"))

(define build-deps
  '("scribble-lib"
    "racket-doc"
    "rackunit-lib"))

(define pkg-desc "Exact-precision JSON library")

(define pkg-authors '("jesse@lisp.sh"))
