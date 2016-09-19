#lang setup/infotab

(define collection 'multi)

(define deps
  (list "base"
        "admiral-edu"))
(define build-deps
  (list "db-lib"
        "html-parsing"
        "math-lib"
        "rackunit-lib"
        "sexp-diff"
        "sxml"
        "typed-racket-lib"
        "typed-racket-more"
        "web-server-lib"
        "yaml"
        "quickcheck"))

(define build-implies
  (list "admiral-edu"))

(define pkg-desc "tests for admiral edu")




