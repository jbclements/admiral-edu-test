#lang racket/base

;; this file provides a function that cleans up urls.

(require net/url
         racket/match)

(provide url-cleanup)

(require rackunit)

;; resolve '..'s in paths, when possible, and
;; eliminate trailing slashes.
(define (url-cleanup url-str)
  (define url-url (string->url url-str))
  (url->string
   (replace-path
    url-url
    (path-cleanup
     (url-path url-url)))))

(define (replace-path url path)
  (make-url (url-scheme url)
            (url-user url)
            (url-host url)
            (url-port url)
            (url-path-absolute? url)
            path
            (url-query url)
            (url-fragment url)))

(define (path-cleanup pathparams)
  (match pathparams
    ['() '()]
    ;; dump trailing slash
    [(list (path/param "" '())) '()]
    [(cons f r)
     (match (path-cleanup r)
       ['() (cons f '())]
       [(cons f2 r2)
        (match f2
          [(path/param 'up '()) r2]
          [other (cons f (cons f2 r2))])])]))

(url-cleanup "http://ffo.com/abc/../def/")

(check-equal?
 (url-cleanup "http://ffo.com/abc/../def/")
 "http://ffo.com/def")