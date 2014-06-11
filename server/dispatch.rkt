#lang racket
(require web-server/servlet
         web-server/servlet-dispatch
         web-server/web-server
         web-server/dispatch)

(require 
  "cmpsci220/initialize.rkt"
  "auth/google-openidc.rkt"
  "config.rkt"
  "ct-session.rkt"
  "database/mysql.rkt")

;; Resets the database to a fresh configuration
(initialize)

(require "pages/index.rkt"
         (prefix-in review: "pages/review.rkt")
         "pages/errors.rkt")

;; Defines how to process incomming requests are handled
(provide ct-rules)
(define-values (ct-rules mk-url)
  (dispatch-rules
   [("") (dispatch index)]
   [("") #:method "post" (post->dispatch post->index)]
   [("review" (string-arg) (string-arg)) (dispatch-html review:load)]
   [("file-container" (string-arg) (string-arg) (string-arg)) (dispatch-html review:file-container)]
   [((string-arg) ...) handler]
   [else four-oh-four]))

(define (handler req path)
  (let ((session (get-session req))
        (bindings (request-bindings req)))
    (handlerPrime session bindings path)))

(define (handlerPrime session bindings path)
  (match path
    [(list "") (render session index)]
    [(cons "sudo" (cons uid rest)) (with-sudo uid session bindings rest)]
    [else ((lambda ()
            (print path) (newline)
            (response/xexpr `(html (body (p "many things!"))))))]))

(define (with-sudo uid session bindings path)
  (let ((new-session (ct-session (ct-session-class session) uid)))
    (handlerPrime new-session bindings path)))

(define (four-oh-four req)
  (response/xexpr
   '(html (body (p "404")))))

;; Defines how a session is created
;; request -> ct-session
(define (get-session req)
  (ct-session class-name (req->uid req)))

;; Returns #f if the session is not valid
;; otherwise returns a role-record
(define (role session)
  (let* ((class (ct-session-class session))
         (uid (ct-session-uid session))
         (result (role:select class uid)))
    result))

;; If the session has a valid role, renders the specified page. Otherwise,
;; this displays an error message
(define (render session page)
  (let ((valid-role (role session)))
    (response/xexpr
     (if (not valid-role) 
         (error-not-registered session)
         (page session valid-role)))))

(define (render-html session page rest)
  (let ((valid-role (role session)))
    (if (not valid-role)
        (response/xexpr (error-not-registered session))
        (response/full
         200 #"Okay"
         (current-seconds) TEXT/HTML-MIME-TYPE
         empty
         (list (string->bytes/utf-8 (page session valid-role rest)))))))

;; If the session is valid, tries to render the specified page. Othewise,
;; this responds with an invalid session error
(define (dispatch-html page)
  (lambda (req . rest)
    (let ((session (get-session req)))
      (if (eq? session 'invalid-session) 
          (response/xexpr error-invalid-session)
          (render-html session page rest)))))
    
;; If the session is valid, tries to render the specified page. Othewise,
;; this responds with an invalid session error
(define (dispatch page)
  (lambda (req)
    (let ((session (get-session req)))
      (if (eq? session 'invalid-session) 
          (response/xexpr error-invalid-session)
          (render session page)))))

;; If the session has a valid role, renders the specified page with the specified bindings. 
;; Otherwise, this displays an error message
(define (post->render session page bindings)
  (let ((valid-role (role session)))
    (response/xexpr
     (if (not valid-role) 
         (error-not-registered session)
         (page session valid-role bindings)))))

;; If the session is valid, tries to render the specified page with any 
;; request-bindings. Othewise, this responds with an invalid session error.
(define (post->dispatch page)
  (lambda (req)
    (let ((session (get-session req))
          (bindings (request-bindings req)))
      (if (eq? session 'invalid-session)
          (response/xexpr error-invalid-session)
          (post->render session page bindings)))))