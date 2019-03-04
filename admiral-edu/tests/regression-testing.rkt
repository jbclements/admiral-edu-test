#lang racket/base

;; this file is a mix of regression and correctness testing. It
;; contains sequences of requests to make to captain teach, modeling
;; various possible interactions. In general, these tests do not
;; try to specify the full web page that is the result of the
;; request. Instead, they test the response code, and may also contain
;; other assertions about the result. Most commonly, they try to
;; check that certain links are present.

;; there are also a few tests that look for specific XSS attacks, by
;; checking to see whether the given tag can be made to appear in
;; the output.

(require racket/string
         racket/list
         racket/match
         rackunit
         rackunit/text-ui
         web-server/http/response-structs
         admiral-edu/dispatch
         admiral-edu/base
         "testing-shim.rkt"
         "testing-support.rkt"
         "html-testing-support.rkt"
         "the-tests.rkt")

(define REGRESSION-FILE-PATH-PERSISTENT
  (string-append "/tmp/regression-results-"(number->string (current-seconds))".rktd"))
;; this one gets overwritten every time
(define REGRESSION-FILE-PATH-TEMP
  (string-append "/tmp/regression-results-tmp.rktd"))


(define (ensure-trailing-slash candidate)
  (let ((len (string-length candidate)))
    (cond [(= 0 len) "/"]
          [else (let ((last-char (string-ref candidate (- len 1))))
                  (cond [(eq? #\/ last-char) candidate]
                        [else (string-append candidate "/")]))])))

(define (explode-response r)
  (cond
    [(response? r)
     (list (response-code r)
           (response-message r)
           (response-seconds r)
           (response-mime r)
           (response-headers r)
           (let ([os (open-output-string)])
             ((response-output r) os)
             (get-output-string os)))]
    ;; need to special-case this, because #<void> can't be
    ;; read by 'read'
    [(void? r)
     (list 'web-server-returned-void 'void-value)]
    [else
     (list 'not-a-response-at-all r)]))

;; we probably only care about the bytes in the case
;; of JSON arguments
(define (spec->bytes binding-spec)
  (match binding-spec
    [(list 'json (? bytes? s))
     s]
    [other #""]))

  
(define (run-request user path [binding-spec '()] [post? #f] [post-data-given #""])
  ;; a shortcut to avoid having to write 'alist everywhere
  (define spec (match binding-spec
                 [(cons (or 'multipart 'json 'alist) _) binding-spec]
                 [other (list 'alist other)]))
  [define bindings (spec->bindings spec)]
  (define raw-bindings (spec->raw-bindings spec))
  (define post-data-from-bindings (spec->bytes spec))
  ;; one or the other but not both...
  (define post-data
    (cond [(equal? post-data-given #"") post-data-from-bindings]
          [(equal? post-data-from-bindings #"") post-data-given]
          [else (error 'run-request "post data from bindings and optional arg: ~e and ~e"
                       post-data-from-bindings post-data-given)]))
  ;; really, this should be happening inside the tested code, not out here....
  (define start-rel-url (ensure-trailing-slash (string-append "/" (class-name-shim) "/" (string-join path "/"))))
  (define session (ct-session-shim (class-name-shim) user #f (make-table start-rel-url bindings)))
  (define result (with-handlers ([(λ (x) #t) server-error-shim])
                   (handlerPrime post? post-data session bindings raw-bindings path)))
  (explode-response result))

;; FIXME make these tags instead of a separate list
  ;; some tests are known not to pass on the old version. Run the code and
  ;; log the output, but don't signal an error on stderr
  (define known-bad-in-original
    '((first-tests
       bad-new-student
       bad-author-post
       bad-author-path
       bad-yaml
       existing-assignment
       boguspath-validate
       bad-review-upload
       not-open-yet
       stranger-feedback
       assignment-description-xss
       assignment-description-xss-2
       stranger-submit
       see-others-file
       bogus-review
       bogus-file-container
       stu1-submits-feedback-xss
       accidental-trainwreck)))

  

  ;; these tests generate output that is expected to contain the string
  ;; "&lt;", and should not be fed to the no-double-encode test
  (define known-to-contain-double-encode
    '(bad-yaml
      assignment-description-xss
      assignment-description-xss-2
      stu1-not-yet-published
      stu1-submits-feedback-xss
      stu3-not-yet-published))

;; port port -> string (listof testspec) -> suite
(define ((tests->suite-maker r-port rt-port) suite-name tests)
  (test-suite
   suite-name

   ;; delete everything in the database
   (init-shim master-user-name)

   ;; delete local files
   (delete-local-files-shim)
  
   (let ((result (initialize)))
     (when (Failure? result)
       (error (format "Could not initialize system: ~a\n"))))

   ;; check that no two tests have the same name
   (check-false
    (check-duplicates (apply
                       append
                       (map (λ (t) (match t
                                     [(list a b c) (list c)]
                                     [other '()]))
                            tests))))
  
   (for ([test-or-thunk (in-list tests)]
         [i (in-naturals)])
     (define test
       (cond [(procedure? test-or-thunk) (test-or-thunk)]
             [else test-or-thunk]))
     (define-values (expected request-args-or-thunk testname)
       (match test
         [(list call expected) (values expected call #f)]
         [(list call expected name) (values expected call name)]))
     (define request-args
       ;; !@#$ request hashes... can't extract until earlier tests have been
       ;; run.
       (cond [(procedure? request-args-or-thunk) (request-args-or-thunk)]
             [else request-args-or-thunk]))
     (define result (apply run-request request-args))
     (unless (and ignore-bad-in-original?
                  (member testname known-bad-in-original))
       (test-case
        (format "~s" (list i testname request-args))
        (match expected
          [(? number? code)
           (check-equal? (first result) code)]
          [(list (? number? code)
                 (? procedure? test-proc))
           (begin (check-equal? (first result) code)
                  (test-proc result))])
        (unless (member testname known-to-contain-double-encode)
          (check-pred no-double-encode (sixth result)))))
     (define output-val (list i testname request-args result))
     (fprintf r-port "~s\n" output-val)
     (fprintf rt-port "~s\n" output-val)
     #;(printf "~s\n" output-val))))

(module+ test

  (define r-port (open-output-file REGRESSION-FILE-PATH-PERSISTENT))
  (define rt-port (open-output-file REGRESSION-FILE-PATH-TEMP
                                    #:exists 'truncate))
 
  (define tests->suite (tests->suite-maker r-port rt-port))

  (for ([name-and-testseq (in-list names-and-testseqs)])
    (match name-and-testseq
      [(list name testseq)
       (run-tests (tests->suite name testseq))]))

  (close-output-port r-port)
  (close-output-port rt-port)
)