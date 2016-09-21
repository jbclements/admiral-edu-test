#lang racket

(require rackunit
         html-parsing
         sxml
         sexp-diff
         "url-cleanup.rkt"
         "regression-edits.rkt")

;; this is very ad-hoc right now... looking for a way to compare
;; the output of two regression test run outputs.

(define pre-change-tests
  (call-with-input-file "regression-results-pre.rktd"
    (λ (port)
      (let loop ()
        (define r (read port))
        (cond [(eof-object? r) '()]
              [else (cons r (loop))])))))

(define post-change-tests
  (call-with-input-file "/tmp/regression-results-tmp.rktd"
    (λ (port)
      (let loop ()
        (define r (read port))
        (cond [(eof-object? r) '()]
              [else (cons r (loop))])))))

;; extract all links from a page
(define (all-links xexp)
  (match xexp
    ;; <a> with an href:
    [(list 'a (list '@ _1 ... (list 'href l) _2 ...) sub-elts ...)
     (cons l (apply append (map all-links sub-elts)))]
    ;; another tag with attrs:
    [(list tag (list '@ _1 ...) sub-elts ...)
     (apply append (map all-links sub-elts))]
    ;; a tag without attrs:
    [(list tag sub-elts ...)
     (apply append (map all-links sub-elts))]
    ;; not a tag
    [other '()]))

;; LINK EXTRACTION
#;(pretty-print
 (for/list ([test-pre (in-list pre-change-tests)])
   (match test-pre
     [(list i n args (list 'web-server-returned-void _1 ...))
      '()]
     [(list i n args (list code-a code-msg-a ts-a encoding-pre
                           headers-a str-pre))
     
      (list i n args (all-links (html->xexp str-pre)))])))


;; how to compare trees? We're using sxml, but we want to ignore
;; all of the extraneous whitespace that might not affect the
;; equality (e.g. in the <head> element). Unfortunately, this is
;; hard. So, as a non-conservative element, we just eliminate all
;; strings containing only whitespace. This may accidentally make
;; not-equal things into equal things, BUT not in a way that's
;; likely to arise as the result of a bug in our code. We hope.

;; given a single sxml element, remove all substrings that consist
;; entirely of whitespace
(define (sxml-eliminate-ws elt)
  (match elt
    [(cons (? symbol? tag) (cons (cons '@ attrs) rest))
     (cons tag (cons (cons '@ attrs)
                     (sxml-eliminate-ws/subelts rest)))]
    [(cons (? symbol? tag) rest)
     (cons tag (sxml-eliminate-ws/subelts rest))]
    [other other]))

;; factoring out common subportion of sxml-eliminate-ws
(define (sxml-eliminate-ws/subelts elts)
  (special-case-cleanup
   (map dump-trailing-url-slash
   (filter (compose not ws-string?)
           (ws-flatten (map sxml-eliminate-ws elts))))))

;; take care of special cases:
(define (special-case-cleanup elts)
  (drop-leading-spaces
   (match elts
     [(list (? string? str))
      ;; for a single string, trim the spaces on either side
      (list (string-trim str))]
     [other other])))

;; remove trailing slashes from urls
(define (dump-trailing-url-slash elt)
  (match elt
    [(list 'a (list '@ (list 'href url))
           subelts ...)
     `(a (@ (href ,(url-cleanup url))) ,@subelts)]
    [(list 'form (list '@ (list 'action url) other-attrs ...) subelts ...)
     `(form (@ (action ,(url-cleanup url)) ,@other-attrs) ,@subelts)]
    [other other]))


;; give a list of sxml elements,
;; combine adjacent strings, replace all consecutive whitespace
;; with a single space
(define (ws-flatten loe)
  (cond [(empty? loe) loe]
        [else
         (define done-rest (ws-flatten (rest loe)))
         (match (first loe)
           [(? string? s1)
            (match done-rest
              ['() (list s1)]
              [(cons (? string? s2) r2)
               (cons (strip-spaces (string-append s1 s2))
                     r2)]
              [(list 'h2 _ ...)
               (cons (drop-trailing-space (strip-spaces s1))
                     done-rest)]
              [other
               (cons (strip-spaces s1) done-rest)])]
           ;; this could get very ugly...
           [other (cons (first loe) done-rest)])]))

;; drop a leading space from the first element
(define (drop-leading-spaces elts)
  (match elts
    ['() '()]
    [(cons (? string? s) r)
     (cons (drop-leading-space s) r)]
    [other other]))

(define (drop-leading-space s)
  (match s
    [(regexp #px"^ *(.*)" (list _ m)) m]))

;; given a string, replace consecutive
;; whitespace with a single space
(define (strip-spaces a)
  (regexp-replace* #px"[[:space:]]+" a " "))

;; is this a string consisting only of whitespace?
(define (ws-string? s)
  (and (string? s) (regexp-match #px"^[[:space:]]*$" s)))

(check-equal? (strip-spaces " bc\n de \n de")
              " bc de de")

(define (drop-trailing-space str)
  (match str
    [(regexp #px"^(.*) $" (list _ sub)) sub]
    [other other]))

(check-equal? (drop-trailing-space "abc ") "abc")
(check-equal? (drop-trailing-space "abc") "abc")

(check-equal? (sxml-eliminate-ws `(html "\n " " " (head "bc" "  \n")))
              `(html (head "bc")))

(check-equal?
 (sxml-eliminate-ws
  '(*TOP* (html
           "\n" "\n" "  "
           (head "\n" "    "
                 (title " Captain Teach - Assignments ")
                 "\n" "  ")
           "\n" "\n" "\n"
           (body "\n" "  "
                 (h1 "Assignments")
                 "\n"
                 (p (a (@ (href "/test-class/author/")) "New Ass" "ignment"))
                 "\n"
                 (h2 "Open Assignments")
                 "\n"
                 (ul)
                 "\n"
                 (h2 "Closed Assignments")
                 "\n" (ul) "\n") "\n")))
 '(*TOP* (html
          (head (title "Captain Teach - Assignments"))
          (body (h1 "Assignments")
                (p (a (@ (href "/test-class/author")) "New Assignment"))
                (h2 "Open Assignments")
                (ul)
                (h2 "Closed Assignments")
                (ul)))))
;; given two terms and an sxml term, replace every instance of 'from'
;; with 'to', return a *list* of results, to allow flattening and
;; disappearing.
(define (sxml-replace from to sxml)
  (unless (list? to)
    (raise-argument-error 'sxml-replace
                          "list" 1 from to sxml))
  (define replaced
    (let loop ([sxml sxml])
      (cond [(equal? sxml from) to]
            [else
             (match sxml
               [(list tag (list '@ attrs ...) subelts ...)
                (list
                 `(,tag (@ ,@attrs)
                       ,@(apply append (map loop subelts))))]
               [(list tag subelts ...)
                (list `(,tag ,@(apply append (map loop subelts))))]
               [other (list other)])])))
  (match replaced
    [(list val) val]
    [other (error 'sxml-replace
                  "expected list of length one, got: ~e"
                  replaced)]))

(check-equal? (sxml-replace "abc" '("def")
                            '(a (@ (abc "abc") (aaa "d,,hp"))
                                "abc"))
              '(a (@ (abc "abc") (aaa "d,,hp"))
                  "def"))

(check-equal? (sxml-replace "abc" '("def" "ghi")
                            '(a (@ (abc "abc") (aaa "d,,hp"))
                                "abc"))
              '(a (@ (abc "abc") (aaa "d,,hp"))
                  "def" "ghi"))

;; given a name and the old
(define (make-test-edits name sxml)
  (define changes-to-apply
    (filter (λ (change) (eq? (first change) name))
            test-edits))
  (for/fold ([updated sxml])
            ([t (in-list (map second changes-to-apply))])
    (match t
      [(list 'replace-with from to)
       (sxml-replace from to updated)]
      [other
       (error 'bad-pattern
              "bad pattern: ~e\n"
              t)])))

(define tests-to-ignore
  '(bad-new-student ;; old one had different context
    bad-author-post ;; old one crashed
    bad-author-path ;; old one returned <void>
    bad-yaml ;; new one has radically better error text
    ))

(define ignore-encoding-tests
  '(bad-yaml ;; old one returned application/json
    boguspath-validate ;; old one returned application/json
    existing-assignment ;; old one returned application/json
    good-validate ;; old one returned application/json
    ))



(define TESTS-OF-INTEREST '(13))

(for ([test-pre (in-list pre-change-tests)]
      [test-post (in-list post-change-tests)])
  (with-handlers ([exn:fail?
                   (λ (exn)
                     (fprintf (current-error-port)
                              "test meltdown on test: ~e\n"
                              test-pre)
                     #f)])
  
    (match-define (list i _ args (list code-a code-msg-a ts-a encoding-pre
                                       headers-a str-pre))
      test-pre)
    (match-define (list _ n _ (list code-b code-msg-b ts-b encoding-post
                                    headers-b str-post))
      test-post)
    ;; check that the tests are aligned. No point in comparing
    ;; garbage.
    (unless (equal? (list (first test-pre) (third test-pre))
                    (list (first test-post) (third test-post)))
      (error 'test-comparison
             "expected first 2 elements to be the same, got ~e and ~e"
             (take test-pre 2) (take test-post 2)))

    (unless (member n tests-to-ignore)
    
      (test-case
       (~v (list i n args))
       ;; ignore differences in codes; these are
       ;; the subject of correctness tests:
       #;(check-equal? code-b code-a)
       #;(check-equal? code-msg-b code-msg-a)
       ;; ignore timestamp...
       (unless (member n ignore-encoding-tests)
         (check-equal? encoding-post encoding-pre))
       ;; ignore headers...
       #;(check-equal? headers-b headers-a)
       ;; not clear how to parse these...
       (define parsed-post (sxml-eliminate-ws (html->xexp str-post)))
       (define parsed-pre (sxml-eliminate-ws (html->xexp str-pre)))
       (define rewritten-pre (make-test-edits n parsed-pre))

       (when (member i TESTS-OF-INTEREST)
         (printf "diff on test ~v: ~v\n"
                 i
                 (sexp-diff rewritten-pre parsed-post)))
       (check-equal? parsed-post rewritten-pre)))))

