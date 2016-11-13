
#lang racket

(require racket/runtime-path
         rackunit
         "testing-shim.rkt"
         "testing-support.rkt"
         "testing-back-doors.rkt"
         "html-testing-support.rkt")


(provide
 (contract-out
  [names-and-testseqs (listof (list/c string? (listof ct-test/c)))]
  [master-user-name string?]))

(define-runtime-path HERE ".")

;; what is a test? These contracts are essentially documentation.

;; represents a request: (user path-strs [bindings] [post?] [raw-bytes?])
(define ct-request-spec/c
  (cons/c string?
          (cons/c (listof string?)
                  (or/c null?
                        (cons/c binding-spec/c
                                (or/c null?
                                      (cons/c boolean?
                                              (or/c null?
                                                    (list/c bytes?)))))))))


(define response-code? (or/c 200 403 400 404 500))
(define ct-expected?
  (or/c response-code?
        (list/c response-code? procedure?)))
(define ct-test?
  (or/c (list/c ct-request-spec/c ct-expected?)
        (list/c ct-request-spec/c ct-expected? symbol?)))
(define ct-test/c
  (or/c ct-test?
        (-> ct-test?)))

;; quick hack to speed test case entry: replace slashes with spaces, turn into list:
(define (path2list p)
  (regexp-split #px"/" p))


;; return the last pending review for given student on "test-with-html"
(define (lastreview-test-with-html uid)
  (last (pending-review-hashes (cons "test-with-html" uid))))

;; return the first pending review for given student on "test-with-html"
(define (firstreview uid)
  (first (pending-review-hashes (cons "test-with-html" uid))))

;; return the first pending review for the given student on "test-with-html"
;; where the reviewee is the given one
(define (last-review-testwithhtml-of reviewer reviewee)
  (last-review-of reviewer reviewee "test-with-html"))

(define (last-review-of reviewer reviewee assignment)
  (last (pending-review-hashes/reviewee (cons assignment reviewer)
                                        reviewee)))

;; return the feedback for given student on "test-with-html"
(define (firstfeedback uid)
  (first (feedback-hashes (cons "test-with-html" uid))))

;; return the feedback for given student on "test-with-html"
(define (lastfeedback uid)
  (last (feedback-hashes (cons "test-with-html" uid))))

(define m "masteruser@example.com")
(define master-user-name m)
(define stu1 "stu1@example.com")
(define stu2 "mf2@example.com")
(define stu3 "stu3@example.com")
;; not in the class, ever:
(define stu9 "stu9@example.com")

(define zipfile-bytes (file->bytes (build-path HERE "tiny.zip")))
  
(define assignment-yaml #"name: Assignment 1 Captain Teach
id: a1-ct
description: Problem 3.3.3 Solution
steps:
  - id: tests
    instructions: \"Submit your solution to problem 3.3.3\"
    reviews:
        - student-submission:
            id: student-reviews
            amount: 2
            rubric:
              - instruction: Click on the line number to add inline comments to the code to indicate missing tests, \
or unclear or poorly organized code. Also, use comments to indicate particularly well-organized or clear tests. You \
must add a summative comment at the end.
              - likert:
                  id: correctness
                  text: These tests are complete, correct, and easy to read.
                  min-label: Disagree
                  max-label: Agree
                  granularity: 9
")

  ;; FIXME what's legal in an ID name?
  
  (define yaml-with-html #"name: Assignment 1 <i>Captain</i> Teach
id: test-with-html
description: Problem <i>3.3.3</i> Solution
steps:
  - id: tests
    instructions: \"Submit your <i>solution</i> to problem 3.3.3\"
    reviews:
        - student-submission:
            id: student-reviews
            amount: 2
            rubric:
              - instruction: Click on the line </i>number<i> to add inline comm\
ents to the code to indicate missing tests, or unclear or poorly organized code\
. Also, use comments to indicate particularly well-organized or clear tests. Yo\
u must add a summative comment at the end.
              - likert:
                  id: correctness
                  text: These tests are <i>complete</i>, correct, and easy to read.
                  min-label: Di<i>sa</i>gree
                  max-label: A<i>gre</i>e
                  granularity: 9
")



(define zipfile-with-dirs-bytes
  (file->bytes (build-path HERE "zip-with-subdir.zip")))

(define names-and-testseqs
  `(("basic"
    (((,m ())
     (200 ,(check-anchor-links
            '("/test-class/assignments/" "/test-class/roster/"))))
    ((,m ("assignments"))
     (200 ,(check-anchor-links
            '("/test-class/author/"))))
    ((,m ("roster"))
     (200 ,(check-anchor-links
            '("/test-class/roster/upload-roster"
              "/test-class/roster/new-student"
              "/test-class/roster/edit/masteruser@example.com"))))
    ((,m ("roster" "new-student")) 200)
    ;; REGRESSION: error feedback less useful than old
    ;; should be a 400, not a 200:
    ((,m ("roster" "new-student") () #t)
     400
     bad-new-student)
    ((,m ("roster" "new-student") (alist ((action . "create-student")
                                          (uid . ,stu1)))
         #t) 200)
    ;; create same student again
    ((,m ("roster" "new-student") (alist ((action . "create-student")
                                          (uid . ,stu1)))
         #t)
     200
     same-student-again)
    ((,m ("author"))
     (200 ,(check-anchor-links '("javascript:validate()")))
     author-page)
    ;; NON-REGRESSION: new version better than old
    ((,m ("author") () #t #"assignment-id : zzz1")
     404
     bad-author-post)
    ;; ouch! another internal error!
    ((,m ("author" "bogwater") () #t #"assignment-id : zzz1")
     404
     bad-author-path)
    ;; bad YAML
    ;; NON-REGRESSION: new version better than old
    ((,m ("author" "validate") () #t #"ziggy stardust")
     (200 ,(and/p
            has-plain-text-mime-type
            (starts-with-string "Fail:")))
     bad-yaml) ;; 10
    ;; bogus path piece... actually, the API just ignores
    ;; everything until the last one. For now, this is just okay.
    ;; holding off on fixing this until we have a handle on paths...
    ((,m ("author" "boguspath" "validate") () #t ,assignment-yaml)
     (200 ,(and/p
            (is-string "Success")
            has-plain-text-mime-type))
     boguspath-validate)
    ;; this one is now invalid because the assignment already exists
    ((,m ("author" "validate") () #t ,assignment-yaml)
     (200 ,(and/p
            has-plain-text-mime-type
            (starts-with-string "Fail:")))
     existing-assignment)
    ((,m ("author" "validate") () #t ,yaml-with-html)
     (200 ,has-plain-text-mime-type)
     good-validate)
    ;; REGRESSION: missing title
    ((,m ("assignments"))
     (200 ,(check-anchor-links
            '("/test-class/author/"
              "/test-class/assignments/dashboard/a1-ct/"
              "/test-class/assignments/dashboard/test-with-html/"))))
    ;; REGRESSION: missing title
    ((,m ("assignments" "dashboard" "test-with-html"))
     (200 ,(check-anchor-links
            '("/test-class/assignments/"
              "/test-class/assignments/status/test-with-html/"
              "/test-class/dependencies/test-with-html/"
              "/test-class/author/edit/test-with-html/"
              "/test-class/export/test-with-html/test-with-html.zip"
              "/test-class/assignments/delete/test-with-html/")))) ;; 15
    ((,m ("dependencies" "test-with-html"))
     (200 ,(check-anchor-links
            '("/test-class/assignments/"
              "/test-class/assignments/dashboard/test-with-html/"
              "/test-class/dependencies/test-with-html/tests/student-reviews/"))))
    ((,m ("dependencies" "test-with-html" "tests" "student-reviews")) 200)
    ;; NON-REGRESSION: fixed bug
    ((,m ("dependencies" "test-with-html" "tests" "student-reviews" "upload") () #t #"")
     400
     bad-review-upload)
    ((,m ("dependencies" "test-with-html" "tests" "student-reviews" "upload")
         (multipart
          ((namefilevalue #"file-1" #"file-1" () #"abcd")
           (namefilevalue #"file-2" #"grogra-2" () #"efgh")))
         #t)
     (200 ,(check-anchor-links
            '("/test-class/assignments/"
              "/test-class/assignments/dashboard/test-with-html/"
              "/test-class/dependencies/test-with-html/tests/student-reviews/"))))
    ((,m ("assignments"))
     (200 ,(check-anchor-links
            '("/test-class/author/"
              "/test-class/assignments/dashboard/a1-ct/"
              "/test-class/assignments/dashboard/test-with-html/"))))
    ((,m ("assignments" "dashboard" "test-with-html"))
     (200 ,(check-anchor-links
            '("/test-class/assignments/"
              "/test-class/assignments/status/test-with-html/"
              "/test-class/assignments/open/test-with-html/"
              "/test-class/dependencies/test-with-html/"
              "/test-class/author/edit/test-with-html/"
              "/test-class/export/test-with-html/test-with-html.zip"
              "/test-class/assignments/delete/test-with-html/"))))
    ;; not open yet:
    ((,stu1 ("next" "test-with-html"))
     400
     not-open-yet)
    ;; open the assignment
    ((,m ("assignments" "open" "test-with-html"))
     (200 ,(check-anchor-links
            '("/test-class/assignments/"
              "/test-class/assignments/status/test-with-html/"
              "/test-class/assignments/close/test-with-html/"
              "/test-class/dependencies/test-with-html/"
              "/test-class/author/edit/test-with-html/"
              "/test-class/export/test-with-html/test-with-html.zip"
              "/test-class/assignments/delete/test-with-html/"))))
    ;; student navigation:
    ((,stu1 ())
     (200 ,(check-anchor-links
            '("/test-class/assignments/"))))
    ((,stu1 ("assignments"))
     (200 ,(check-anchor-links
            '("/test-class/feedback/test-with-html/"))))
    ((,stu9 ("feedback" "test-with-html"))
     403
     stranger-feedback)
    ((,stu1 ("feedback" "test-with-html"))
     (200 ,(check-anchor-links
            '("/test-class/next/test-with-html/"))))
    ;; original code just sends you to la-la-land path of your choice:
    ((,stu1 ("feedback" "test-with-html" "BOGUS" "PATH" "ELEMENTS"))
     404)
    ;; XSS attack: html in assignment description:
    ((,stu1 ("next" "test-with-html"))
     (200 ,no-italics)
     assignment-description-xss)
    ((,stu1 ("submit" "test-with-html" "tests")
            (multipart
             ((namefilevalue #"file" #"my-file" ()
                             #"oh.... \n two lines!\n")))
            #t)
     (200 ,(check-anchor-links
            '("/test-class/next/test-with-html/"))))
    ((,stu1 ("next" "test-with-html"))
     (200 ,no-italics)
     assignment-description-xss-2) ;; 31
    ;; re-submit
    ((,stu1 ("submit" "test-with-html" "tests")
            (multipart
             ((namefilevalue
               #"file" #"my-file" () #"oops... \n two different lines\n")))
            #t)
     (200 ,(check-anchor-links
            ;; FIXME YUCKY URL
            '("/test-class/next/test-with-html/"))))
    ;; re-submit with different file name
    ((,stu1 ("submit" "test-with-html" "tests")
            (multipart
             ((namefilevalue #"file"
                             #"my-diff? erent-file"
                             ()
                             #"oops... \n two different lines\n")))
            #t)
     (200
      ,(check-anchor-links
        '("/test-class/next/test-with-html/")))
     stu1-resubmits)
    ((,stu1 ("next" "test-with-html"))
     (200 ,(and/p
            (check-form-submit-links
             '("/test-class/next/test-with-html/../../submit/test-with-html/tests/"))
            (check-iframe-link
             "/test-class/next/test-with-html/../../browse/test-with-html/tests/")))
     stu1-not-yet-published)
    ;; content of the iframe:
    ((,stu1 ("browse" "test-with-html" "tests"))
     (200 ,(check-anchor-links
            '("/test-class/browse/test-with-html/tests/my-diff%3F%20erent-file"
              ;; update to new style:
              "/test-class/browse-download/test-with-html/tests/my-diff%3F%20erent-file")))
     iframe-content)
    ;; the file 
    ((,stu1 ("browse" "test-with-html" "tests" "my-diff? erent-file"))
     (200 ,(check-anchor-links
            ;; FIXME how do we feel about this? first relative url path?
            '("../tests"))))
    ;; ouch, what about this:
    ((,stu1
      ("browse" "test-with-html" "tests" "my-diff? erent-file" "download" "test-class"
                "test-with-html" ,stu1 "tests" "my-diff? erent-file"))
     403
     accidental-trainwreck)
    ;; let's see what the download content looks like
    ;; removed old-style download
    #;((,stu1 ("browse" "test-with-html" "tests" "download" "my-diff? erent-file"))
       200
       download)
    ;; trying the new-style browse download
    ((,stu1 ("browse-download" "test-with-html" "tests" "my-diff? erent-file"))
     200
     new-download)
      
    ;; wait... random strangers can submit???
    ((,stu9 ("submit" "test-with-html" "tests")
            (multipart
             ((namefilevalue #"file"
                             #"file-from-stranger" ()
                             #"anotuh\n1234\n3")))
            #t)
     403
     stranger-submit)
    ;; create another student
    ((,m ("roster" "new-student") (alist ((action . "create-student")
                                          (uid . ,stu2)))
         #t)
     200)
      
    ;; that student submits:
    ((,stu2 ("submit" "test-with-html" "tests")
            (multipart
             ((namefilevalue
               #"file" #"a-third-file.arr" () #"zzz\n\nzzz\nzzz\n")))
            #t)
     (200 ,(check-anchor-links
            '("/test-class/next/test-with-html/")))
     stu2-submits)
    ;; re-submit of same file with same extension
    ((,stu2 ("submit" "test-with-html" "tests")
            (multipart
             ((namefilevalue
               #"file" #"a-third-file.arr" () #"zzz\n\nzzz\nzzz\ndcalfine")))
            #t)
     (200 ,(check-anchor-links
            '("/test-class/next/test-with-html/")))
     stu2-re-submits)
    ;; can stu2 read stu1's file? No. Good.
    ((,stu2 ("browse" "test-with-html" "tests" "my-diff? erent-file"))
     403
     see-others-file)
    ;; stu1 publishes:
    ((,stu1 ,(path2list "submit/test-with-html/tests")
            (alist ((action . "submit")))
            #t)
     (200 ,(check-anchor-links
            ;; FIXME yucky url
            '("/test-class/submit/test-with-html/tests/../../../feedback/test-with-html/")))
     stu1-publishes)
    ((,stu1 ,(path2list "feedback/test-with-html"))
     ;; FIXME yucky urls
     (200 ,(λ (x)
             (let ([hashes (pending-review-hashes (cons "test-with-html" stu1))])
               ((check-anchor-links
                 (cons
                  "/test-class/browse/test-with-html/tests/"
                  (map (λ (hash)
                         (string-append
                          "/test-class/feedback/test-with-html/../../review/" hash "/"))
                       hashes)))
                x)))))
    ;; bogus hash:
    ((,stu1 ,(path2list "review/598109a435c52dc6ae10c616bcae407a"))
     403
     bogus-review)
    ;; viewing a bogus feedback
    ((,stu1 ("feedback" "file-container" "BOGUSSS" "ALSOBAD" "load"))
     403
     bogus-file-container)
    ;; thunk to delay extraction of hash:
    ,(λ ()
       `((,stu1 ("review" ,(lastreview-test-with-html stu1)))
         ;; FIXME there's a *space* in there? and in the iframe link too?
         (200 ,(λ (x)
                 (and/p
                   ((check-anchor-links
                     (list (string-append
                            "/test-class/review/" (lastreview-test-with-html stu1)
                            "/../../review/submit/" (lastreview-test-with-html stu1) "/")))
                    x)
                   ((check-iframe-link
                     (string-append
                      "/test-class/review/" (lastreview-test-with-html stu1)
                      "/../../file-container/" (lastreview-test-with-html stu1)))
                    x))))))
    ;; the iframe...
    ,(λ ()
       `((,stu1 ("file-container" ,(lastreview-test-with-html stu1)))
         (200 ,(λ (r)
                 ;; nasty hack here because of nondeterminism; don't know whether
                 ;; file name will be file-1 or grogra-2.
                 (define (make-links filename)
                   (list
                    (string-append "/test-class/file-container/" (lastreview-test-with-html stu1) "/" filename)
                    ;; update to new style
                    (string-append "/test-class/download/" (lastreview-test-with-html stu1) "/" filename)))
                 (define links-1 (make-links "file-1"))
                 (define links-2 (make-links "grogra-2"))
                 (check-pred
                  (λ (x) (or ((has-anchor-links/bool links-1) x)
                             ((has-anchor-links/bool links-2) x)))
                  r)))))
    ;; stu2 logs in:
    ((,stu2 ())
     (200 ,(check-anchor-links '("/test-class/assignments/"))))
    ;; clicks on assignments
    ((,stu2 ("assignments"))
     (200 ,(check-anchor-links '("/test-class/feedback/test-with-html/"))))
    ;; stu2 publishes:
    ((,stu2 ,(path2list "submit/test-with-html/tests")
            (alist ((action . "submit")))
            #t)
     (200 ,(check-anchor-links
            ;; FIXME yucky urls
            '("/test-class/submit/test-with-html/tests/../../../feedback/test-with-html/")))
     stu2-publishes)
    ((,stu2 ("feedback" "test-with-html")) 200)
    ;; stu2 clicks on last review
    ,(λ ()
       `((,stu2 ("review" ,(last-review-testwithhtml-of stu2 stu1)))
         200
         review))
    ;; load review file-container for directory
    ,(λ ()
       `((,stu2 ("file-container" ,(last-review-testwithhtml-of stu2 stu1)))
         200
         review-iframe-dir))
    ;; file-container for file
    ,(λ ()
       `((,stu2 ("file-container" ,(last-review-testwithhtml-of stu2 stu1)
                                  "my-diff? erent-file"))
         (200 ,(check-anchor-links '("./")))
         review-iframe-file))
    ;; actual text of file
    ,(λ ()
       `((,stu2 ("file-container" ,(last-review-testwithhtml-of stu2 stu1) "download"
                                  "my-diff? erent-file"))
         200
         review-iframe-file-content))
    ;; actual text of file using new endpoint:
    ,(λ ()
       `((,stu2 ("download" ,(last-review-testwithhtml-of stu2 stu1) "my-diff? erent-file"))
         200
         review-iframe-file-content-new))
    ;; should it be an error to submit bogus rubric json?
    ,(λ ()
       `((,stu2 ("review" ,(last-review-testwithhtml-of stu2 stu1) "tests" "save")
                (json #"\"abcd\"")
                #t)
         200))
    ,(λ ()
       `((,stu2 ("review" "submit" ,(last-review-testwithhtml-of stu2 stu1)))
         200
         stu2-submits-review1))
    ;; do the other review too
    ,(λ ()
       `((,stu2 ("review" ,(lastreview-test-with-html stu2) "tests" "save")
                (json #"\"abcde\"")
                #t)
         200))
    ,(λ ()
       `((,stu2 ("review" "submit" ,(lastreview-test-with-html stu2)))
         (200 ,(check-anchor-links '("/test-class/feedback/test-with-html")))
         stu2-submits-review2))
    ;; stu1 now views it
    ,(λ ()
       `((,stu1 ("feedback" "view" ,(firstfeedback stu1)))
         200
         stu1-views-review))
    ,(λ ()
       `((, stu1 ("feedback" "file-container" ,(firstfeedback stu1)))
         (200 ,(check-anchor-links
                (list
                 (string-append "/test-class/feedback/file-container/"
                                (firstfeedback stu1)
                                "/my-diff%3F%20erent-file"))))
         stu1-views-review-fc-dir))
    ,(λ ()
       `((,stu1 ("feedback" "file-container" ,(firstfeedback stu1) "my-diff? erent-file"))
         (200 ,(check-anchor-links
                `("./")))
         stu1-views-review-fc-file))
    ,(λ ()
       `((,stu1 ("download" ,(firstfeedback stu1) "my-diff? erent-file"))
         ;; need a check that this is a file-download-y thing and not HTML:
         200
         stu1-views-review-fc-file-raw))
    ,(λ ()
       `((,stu1 ("feedback" "view" ,(firstfeedback stu1))
                (alist
                 ((feedback . "feedback with <i>italics</i>.")
                  (flag . "goronsky")))
                #t)
         (200 ,no-italics)
         stu1-submits-feedback-xss))
    ,(λ ()
       `((,stu2 ("feedback" "test-with-html"))
         (200 ,(check-anchor-links
                (cons
                 "/test-class/browse/test-with-html/tests/"
                 (for/list ([hash (in-list (completed-review-hashes
                                            (cons "test-with-html" stu2)))])
                   (string-append
                    "/test-class/feedback/test-with-html/../../review/" hash "/")))))))
    ;; NEED CONTAINED-LINKS FOR THE REMAINING
    ;; edit user missing username
    ((,m ("roster" "edit"))
     404
     edit-user-missing-name)
    ((,m ("roster" "upload-roster"))
     200
     upload-roster-page)
    ((,m ("roster") (multipart
                     ((nameandvalue
                       #"action" #"process-roster")
                      (namefilevalue
                       #"file" #"my-roster" () #"razza\nrazza\nrazza"))) #t)
     200
     upload-bad-roster)
    ((,m ("download" "abcde"))
     404
     download-no-path)
    ;; create new user, try uploading zip file
    ((,m ("roster" "new-student") (alist ((action . "create-student")
                                          (uid . ,stu3)))
         #t) 200)
    ((,stu3 ("submit" "test-with-html" "tests")
            (multipart
             ((namefilevalue #"file" #"tiny.zip" ()
                             ,zipfile-bytes)))
            #t)
     (200 ,(check-anchor-links
            '("/test-class/next/test-with-html/"))))
    ((,stu3 ("next" "test-with-html"))
     (200
      ,(and/p
         (check-form-submit-links
          '("/test-class/next/test-with-html/../../submit/test-with-html/tests/"))
         (check-iframe-link
          "/test-class/next/test-with-html/../../browse/test-with-html/tests/")
         (hasnt-string
          "Your submission contains no files.")))
     stu3-not-yet-published)
    ;; now navigate to subdirectory, then submit
    ))
    ("bogus submit"
     (((,m ("roster" "new-student") (alist ((action . "create-student")
                                            (uid . ,stu1)))
           #t) 200)
      ((,stu1 ("submit" "test-with-html" "tests")
              (multipart
               ((namefilevalue #"file" #"tiny.zip" ()
                               ,zipfile-with-dirs-bytes)))
              #t)
       404)))
    ;; check that links are correctly constructed for zips containing
    ;; subdirectories.
    ("zip with subdirs"
     (((,m ("roster" "new-student") (alist ((action . "create-student")
                                            (uid . ,stu1)))
           #t) 200)
      ((,m ("roster" "new-student") (alist ((action . "create-student")
                                            (uid . ,stu2)))
           #t) 200)
      ((,m ("author" "validate") () #t ,assignment-yaml)
       200)
      ((,m ("dependencies" "a1-ct" "tests" "student-reviews" "upload")
           (multipart
            ((namefilevalue #"file-1" #"file-1" () #"abcd")
             (namefilevalue #"file-2" #"grogra-2" () #"efgh")))
           #t)
       200)
      ((,m ("assignments" "open" "a1-ct"))
       200)
      ;; student 1 submits zip with subdirs:
      ((,stu1 ("submit" "a1-ct" "tests")
              (multipart
               ((namefilevalue #"file" #"zipwithdirs.zip" ()
                               ,zipfile-with-dirs-bytes)))
              #t)
       200)
      ;; check that links have path included:
      ((,stu1 ("browse" "a1-ct" "tests" "zz"))
       (200 ,(check-anchor-links
              '("/test-class/browse/a1-ct/tests/zz/b.txt"
                "/test-class/browse/a1-ct/tests/zz/yy"))))
      ;; stu1 publishes
      ((,stu1 ,(path2list "submit/a1-ct/tests")
              (alist ((action . "submit")))
              #t)
       200)
      ;; stu2 submits dontcare
      ((,stu2 ("submit" "a1-ct" "tests")
              (multipart
               ((namefilevalue #"file" #"my-file" ()
                               #"oh.... \n two lines!\n")))
              #t)
       200)
      ;; stu2 publishes
      ((,stu2 ,(path2list "submit/a1-ct/tests")
              (alist ((action . "submit")))
              #t)
       200)
      ,(λ ()
         `((,stu2 ("file-container" ,(last-review-of stu2 stu1 "a1-ct") "zz"))
           (200 ,(check-anchor-links
                  (list
                   (~a "/test-class/file-container/" (last-review-of stu2 stu1 "a1-ct") "/zz/yy")
                   (~a "/test-class/file-container/" (last-review-of stu2 stu1 "a1-ct") "/zz/b.txt"))))))
      ))
    ("db bug"
     (((,m ("roster" "new-student") (alist ((action . "create-student")
                                            (uid . ,stu1)))
           #t) 200)
      ((,stu1 ("next" "bogus-assignment")) 404)))))


