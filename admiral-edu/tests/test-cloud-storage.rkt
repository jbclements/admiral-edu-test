#lang racket

;; running tests for cloud storage requires credentials. This
;; file therefore provides a function to be called with S3
;; credentials.

;; WARNING: STOMPS THE /tmp SUBDIRECTORY OF THE BUCKET
;; NB: assumes that the /tmp subdirectory is empty.

;; no testing for this ...
(module* test racket/base)

(require admiral-edu/configuration
         admiral-edu/tests/test-configuration
         admiral-edu/storage/storage-basic-tr
         racket/hash
         rackunit)

(define testing-prefix "tmp")

(define CLOUD-SERVER "storage.googleapis.com")
(define TEST-BUCKET "cloud-storage-test-bucket/")

(provide
 (contract-out [run-tests
                (-> string? string? string? void?)]))

(define (add-testing-prefix str)
  (path->string (build-path testing-prefix str)))

(define (run-tests local-tmp-dir access-id secret)
  (define cloud-conf
    (hash-union
     #:combine (λ (a b) b)
     test-conf
     (hash "cloud-access-key-id" access-id
           "cloud-host" CLOUD-SERVER
           "bucket" TEST-BUCKET
           "storage-mode" "cloud-storage"
           "cloud-secret-key" secret
           "local-storage-path" local-tmp-dir)))
  (check-equal? (hash-ref cloud-conf "cloud-secret-key") secret)

  (define path1 (add-testing-prefix "wobbly-bobbly"))
  (define path2 (add-testing-prefix "zabba/dabba/trog"))
  
  (parameterize ([current-configuration cloud-conf])
    (delete-path testing-prefix)
    ;; it starts empty:
    (check-equal? (list-sub-files testing-prefix) '())
    ;; add a file:
    (check-equal? (write-file path1
                              #"this content \n goes in the file.")
                  (void))
    ;; check that it's listed:
    (check-equal? (list-sub-files testing-prefix) '("tmp/wobbly-bobbly"))
    (check-equal? (list-files testing-prefix) '("wobbly-bobbly"))
    (check-equal? (list-dirs testing-prefix) '())
    ;; check the content:
    (check-equal? (retrieve-file-bytes path1)
                  #"this content \n goes in the file.")
    ;; delete it:
    (check-equal? (delete-path path1)
                  (void))
    ;; check that there are no files:
    (check-equal? (list-sub-files testing-prefix) '())
    (check-equal? (write-file path2 #"secondfilecontent")
                  (void))
    (check-equal? (list-files testing-prefix) '())
    (check-equal? (list-dirs testing-prefix) '("zabba"))
    ;; delete local copy only:
    (check-true (file-exists? (build-path (local-storage-path) path2)))
    (delete-file (build-path (local-storage-path) path2))
    (check-false (file-exists? (build-path (local-storage-path) path2)))
    ;; should re-fetch from cloud:
    (check-equal? (retrieve-file-bytes path2) #"secondfilecontent")
    ;; file exists locally again:
    (check-true (file-exists? (build-path (local-storage-path) path2)))
    ;; should delete all sub-files
    (check-equal? (delete-path testing-prefix) (void))
    (check-equal? (list-sub-files testing-prefix) '())
    (check-false (file-exists? (build-path (local-storage-path) path2)))
    )
  (void))