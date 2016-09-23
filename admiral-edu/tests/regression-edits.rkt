#lang racket

(provide test-edits)

(define test-edits
  `((same-student-again
     (replace-with
       " Received 'frogstar@example.com' but User ID is already registered in the class. "
       ((p "Received 'frogstar@example.com' but User ID is already registered in the class."))))
    (author-page
     (replace-with
      ,(string-append
       "function getClassName(){ return \"test-class\"; } function save(content, "
       "callback){ console.log(\"Attempting to save to \" + 'validate'); var xhr "
       "= new XMLHttpRequest(); xhr.open(\"POST\", 'validate', true); xhr.setRequ"
       "estHeader('Content-Type', 'application/json; charset=UTF-8'); xhr.send(co"
       "ntent); xhr.onloadend = callback; } function load(callback){ }")
      (,(string-append
       "function getClassName(){ return \"test-class\"; } function save(content, "
       "callback){ console.log(\"Attempting to save to \" + \"validate\"); var xhr "
       "= new XMLHttpRequest(); xhr.open(\"POST\", \"validate\", true); xhr.setRequ"
       "estHeader('Content-Type', 'application/json; charset=UTF-8'); xhr.send(co"
       "ntent); xhr.onloadend = callback; } function load(callback){ }"))))
    (existing-assignment
     (replace-with
      "The specified assignment id 'a1-ct' already exists."
      ("Fail: The specified assignment id 'a1-ct' already exists.")))))
