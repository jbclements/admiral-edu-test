#lang racket

(provide test-edits)

(define test-edits
  ((same-student-again
    (replace-with
     (string-append
      "function getClassName(){ return \"test-class\"; } function save(content, "
      "callback){ console.log(\"Attempting to save to \" + 'validate'); var xhr "
      "= new XMLHttpRequest(); xhr.open(\"POST\", 'validate', true); xhr.setRequ"
      "estHeader('Content-Type', 'application/json; charset=UTF-8'); xhr.send(co"
      "ntent); xhr.onloadend = callback; } function load(callback){ }")
     (string-append
      "function getClassName(){ return \"test-class\"; } function save(content, "
      "callback){ console.log(\"Attempting to save to \" + \"'validate'\"); var "
      "xhr = new XMLHttpRequest(); xhr.open(\"POST\", \"'validate'\", true); xhr"
      ".setRequestHeader('Content-Type', 'application/json; charset=UTF-8'); xhr"
      ".send(content); xhr.onloadend = callback; } function load(callback){ }")))))
