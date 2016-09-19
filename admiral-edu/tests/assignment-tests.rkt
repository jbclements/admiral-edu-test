#lang racket/base

(module+ test
(require admiral-edu/authoring/assignment-parser
         admiral-edu/authoring/assignment-structs
         admiral-edu/authoring/next-action
         yaml
         quickcheck
         rackunit
         rackunit/text-ui
         rackunit/quickcheck)

  ;; fundamentally I think these are all just indirect tests of the YAML packge....

  (check-property
   (property
    ([str arbitrary-string])
    (let ((i (instruction str)))
      (equal? i (yaml->instruction (instruction->yaml i))))))

  (check-property
   (property
    ([id arbitrary-string]
     [text arbitrary-string])
    (let ((form (free-form id text)))
      (equal? form (yaml->free-form (free-form->yaml form))))))

  
  (check-property
   (property
    ([id arbitrary-string]
     [text arbitrary-string]
     [min arbitrary-string]
     [max arbitrary-string]
     [granularity arbitrary-natural])
    (let ((l (likert id text min max (add1 granularity))))
      (equal? l (yaml->likert (likert->yaml l))))))


(check-true (rubric-element? (instruction "Some test instructions")))
(check-true (rubric-element? (likert "some-id" "Some likert instructions" "Disagree" "Agree" 9)))
(check-true (rubric-element? (free-form "some-id" "Some test instructions")))



(define test-assignment
  (Assignment "Clocks"
              "clocks"
              "Students develop functions representing an alarm clock."
              default-assignment-handler
              (list
               (step "tests"
                     "Submit your test cases. Do not submit any clock implementation."
                     (instructor-solution "Poor Tests"
                                          (rubric
                                           (likert "correctness"
                                                   "These tests are correct."
                                                   "Disagree"
                                                   "Agree"
                                                   9)
                                           
                                           (instruction "Provide feedback on tests that are not correct by clicking on the line number and adding a comment.")
                                           
                                           (likert "coverage"
                                                   "These tests cover the possible inputs."
                                                   "Disagree"
                                                   "Agree"
                                                   9)
                                           
                                           (free-form "not-covered"
                                                      "If applicable, provide inputs that are not covered by the tests.")))
                     
                     (instructor-solution "Good Tests"
                                          (rubric
                                           (likert "correctness"
                                                   "These tests are correct."
                                                   "Disagree"
                                                   "Agree"
                                                   9)
                                          
                                           (instruction "Provide feedback on tests that are not correct by clicking on the line number and adding a comment.")
                                          
                                           (likert "coverage"
                                                   "These tests cover the possible inputs."
                                                   "Disagree"
                                                   "Agree"
                                                   9)
                                           
                                           (free-form "not-covered"
                                                      "If applicable, provide inputs that are not covered by the tests.")))
                     
                     (student-submission "student-review"
                                         1
                                         (rubric
                                          (likert "correctness"
                                                  "These tests are correct."
                                                  "Disagree"
                                                  "Agree"
                                                  9)
                                          
                                          (instruction "Provide feedback on tests that are not correct by clicking on the line number and adding a comment.")
                                          
                                          (likert "coverage"
                                                  "These tests cover the possible inputs."
                                                  "Disagree"
                                                  "Agree"
                                                  9)
                                          
                                          (free-form "not-covered"
                                                     "If applicable, provide inputs that are not covered by the tests."))))
               
               (step "implementation"
                     "Submit all of your test cases and your clock implementation."
                     
                     (instructor-solution "Poor Implementation"
                                          (rubric
                                          (likert "behavior"
                                                  "This code correctly implements the desired behavior."
                                                  "Disagree"
                                                  "Agree"
                                                  9)
                                          
                                          (instruction "If applicable, leave inline feedback where the incorrect behaviors exist.")
                                          
                                          (likert "structure"
                                                  "This code is structured well."
                                                  "Disagree"
                                                  "Agree"
                                                  9)
                                          
                                          (instruction "If applicable, leave inline feedback where the code is not structured well.")
                                          
                                          (free-form "feedback"
                                                     "Additional Comments")))
                     
                     (instructor-solution "Good Implementation"
                                          (rubric
                                          (likert "behavior"
                                                  "This code correctly implements the desired behavior."
                                                  "Disagree"
                                                  "Agree"
                                                  9)
                                          
                                          (instruction "If applicable, leave inline feedback where the incorrect behaviors exist.")
                                          
                                          (likert "structure"
                                                  "This code is structured well."
                                                  "Disagree"
                                                  "Agree"
                                                  9)
                                          
                                          (instruction "If applicable, leave inline feedback where the code is not structured well.")
                                          
                                          (free-form "feedback"
                                                     "Additional Comments")))
                     
                     (student-submission "student-review"
                                         1
                                         (rubric
                                         (likert "behavior"
                                                  "This code correctly implements the desired behavior."
                                                  "Disagree"
                                                  "Agree"
                                                  9)
                                          
                                         (instruction "If applicable, leave inline feedback where the incorrect behaviors exist.")
                                          
                                         (likert "structure"
                                                  "This code is structured well."
                                                  "Disagree"
                                                  "Agree"
                                                  9)
                                          
                                         (instruction "If applicable, leave inline feedback where the code is not structured well.")
                                          
                                         (free-form "feedback"
                                                     "Additional Comments")))))))

(define test-rubric
  (rubric
   (likert "behavior"
           "This code correctly implements the desired behavior."
           "Disagree"
           "Agree"
           9)
   
   (instruction "If applicable, leave inline feedback where the incorrect behaviors exist.")
   
   (likert "structure"
           "This code is structured well."
           "Disagree"
           "Agree"
           9)
   
   (instruction "If applicable, leave inline feedback where the code is not structured well.")
   
   (free-form "feedback"
              "Additional Comments")))

(check-equal? test-rubric (yaml->rubric (rubric->yaml test-rubric)))

(let ((submission (instructor-solution "test-id" test-rubric)))
  (check-equal? submission (yaml->instructor-solution (instructor-solution->yaml submission)))
  (check-equal? submission (yaml->review (review->yaml submission))))

(check-true (yaml? (student-submission->yaml (student-submission "review" 1 test-rubric))))

(let ((submission (student-submission "review" 2 test-rubric)))
  (check-equal? submission (yaml->student-submission (student-submission->yaml submission)))
  (check-equal? submission (yaml->review (review->yaml submission))))

(check-true (yaml? (step->yaml (step "test-id" "test instructions"))))
(check-true (yaml? (step->yaml (step "test-id" "test instructions" (instructor-solution "test-id" test-rubric)))))
(check-true (yaml? (step->yaml (step "test-id" "test instructions" (instructor-solution "test-id" test-rubric) (student-submission "review" 2 test-rubric)))))

(let ((step (step "test-id" "test instructions")))
  (check-equal? step (yaml->step (step->yaml step))))

(let ((step (step "test-id" "test instructions" (instructor-solution "test-id" test-rubric))))
  (check-equal? step (yaml->step (step->yaml step))))

(let ((step (step "test-id" "test instructions" (instructor-solution "test-id" test-rubric) (student-submission "review" 2 test-rubric))))
  (check-equal? step (yaml->step (step->yaml step))))

(check-true (yaml? (assignment->yaml test-assignment)))
(check-equal? test-assignment (yaml->assignment (assignment->yaml test-assignment)))


(let ((i (instruction "Test instructions")))
  (check-equal? i (yaml->instruction (instruction->yaml i))))


(let ((form (free-form "test-id" "Test instructions")))
  (check-equal? form (yaml->free-form (free-form->yaml form))))


(let ((l (likert "test-id" "Test instructions" "Disagree" "Agree" 9)))
  (check-equal? l (yaml->likert (likert->yaml l)))))
