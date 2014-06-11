#lang racket

(require "mysql/common.rkt"
         (prefix-in class: "mysql/class.rkt") 
         (prefix-in user: "mysql/user.rkt")
         (prefix-in role: "mysql/role.rkt")
         (prefix-in roles: "mysql/roles.rkt")
         (prefix-in assignment: "mysql/assignment.rkt")
         (prefix-in submission: "mysql/submission.rkt")
         (prefix-in review: "mysql/review.rkt"))

;; Initializes the database.
(provide init-db)
(define (init-db)
  (user:init)
  (class:init)
  (role:init)
  (roles:init)
  (assignment:init)
  (submission:init)
  (review:init))


;; User Table
(provide user:all user:create user:exists?)

;; Class Table
(provide class:all class:create class:exists?)

;; Role Table
(provide role:select role:associate role:in-class role:user-uid role:user-class role:user-role role:exists?)

;; Roles Table
(provide roles:create roles:get-role roles:role-id roles:role-name roles:role-can-edit roles:all)

;; Assignment Table
(provide assignment:exists? assignment:create assignment:all assignment:list)

;; Submission Table
(provide submission:table 
         submission:record submission:record?
         submission:assignment-id submission:assignment-id-type submission:record-assignment
         submission:version submission:version-type submission:record-version
         submission:class-id submission:class-id-type submission:record-class
         submission:step-id submission:step-id-type submission:record-step
         submission:user-id submission:user-id-type submission:record-user
         submission:time-stamp submission:time-stamp-type submission:record-time-stamp
         submission:create submission:list submission:count
         submission:exists?)


;; Review Table
(provide review:table
         review:assignment-id review:assignment-id-type
         review:step-id review:step-id-type
         review:version review:version-type
         review:class-id review:class-id-type
         review:reviewee-id review:reviewee-id-type
         review:reviewer-id review:reviewer-id-type
         review:time-stamp review:time-stamp-type
         review:select-review)