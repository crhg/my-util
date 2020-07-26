(defpackage my-util/tests/case-match
  (:use :cl
        :my-util/case-match
        :rove))
(in-package :my-util/tests/case-match)

;; NOTE: To run this test file, execute `(asdf:test-system :my-util)' in your Lisp.

(deftest test-case-match
  (ok (equal
       (case-match 1
		   (?x :where (numberp ?x) `(number ,?x)))
       '(number 1))))

		 
