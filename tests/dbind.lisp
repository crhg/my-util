(defpackage my-util/tests/dbind
  (:use :cl
        :my-util/dbind
        :rove))
(in-package :my-util/tests/dbind)

;; NOTE: To run this test file, execute `(asdf:test-system :my-util)' in your Lisp.

(deftest test-dbind
  (ok (equal
       (dbind (x y) '(a 1) `((x ,x) (y ,y)))
       '((x a) (y 1)))))



