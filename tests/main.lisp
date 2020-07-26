(defpackage my-util/tests/main
  (:use :cl
        :my-util
        :rove))
(in-package :my-util/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :my-util)' in your Lisp.

(deftest test-with-gensyms
  (ok (equal
       (macroexpand '(my-util::with-gensyms () body))
       '(let () body)))
  (ok (equal
       (macroexpand '(my-util::with-gensyms (a) body))
       '(let ((a (gensym))) body)))
  (ok (equal
       (macroexpand '(my-util::with-gensyms (a b) body))
       '(let ((a (gensym))
	      (b (gensym)))
	 body))))

(deftest test-if-match
  (ok (equal
       (if-match ?x 1 ?x)
       1)))

(deftest test-if-match-where
  (ok (equal
       (if-match-where ?x 1 (= ?x 1) ?x 'else)
       1))
  (ok (equal
       (if-match-where ?x 2 (= ?x 1) ?x 'else)
       'else)))

