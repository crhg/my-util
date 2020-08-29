(defpackage my-util/tests/main
  (:use :cl
        :my-util
        :rove))
(in-package :my-util/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :my-util)' in your Lisp.

(deftest test-with-gensyms
  (ok (equal
       (macroexpand '(with-gensyms () body))
       '(let () body)))
  (ok (equal
       (macroexpand '(with-gensyms (a) body))
       '(let ((a (gensym))) body)))
  (ok (equal
       (macroexpand '(with-gensyms (a b) body))
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

(deftest test-mb-mapcar
  (multiple-value-bind (v1 v2)
      (mb-mapcar #'(lambda (x y) (values (+ x y) (- x y))) '(1 2 3) '(2 4 6))
    (ok (equal v1 '(3 6 9)))
    (ok (equal v2 '(-1 -2 -3))))
  (multiple-value-bind (v1 v2)
      (mb-mapcar #'(lambda (x) (values x x)) '())
    (ok (equal v1 '()))
    (ok (equal v2 '()))
  )
  (multiple-value-bind (v1 v2)
      (mb-mapcar #'(lambda (x) (values nil x)) '(1 2))
    (ok (equal v1 '(nil nil)))
    (ok (equal v2 '(1 2)))
  ))
