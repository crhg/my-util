(defpackage my-util/case-match
  (:use :cl :my-util/dbind :my-util)
  (:export #:case-match))

(in-package :my-util/case-match)

(defmacro case-match (expr &rest cases)
  (with-gensyms (gexpr)
    `(let ((,gexpr ,expr))
       ,(expand-cases gexpr cases))))

(defun expand-cases (gexpr cases)
  (if (null cases)
      nil
      (let ((c (car cases))
	    (else (expand-cases gexpr (cdr cases))))
	(if-match
	 (?pat :where ?guard . ?body)
	 c
	 `(if-match-where ,?pat ,gexpr ,?guard (progn ,@?body) ,else)
	 (dbind (?pat . ?body) c
	   `(if-match ,?pat ,gexpr (progn ,@?body) ,else))))))
