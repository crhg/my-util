(defpackage :my-util/misc
  (:use :cl)
  (:export #:with-gensyms #:mb-mapcar #:transpose))

(in-package :my-util/misc)

;; On Lispより
(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
		     `(,s (gensym)))
		 syms)
     ,@body))

(defun mb-mapcar (func &rest params)
  "多値を返す関数FUNCについて拡張したMAPCAR。それぞれの値をリストにして多値で返す
   PARAMSが空の時FUNCを呼ばないのでいくつ多値を返せばいいかわからないので0個で返すことにする
   (MULTIPLE-VALUE-BINDで足りない変数はNILになるからたぶん大丈夫だろう)"
  (labels ((f (&rest args) (multiple-value-list (apply func args))))
    (apply #'values (apply #'transpose (apply #'mapcar #'f params)))))

(defun transpose (&rest lists)
  "LISTS=((X1 X2 ...) (Y1 Y2 ...) ...) を((X1 Y1 ...) (X2 Y2 ...) ...)に組み直す"
  (if (null lists)
      nil
      (apply #'mapcar #'list lists)))

