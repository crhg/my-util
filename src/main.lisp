(defpackage my-util
  (:use :cl :my-util/dbind :my-util/misc :my-util/case-match)
  (:export
   #:dbind #:destruc
   #:with-gensyms #:mb-mapcar #:transpose
   #:if-match #:if-match-where #:case-match))
