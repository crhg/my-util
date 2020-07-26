(defpackage my-util
  (:use :cl :my-util/dbind)
  (:export with-gensyms if-match if-match-where))

(in-package :my-util)

;; On Lispより
(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
		     `(,s (gensym)))
		 syms)
     ,@body))

;;; マッチ

;; On Lispを参考にしたが仕様は異なる
;;
;; <パターン> ::= ?で始まるシンボル -- 変数。何にでもマッチ。値を束縛する
;;              | :_ -- 何にでもマッチ。値は捨てる
;;              | その他アトム -- 定数。そのものとequalならマッチ
;;              | (<パターン> . <パターン>)
;; 同じ変数が複数現れてはいけない(動作は未定義)

(defmacro if-match (pat exp then &optional else)
  (with-gensyms (val)
    `(let ((,val ,exp)
	   ,@(mapcar #'(lambda (v) `(,v nil))
		 (vars-in pat)))
       (declare (ignorable ,val))
       (if ,(pat-match pat val)
	   ,then
	   ,else))))

;; ガード付きに拡張したif-match
;; expの値がpatにマッチしたときさらにguardを評価して真ならthen, それ以外のときはelse
;; マッチによって束縛した変数はguardからも見える
(defmacro if-match-where (pat exp guard then &optional else)
  (with-gensyms (else-func)
    `(labels ((,else-func () ,else))
       (if-match ,pat ,exp
		 (if ,guard ,then (,else-func))
		 (,else-func)))))

(defun pat-match (pat val)
  (cond
    ((var? pat) `(progn (setf ,pat ,val) t))
    ((eq pat :_) 't)
    ((atom pat) `(equal ',pat ,val))
    (t `(and
	 (consp ,val)
	 ,(pat-match (car pat) `(car ,val))
	 ,(pat-match (cdr pat) `(cdr ,val))))))

(defun vars-in (pat)
  (cond
    ((var? pat) (list pat))
    ((atom pat) nil)
    (t
      (union (vars-in (car pat))
	     (vars-in (cdr pat))))))

(defun var? (x)
  (and (symbolp x) (eq (char (symbol-name x) 0) #\?)))

