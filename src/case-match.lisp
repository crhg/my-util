(defpackage my-util/case-match
  (:use :cl :my-util/dbind :my-util/misc)
  (:export #:if-match #:if-match-where #:case-match))

(in-package :my-util/case-match)

;; On Lispを参考にしたが仕様は異なる
(defmacro if-match (pat exp then &optional else)
  "EXPがPATにマッチするときPATの中の変数を束縛してTHENを、マッチしないときELSEを実行する
   <パターン> ::= ?で始まるシンボル -- 変数。何にでもマッチ。値を束縛する
                | :_ -- 何にでもマッチ。値は捨てる
                | その他アトム -- 定数。そのものとequalならマッチ
                | (<パターン> . <パターン>)
  同じ変数が複数現れてはいけない(動作は未定義)"
  (with-gensyms (val)
    `(let ((,val ,exp)
	   ,@(mapcar #'(lambda (v) `(,v nil))
		 (vars-in pat)))
       (declare (ignorable ,val))
       (if ,(pat-match pat val)
	   ,then
	   ,else))))

(defmacro if-match-where (pat exp guard then &optional else)
  "ガード付きに拡張したif-match
EXPの値がPATにマッチしたときさらにGUARDを評価して真ならTHEN, それ以外のときはELSE
マッチによって束縛した変数はGUARDからも見える"
  (with-gensyms (else-func)
    `(labels ((,else-func () ,else))
       (if-match ,pat ,exp
		 (if ,guard ,then (,else-func))
		 (,else-func)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun pat-match (pat val)
    "PATをVALにマッチさせるためのプログラムに展開する補助関数"
    (cond
      ((var? pat) `(progn (setf ,pat ,val) t))
      ((eq pat :_) 't)
      ((atom pat) `(equal ',pat ,val))
      (t `(and
	   (consp ,val)
	   ,(pat-match (car pat) `(car ,val))
	   ,(pat-match (cdr pat) `(cdr ,val))))))

  (defun vars-in (pat)
  "PATの中に含まれている変数のリスト"
  (cond
    ((var? pat) (list pat))
    ((atom pat) nil)
    (t
      (union (vars-in (car pat))
	     (vars-in (cdr pat))))))

(defun var? (x)
  "Xが変数か"
  (and (symbolp x) (eq (char (symbol-name x) 0) #\?)))
)

(defmacro case-match (expr &rest cases)
  "EXPRの値をCASESにマッチさせて結果を返す
CASESは以下の形式のCASE節を並べたもの
  (<パターン> [:where <ガード>] 式...)"
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
