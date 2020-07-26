(defsystem "my-util"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "dbind")
		 (:file "main" :depends-on ("dbind"))
		 (:file "case-match" :depends-on ("main" "dbind")))))
  :description ""
  :in-order-to ((test-op (test-op "my-util/tests"))))

(defsystem "my-util/tests"
  :author ""
  :license ""
  :depends-on ("my-util"
               "rove")
  :components ((:module "tests"
			:components
			((:file "dbind")
			 (:file "main")
			 (:file "case-match"))))
  :description "Test system for my-util"
  :perform (test-op (op c) (symbol-call :rove :run c)))
