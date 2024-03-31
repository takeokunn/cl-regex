(defsystem "cl-regex"
  :version "0.0.1"
  :author ""
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "cl-regex/tests"))))

(defsystem "cl-regex/tests"
  :author ""
  :license ""
  :depends-on ("cl-regex"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cl-regex"
  :perform (test-op (op c) (symbol-call :rove :run c)))
