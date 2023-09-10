(defsystem "zero-cost-structures"
  :serial t
  :description "Simple wrapper macros to use zero runtime cost structures in Common Lisp"
  :depends-on ("alexandria"
               "cl-environments")
  :components ((:file "package"))
  :in-order-to ((test-op (test-op "zero-cost-structures/tests"))))

(defsystem "zero-cost-structures/tests"
  :serial t
  :depends-on ("zero-cost-structures"
               "fiveam")
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :fiveam '#:run! :zero-cost-structures)))
