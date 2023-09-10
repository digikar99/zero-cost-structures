(defpackage :zero-cost-structures/tests
  (:use :cl :fiveam :zero-cost-structures))

(in-package :zero-cost-structures/tests)

(def-suite :zero-cost-structures)
(in-suite :zero-cost-structures)

(define-zero-cost-struct pair a b)

(def-test make-read ()
  (is-true (every #'=
                  '(3.0 2.0)
                  (with-zero-cost-struct (x pair)
                                         (make-pair :a 2.0 :b 3.0)
                    (list (pair-b x) (pair-a x))))))

(def-test make-return ()
  (is-true (every #'=
                  '(3.0 2.0)
                  (with-zero-cost-struct
                      (y pair)
                      (with-zero-cost-struct (x pair)
                                             (make-pair :a 2.0 :b 3.0)
                        x)
                    (list (pair-b y) (pair-a y))))))

(def-test make-write-return ()
  (is-true (every #'=
                  '(5.0 7.0)
                  (with-zero-cost-struct
                      (y pair)
                      (with-zero-cost-struct (x pair)
                                             (make-pair :a 2.0 :b 3.0)
                        (funcall #'(setf pair-a) 7.0 x)
                        (funcall #'(setf pair-b) 5.0 x)
                        x)
                    (list (pair-b y) (pair-a y))))))
