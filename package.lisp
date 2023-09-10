(defpackage :zero-cost-structures
  (:use :cl :cl-environments)
  (:export #:define-zero-cost-struct
           #:with-zero-cost-struct
           #:with-zero-cost-structs)
  (:import-from #:alexandria
                #:symbolicate
                #:with-gensyms
                #:nconcf))

(in-package :zero-cost-structures)

(defvar *zero-cost-structs* (make-hash-table))
(defun zero-cost-struct (name &optional error-if-not-exists)
  (multiple-value-bind (definition existsp)
      (gethash name *zero-cost-structs*)
    (cond (existsp
           definition)
          (error-if-not-exists
           (error "No zero-cost-struct with name ~S" name)))))
(defun (setf zero-cost-struct) (definition name)
  (cond (definition
         (setf (gethash name *zero-cost-structs*) definition))
        (t
         (remhash name *zero-cost-structs*))))

(define-declaration zero-cost-struct (args)
  (destructuring-bind (var type &rest slot-symbols) args
    (values :variable
            (list (list var 'zero-cost-struct (list* type slot-symbols))))))

(defun normalizes-slot-descriptions (slot-descriptions)
  (loop :for description :in slot-descriptions
        :do (setq description
                  (etypecase description
                    (symbol (list description nil))
                    (list description)))
            (unless (getf description :type)
              (nconcf description
                      (list :type cl:t)))
            (unless (getf description :read-only)
              (nconcf description
                      (list :read-only nil)))
        :collect description))

(defun normalize-name (name)
  ;; TODO: Parse when NAME is a list
  (values name
          (symbolicate 'make '- name)
          (symbolicate name '-)))

(define-condition no-with-zero-cost-struct-variable ()
  ((accessor-name :initarg :accessor-name))
  (:report (lambda (c s)
             (with-slots (accessor-name) c
               (format s "The argument to ~S should be a variable defined
using a WITH-ZERO-COST-STRUCT form.

If it is indeed such a variable, then may be the implementation
does not have good support for DEFINE-DECLARATION since no
information for the user-defined declaration ZERO-COST-STRUCT
was found.

Alternatively, if this is a runtime error, then the compiler
macro was not expanded."
                       accessor-name)))))



(defun accessor-definitions
    (conc-name slot-position slot-name type read-only)

  (let ((accessor-name (symbolicate conc-name slot-name)))

    (with-gensyms (env variable-type localp decl
                       zero-cost-struct-info
                       new-value-form)

      `((defun ,accessor-name (zero-cost-struct)
          (declare (ignore zero-cost-struct))
          (error 'no-with-zero-cost-struct-variable
                 :accessor-name ',accessor-name))
        (define-compiler-macro ,accessor-name
            (zero-cost-struct-var &environment ,env)
          (check-type zero-cost-struct-var symbol)
          (multiple-value-bind (,variable-type ,localp ,decl)
              (variable-information zero-cost-struct-var ,env)
            (let ((,zero-cost-struct-info
                    (cdr (assoc 'zero-cost-struct ,decl))))
              (if (and (eq ,variable-type :symbol-macro)
                       ,localp
                       ,zero-cost-struct-info)
                  (the ,type
                       (nth ,slot-position
                            (cdr ,zero-cost-struct-info)))
                  (error 'no-with-zero-cost-struct-variable
                         :accessor-name
                         ',accessor-name)))))

        ,@(unless read-only

            `((defun (setf ,accessor-name)
                  (new-value zero-cost-struct)
                (declare (ignore new-value zero-cost-struct))
                (error 'no-with-zero-cost-struct-variable
                       :accessor-name '(setf ,accessor-name)))
              (define-compiler-macro (setf ,accessor-name)
                  (,new-value-form zero-cost-struct-var
                   &environment ,env)
                (check-type zero-cost-struct-var symbol)
                (multiple-value-bind (,variable-type ,localp ,decl)
                    (variable-information zero-cost-struct-var ,env)
                  (let ((,zero-cost-struct-info
                          (cdr (assoc 'zero-cost-struct ,decl))))
                    (if (and (eq ,variable-type :symbol-macro)
                             ,localp
                             ,zero-cost-struct-info)
                        (list 'setq
                              (nth ,slot-position
                                   (cdr ,zero-cost-struct-info))
                              ,new-value-form)
                        (error 'no-with-zero-cost-struct-variable
                               :accessor-name
                               '(setf ,accessor-name))))))))))))


(defmacro define-zero-cost-struct (name &body slot-descriptions)

  (let ((slot-descriptions (normalizes-slot-descriptions slot-descriptions)))

    (multiple-value-bind (name constructor conc-name) (normalize-name name)

      `(eval-when (:compile-toplevel :load-toplevel :execute)

         (setf (zero-cost-struct ',name) ',slot-descriptions)

         (declaim (inline ,constructor))
         ,(let ((slot-name-defaults
                  (loop :for (name default . rest) :in slot-descriptions
                        :collect (list name default))))
            `(defun ,constructor (&key ,@slot-name-defaults)
               (values ',name
                       ,@(loop :for (name . rest) :in slot-descriptions
                               :collect name))))

         ,@(loop :for desc :in slot-descriptions
                 :nconcing
                 (destructuring-bind (slot-name default
                                      &key read-only type)
                     desc
                   (declare (ignore default))
                   (accessor-definitions conc-name
                                         (position slot-name slot-descriptions
                                                   :key #'first)
                                         slot-name type read-only)))))))



(defmacro with-zero-cost-struct
    ((var type) zero-cost-struct-form &body body)
  (let* ((slots (mapcar #'first (zero-cost-struct type)))
         (slot-symbols (loop :for s :in slots
                             :collect (gensym (string s))))
         (type-sym (gensym "ZERO-COST-STRUCT-TYPE")))
    `(multiple-value-bind (,type-sym ,@slot-symbols) ,zero-cost-struct-form
       (declare (ignorable ,type-sym ,@slot-symbols))
       (symbol-macrolet ((,var (values ,type-sym ,@slot-symbols)))
         (declare (ignorable ,var)
                  (zero-cost-struct ,var ,type ,@slot-symbols))
         ,@body))))

(defmacro with-zero-cost-structs (bindings &body body)
  (if (null bindings)
      `(progn ,@body)
      `(with-zero-cost-struct ,@(first bindings)
         (with-zero-cost-structs ,(rest bindings) ,@body))))

#|

(define-zero-cost-struct pair a b)

(with-zero-cost-struct (x pair) (make-pair :a 2.0 :b 3.0)
  (pair-a x))

(make-pair :a 2.0 :b 3.0)
;=> (values 'pair 2.0 3.0)

(with-zero-cost-struct (x pair)
    (make-pair :a 2.0 :b 3.0)
  (pair-a x)
  (pair-b x))

(with-zero-cost-struct (x pair)
    (make-pair :a 2.0 :b 3.0)
  (let ((x 42.0d0))
    (pair-a x)
    (pair-b x)))
;=> Error

|#
