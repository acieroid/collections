(in-package :collections)

(define-condition an-error (error)
  ((error-type :initform "(not set)" :initarg :type :reader error-type)
   (description :initform "(not set)" :initarg :description
                :reader description))
  (:documentation "A general class for any error, with a type and description")
  (:report (lambda (e stream)
             (format stream
                     "Error not handled!~%Type: ~a~%Description: ~a"
                     (error-type e) (description e)))))
(defun launch-error (type description)
  "Launch an error of the AN-ERROR class"
  (declaim (inline launch-error))
  (error 'an-error :type type :description description))


