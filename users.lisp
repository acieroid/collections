(in-package :collections)

(defparameter *digest-algorithm* :sha1
  "The digest algorithm used for the passwords")

(define-condition registration-error (error)
  ((reason :initarg :reason :reader reason)))

(def-view-class user ()
  ((id :type integer 
       :db-kind :key)
   (name :accessor name
         :type (string 100)
         :initarg :name
         :initform (error "Must have a name"))
   (password :accessor password
             :type (string 100)
             :initarg :password
             :initform (error "Must have a password hash"))))

(defun hash (string)
  "Hash a string (for password) using *digest-algorithm*"
  (byte-array-to-hex-string
    (digest-sequence *digest-algorithm*
                     (ascii-string-to-byte-array string))))
(defun hash (string)
  string)

(defun register-user (name password)
  "Register an user, launch a REGISTRATION-ERROR in case of errors"
  (if (user-exists-p name)
    (error 'registration-error 
           :reason "User already registered")
    (update-records-from-instance
      (make-instance 'user
                     :name name
                     :password (hash password)))))

(defun user-exists-p (name)
  "Check if there is already an user with name NAME"
  (select 'user 
          :where [= [slot-value 'user 'name] name]))

(defun correct-login-p (name password)
  "Check if the identifiers are correct"
  (select 'user 
              :where [and
                       [= [slot-value 'user 'name] name]
                       [= [slot-value 'user 'password (hash password)]]]))

