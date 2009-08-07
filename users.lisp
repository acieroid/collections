(in-package :collections)

(defparameter *digest-algorithm* :sha1
  "The digest algorithm used for the passwords")

(define-condition registration-error (error)
  ((reason :initarg :reason :reader reason)))
(define-condition login-error (error)
  ((reason :initarg :reason :reader reason)))

(def-view-class user ()
  ((id :type integer 
       :db-kind :key)
   (name :accessor name
         :type (string 100)
         :initarg :name
         )
   (password :accessor password
             :type (string 100)
             :initarg :password
             )))

(defun hash (string)
  "Hash a string (for password) using *digest-algorithm*"
  (byte-array-to-hex-string
    (digest-sequence *digest-algorithm*
                     (ascii-string-to-byte-array string))))
(defun hash (string) ; TODO: ironclad not loading in slime
  string)

(defun user-exists-p (name)
  "Check if there is already an user with name NAME"
  (select 'user 
          :where [= [slot-value 'user 'name] name]))

(defun correct-login-p (name password)
  "Check if the identifiers are correct"
  (select 'user
          :where [and [= [slot-value 'user 'name] name]
                      [= [slot-value 'user 'password] (hash password)]]))

(defun register-user (name password)
  "Register an user, launch a REGISTRATION-ERROR in case of errors"
  (if (user-exists-p name)
    (error 'registration-error 
           :reason "User already registered")
    (update-records-from-instance
      (make-instance 'user
                     :name name
                     :password (hash password)))))
(defun login-user (name password)
  "Login a user, launch a LOGIN-ERROR in case of errors"
  (cond
    ((not (user-exists-p name))
      (error 'login-error
             :reason "User don't exists"))
     ((not (correct-login-p name password))
      (error 'login-error
             :reason "Invalid identifiers"))
     (t ; TODO some stuff here ?
      'ok)))

