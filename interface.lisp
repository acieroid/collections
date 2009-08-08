(in-package :collections)

;;;; Some stuff to help defining new pages

(defmacro handle-errors (&body body)
  `(handler-case
       (progn ,@body)
     (an-error (e) (error-page
                    (format nil "An error has occured during ~a : ~a"
                            (error-type e) (description e))))
     (simple-error () (error-page
                       (format nil "An unknown error happened")))))

(defmacro let-after-fun (binds fun &body body)
  "Apply the function FUN to a quote of every element of BINDS
   and bind the result to the symbol"
  `(let ,(mapcar #'(lambda (x)
		   `(,x (,fun ',x)))
		 binds)
     ,@body))

(defmacro with-gensyms (syms &body body)
  "The well-known WITH-GENSYMS macro, using LET-AFTER-FUN.
  See On Lisp by Paul Graham, page 145 for a normal definition"
  `(let-after-fun ,syms (lambda (x)
                          (declare (ignore x))
                          (gensym))
     ,@body))

(defmacro add-page (path params &body body)
  "Publish a page easily"
  (with-gensyms (req ent)
  `(publish :path ,path :content-type "text/html"
            :function
            (lambda (,req ,ent)
              (let-after-fun ,params
                             (lambda (x)
                               (request-query-value
                                (string-downcase (string x)) ,req))
                (with-http-response (,req ,ent)
                  (with-http-body (,req ,ent)
                    ,@body)))))))

(defmacro define-page (name params (&key (title "No title")) &body body)
  `(defun ,name ,params
     (html
      (:html
       (:head (:title ,title))
       (:body ,@body)))))

;;;; defined as a macro because the body contains
;;;; html's specials form
(defmacro standard-page (title &body body)
  `(html
    (:html
     (:head (:title ,title))
     (:body ,@body))))

(define-page error-page (reason) (:title "Error") 
  (:h1 "Error")
  (:p (:princ-safe reason)))

(define-page info-page (info) (:title (:princ-safe info))
  (:p (:princ-safe info)))

(defmacro user-pass-form (page submit-value)
  "A form with username and password fields"
  `(standard-page ,submit-value
     ((:form :action ,page :method "post")
      "Username : " ((:input :type "text"
                             :name "name"
                             :maxlength "20"))
      "Password : " ((:input :type "password"
                             :name "password"
                             :maxlength "20"))
      ((:input :type "submit" :value ,submit-value)))))

;;;; The interface's function start here

;;;; Pages related to authentification
(add-page "/register" (name password)
  (if (and name password)
      ;; registration
      (handle-errors
        (register-user name password)
        (info-page "You're now registered, welcome !"))
      ;; form
      (user-pass-form "register" "Register")))

(add-page "/login" (name password)
  (if (and name password)
      (handle-errors
        (login-user name password)
        (info-page "You're now logged"))
      (user-pass-form "login" "Login")))

; pages related to the collection
(add-page "/list" ()
  (standard-page "List"
    (dolist (element (get-all-elements))
      (html (:h2 (:princ-safe (name element)))
            (:p "Score " (:princ-safe (score element))
                ((:a href (concatenate
                           'string "vote?id=" (write-to-string (id element))))
                 "(+1)"))))))
;(add-page "/vote" (id)
;  (if id
;      (vote-for 

      

