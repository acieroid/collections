(in-package :collections)

;;;; Some stuff to help defining new pages

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
  (:p "An error as occured : " (:b (:princ-safe reason))))

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
(add-page "/register.html" (name password)
  (if (and name password)
      ;; registration
      (handler-case 
          (progn
            (register-user name password)
            (info-page "You're now registered, welcome !"))
        (registration-error (err) (error-page 
                                   (concatenate 'string
                                                "Error when registrating : "
                                                (reason err)))))
      ;; form
      (user-pass-form "register.html" "Register")))

(add-page "/login.html" (name password)
  (if (and name password)
      (handler-case
          (progn
            (login-user name password)
            (info-page "You're now logged"))
        (login-error (err)
          (error-page (concatenate 'string "error during login, "
                                   (reason err)))))
      (user-pass-form "login.html" "Login")))



       
          
            
      