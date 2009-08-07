(in-package :collections)

(defmacro let-after-fun (binds fun &body body)
  `(let ,(mapcar #'(lambda (x)
		   `(,x (,fun ',x)))
		 binds)
     ,@body))

(defmacro with-gensyms (syms &body body)
  "The well-known WITH-GENSYMS macro, using LET-AFTER-FUN.
  See On Lisp by Paul Graham, page 145"
  `(let-after-fun ,syms (lambda (x)
                          (declare (ignore x))
                          (gensym))
     ,@body))

(defmacro add-page (path params &body body)
  "Publish a page"
  (with-gensyms (req ent)
  `(publish :path ,path :content-type "text/html"
            :function
            (lambda (,req ,ent)
              (let-after-fun ,params
                             (lambda (x)
                               (request-query-value x ,req))
                (with-http-response (,req ,ent)
                  (with-http-body (,req ,ent)
                    ,@body)))))))

(defmacro define-page (name params (&key (title "No title")) &body body)
  `(defun ,name ,params
     (html
       (:head (:title ,title))
       (:body ,@body))))

(define-page error-page (reason) (:title "Error") 
  (:h1 "Error")
  (:p "An error as occured : " (:princ-safe reason)))

(define-page info-page (info) (:title info)
  (:p info))

(add-page "register.html" (name password)
  (if (and name pwd)
    ;; registration
    (handler-case 
      (progn
        (register-user name pwd)
        (info-page "You're now registered, welcome !"))
      (registration-error (err) (error-page 
                                  (concatenate 'string
                                               "Error when registrating : "
                                               (reason err)))))
    ;; form
    (standard-page
      ((:form :action "register.html" :method "post")
       "Username : " ((:input :type "text"
                              :name "name"
                              :maxlength "20"))
       "Password : " ((:input :type "password"
                              :name "password"
                              :maxlength "20"))
       ((:input :type "submit" :value "Register"))))))
