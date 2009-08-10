(in-package :collections)

;;; Some stuff to help defining new pages
(defmacro handle-errors (&body body)
  `(handler-case
       (progn ,@body)
     (an-error (e) (error-page
                    (format nil "An error has occured during ~a : ~a"
                            (error-type e) (description e))))
     (simple-error () (error-page
                       (format nil "An unknown error happened")))))

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

;;; defined as a macro because the body contains
;;; html's specials form
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

(defmacro format-safe (control-string &rest arguments)
  `(:princ-safe (format nil ,control-string ,@arguments)))

(defun htmlize-element (element)
  (let ((id (write-to-string (id element))))
    (html (:h2
           ((:a href (concatenate 'string "view?id="
                                  id))
            (:princ-safe (name element))))
          (:p (:princ-safe (description element)))
          (:p (format-safe "~a vote~:p" (score element))
              ((:a href (concatenate
                         'string "view?vote&id=" (write-to-string (id element))))
               "(+1)")))))

;;; The interface's function start here

;;; Pages related to authentification
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

;;; pages related to the collection
(add-page "/list" ()
  (standard-page "List"
    (dolist (element (get-all-elements))
      (htmlize-element element))))

(add-page "/view" (id vote)
  (let ((element (get-element-by-id
                  (parse-integer id :junk-allowed t))))
    (if element
        (progn
          (when vote (vote-for element))
          (standard-page (:princ-safe (name element))
            (htmlize-element element)))
        (error-page "bad id specified"))))

(add-page "/add" (name image descr)
  (if (and name image descr)
      (handle-errors
        (let ((el (make-element :name name
                                :image image
                                :description descr)))
          (add-element el))
        (standard-page "Adding an element" "The element has ben added"))
      (standard-page "Adding an element"
          ((:form :action "/add" :method "post")
           "Name : " ((:input :type "text"
                              :name "name"
                              :maxlength "100"))
           (:br)
           "Description : " (:br) ((:textarea
                                    :name "descr"
                                    :rows "15"
                                    :cols "50"))
           (:br)
           "Image path : " ((:input :type "text"
                                    :name "image"
                                    :maxlength "100"))
           (:br)
           ((:input :type "submit" :value "Add"))))))
