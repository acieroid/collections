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
     ))

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

#.(locally-enable-sql-reader-syntax)
(defmethod htmlize ((what item))
  (let ((id (write-to-string (id what))))
    (html (:h2
           ((:a href (concatenate 'string "viewitem?id="
                                  id))
            (:princ-safe (name what))))
          (:p (:princ-safe (description what)))
          (:p (format-safe "~d vote~:p " (votes what))
              ((:a href (concatenate
                         'string "viewitem?vote&id=" id))
               "(+1)")
              (:br)
              ((:a href (concatenate 'string
                                     "viewitem?id=" id))
               (format-safe "~d note~:p"
                            (count-instances
                             [note]
                             [= [slot-value 'note 'item-id]
                                (id what)])))
              " "
              ((:a href (concatenate
                         'string "addnote?itemid=" id))
               "(add)")))))
(defmethod htmlize ((what note))
  (let ((id (write-to-string (id what))))
    (html (:b
           ((:a href (concatenate 'string "viewnote?id="
                                  id))
            (:princ-safe (title what))))
          (:p (:princ-safe (content what)))
          (:p (format-safe "~d vote~:p " (votes what))
              ((:a href (concatenate
                         'string "viewnote?vote&id=" id))
               "(+1)")))))
#.(locally-disable-sql-reader-syntax)                  

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
    (mapcar #'htmlize (get-all-instances 'item))))

(add-page "/viewitem" (id vote)
  (let ((item
         (when id
           (get-instance-by-id
            'item (parse-integer id :junk-allowed t)))))
    (if item
        (handle-errors
          (when vote (vote-for item))
          (standard-page (:princ-safe (name item))
            (htmlize item))
          (mapcar #'htmlize (notes item)))
        (error-page "bad id"))))

(add-page "/viewnote" (id vote)
  (if id
      (let* ((id (parse-integer id :junk-allowed t))
             (note (get-instance-by-id id)))
        (if note
            (handle-errors
             (vote-for note)
             (htmlize note))
            (error-page "bad id")))
      (error-page "bad id")))

(add-page "/additem" (name image descr)
  (if (and name image descr)
      (handle-errors
        (let ((item (make-with-id 'item
                                  :name name
                                  :image image
                                  :description descr)))
          (add-instance item))
        (standard-page "Adding an item" "The item has ben added"))
      (standard-page "Adding an item"
          ((:form :action "/additem" :method "post")
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

(add-page "/addnote" (itemid title content)
  (cond
    ((and itemid title content)
      (handle-errors
        (let ((note (make-with-id 'note
                                  :title title
                                  :item-id
                                   (parse-integer itemid :junk-allowed t)
                                  :content content)))
          (add-instance note))
        (standard-page "Adding a note" "The note has ben added")))
    (itemid
      (standard-page "Adding a note"
          ((:form :action "/addnote" :method "post")
           "Title : " ((:input :type "text"
                               :name "title"
                               :maxlength "100"))
           (:br)
           "Content : " (:br) ((:textarea
                                :name "content"
                                :rows "15"
                                :cols "50"))
           ((:input :type "hidden" :name "itemid" :value itemid))
           (:br)
           ((:input :type "submit" :value "Add")))))
    (t (error-page "No item selected"))))

