(in-package :collections)

(def-view-class element ()
  ((id :type integer
       :db-kind :key
       :initarg :id
       :accessor id)
   (name :accessor name
         :type (string 100)
         :initarg :name)
   (image :accessor image
          :type (string 100)
          :initarg :image
          :initform nil)
   (score :accessor score
          :type integer
          :initform 0)))

#.(locally-enable-sql-reader-syntax)
(defun get-element-by-id (id)
  (caar (select 'element :from [element]
                :where [= [slot-value 'element 'id] id]
                :refresh t)))

(defun get-all-elements ()
  "Return a list of all the elements"
  ; must loop because select return each element in a list
  (loop for i in (select 'element :from [element] :refresh t)
       collect (car i)))
#.(restore-sql-reader-syntax-state)

(defmethod add-element ((el element))
  "Add an element to the database"
  (update-records-from-instance el))

(defun vote-for-id (id)
  "Vote for an element with its id"
  (let ((element (get-element-by-id id)))
    (incf (score element))
    (update-records-from-instance element)))

(defmacro count-elements (&rest args)
  "Count the elements that match some patterns given by args (clsql's syntax)"
  `(caar (select [count [*]] :from [element]
                 ,@(when args `(:where ,@args)))))

(defmacro make-element (&rest args)
  "Shortcut for (make-instance 'element), calculates the correct id"
  `(make-instance 'element ,@args :id (1+ (length (get-all-elements)))))

