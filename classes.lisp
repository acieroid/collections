(in-package :collections)

(def-view-class with-id ()
  ((id :type integer
       :db-kind :key
       :initarg :id
       :reader id)))

(def-view-class votable (with-id)
  ((votes :accessor votes
          :type integer
          :initform 0)))

(def-view-class item (votable)
  ((name :accessor name
         :type (string)
         :initarg :name)
   (description :accessor description
                :type (string)
                :initarg :description
                :initform ""))
   ;; not working :(
   ;; (notes :reader notes
   ;;        :db-kind :join
   ;;        :db-info (:join-class note
   ;;                  :home-key id
   ;;      	    :foreign-key item-id)))
  (:base-table item))

(def-view-class note (votable)
   ((title :accessor title
           :type (string)
           :initarg :title)
    (content :accessor content
             :type (string)
             :initarg :content)
    (item-id :reader item-id
             :type integer
             :initarg :item-id))
   (:base-table note))


(defmethod vote-for ((thing votable))
  (incf (votes thing))
  (update-records-from-instance thing))

#.(locally-enable-sql-reader-syntax)
(defun get-instance-by-id (class id)
  (caar (select class
                :where [= [slot-value class 'id] id]
                :refresh t))) 

(defun get-all-instances (class)
  ;; must loop because select return each element in a list
  (loop for i in (select class :refresh t)
       collect (car i)))

(defmacro count-instances (class &rest args)
  `(caar (select [count [*]] :from ,class
                ,@(when args `(:where ,@args)))))

;;; because clsql's joins don't works
(defmethod notes ((i item))
  (reduce (lambda (x y) (cons (car y) x))
          (select 'note :from [note]
                  :where [= [slot-value 'note 'item-id] (id i)]
                  :refresh t)))
#.(restore-sql-reader-syntax-state)

(defmethod add-instance (instance)
  (update-records-from-instance instance))

(defmethod add-instance ((note note))
  ;; check that the item exists 
  (unless (get-instance-by-id 'item (item-id note))
    (launch-error "adding a note"
                  "bad item"))
  (call-next-method))

(defmacro make-with-id (class &rest args)
  `(make-instance ,class ,@args :id (1+ (length (get-all-instances ,class)))))

