(in-package :collections)

(def-view-class element ()
  ((id :type integer
       :db-kind :key)
   (name :accessor name
         :type (string 100)
         :initarg :name
         :initform (error "Must have a name"))
   (image :accessor image
          :type (string 100)
          :initarg :image
          :initform nil)
   (score :accessor score
          :type integer
          :initform 0)))

(defmethod vote-for ((el element))
  "Vote for an element"
  (incf (score el))
  (update-records-from-instance el))

(defmethod add-element ((el element))
  "Add an element to the database"
  (update-records-from-instance el))
