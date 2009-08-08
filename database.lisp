(in-package :collections)

(defparameter *db-name* "collections.db"
  "The name of the database")

(defun connect-db ()
  (connect (list *db-name*) :database-type :sqlite3))

(defun create-tables ()
  (create-view-from-class 'element)
  (create-view-from-class 'user))




