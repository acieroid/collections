(in-package :collections)

(defparameter *db-name* "collections.db"
  "The name of the database")

(defun connect-db ()
  (connect '(*db-name*) :database-type :sqlite3)
  (enable-sql-reader-syntax))

(defun create-tables ()
  (create-view-from-class 'element)
  (create-view-from-class 'user))

