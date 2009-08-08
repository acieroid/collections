(in-package :collections)

(defparameter *db-name* "collections.db"
  "The name of the database")

(defun connect-db ()
  (connect (list *db-name*) :database-type :sqlite3))

(defun create-tables ()
  (mapcar (lambda (table) (unless (table-exists-p table)
                            (create-view-from-class table)))
          '(element user)))

