(in-package :collections)

(defparameter *db-name* "collections.db"
  "The name of the database")

(defun connect-db ()
  (unless (find-if (lambda (x) (string= (database-name x)*db-name*))
                   (connected-databases))
    (connect (list *db-name*) :database-type :sqlite3)))

(defun create-tables (&rest tables)
  (mapcar (lambda (table) (unless (table-exists-p table)
                            (create-view-from-class table)))
          tables))
