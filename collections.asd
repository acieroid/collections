; vim: ft=lisp

(asdf:defsystem collections
  :name "collections"
  :author "Quentin Stievenart <quentin.stievenart@gmail.com>"
  :version "0.0"
  :licence "BSD"
  :description "A web interface to manage collections."
  :depends-on (:aserve :clsql-sqlite3 :ironclad)
  :components ((:file "package")
               (:file "database")
               (:file "users" :depends-on ("database"))
               (:file "collections" :depends-on ("database"))))

