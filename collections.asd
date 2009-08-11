; vim: ft=lisp

(asdf:defsystem collections
  :name "collections"
  :author "Quentin Stievenart <quentin.stievenart@gmail.com>"
  :version "0.0"
  :licence "BSD"
  :description "A web interface to manage collections."
  :depends-on (:aserve :clsql-sqlite3 :ironclad)
  :components ((:file "package")
               (:file "utilities")
               (:file "errors"
                      :depends-on ("package"))
               (:file "collections"
                      :depends-on ("package"
                                   "database"))
               (:file "database"
                      :depends-on ("package"))
               (:file "interface"
                      :depends-on ("package"
                                   "errors"
                                   "utilities"
                                   "classes"))
               (:file "classes"
                      :depends-on ("database"
                                   "package"
                                   "errors"
                                   "utilities"))))

