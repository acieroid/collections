(in-package :collections)

(defun start-server (&rest args-to-aserve)
  "Establish connection to the database, create table if they don't exists,
   and start aserve (look at NET.ASERVE:START for the arguments)"
  (connect-db)
  (create-tables)
  (apply #'net.aserve:start args-to-aserve))
