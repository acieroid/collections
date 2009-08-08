(in-package :collections)

(defun start (&rest args-to-aserve)
  (handler-case
      (progn (connect-db)
             (create-tables)
             (apply #'net.aserve:start args-to-aserve))))