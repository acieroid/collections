(in-package :collections)

;;; Some functions that can help
(defun string-void-p (str)
  (if (string= str "")
      t
      (and (char= (elt str 0) #\Space)
           (void-string (subseq str 1)))))
      
(defun string-has-spaces-p (str)
  (find-if (lambda (x) (char= x #\Space)) str))
