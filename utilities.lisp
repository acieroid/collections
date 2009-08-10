(in-package :collections)

(defun string-void-p (str)
  "Return T if STR contains nothing or just spaces"
  (if (string= str "")
      t
      (and (char= (elt str 0) #\Space)
           (void-string (subseq str 1)))))
      
(defun string-has-spaces-p (str)
  "Return non-NIL if STR contains spaces"
  (find-if (lambda (x) (char= x #\Space)) str))

(defmacro let-after-fun (binds fun &body body)
  "Apply the function FUN to a quote of every element of BINDS
   and bind the result to the symbol"
  `(let ,(mapcar #'(lambda (x)
		   `(,x (,fun ',x)))
		 binds)
     ,@body))

(defmacro with-gensyms (syms &body body)
  "The well-known WITH-GENSYMS macro, using LET-AFTER-FUN.
  See On Lisp by Paul Graham, page 145 for a normal definition"
  `(let-after-fun ,syms (lambda (x)
                          (declare (ignore x))
                          (gensym))
     ,@body))
