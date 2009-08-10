(defpackage :collections
  (:use :cl 
        :net.aserve :net.html.generator
        :clsql
        :ironclad
        )
  (:shadowing-import-from :ironclad :NULL)
  )

;;; fix from
;;; http://stackoverflow.com/questions/928163/allegroserve-on-sbcl-1-0-28-failing-with-accept-invalid-keyword-argument-auto
(defmethod sb-bsd-sockets:socket-make-stream ((socket sb-bsd-sockets:socket)
                               &key input output
                               (element-type 'character)
                               (buffering :full)
                               (external-format :default)
                               timeout
                           (auto-close t))
  "Default method for SOCKET objects.  An ELEMENT-TYPE of :DEFAULT
will construct a bivalent stream.  Acceptable values for BUFFERING
are :FULL, :LINE and :NONE.  Streams will have no TIMEOUT
by default.
  The stream for SOCKET will be cached, and a second invocation of this
method will return the same stream.  This may lead to oddities if this
function is invoked with inconsistent arguments \(e.g., one might request
an input stream and get an output stream in response\)."
  (let ((stream
         (and (slot-boundp socket 'stream) (slot-value socket 'stream))))
    (unless stream
      (setf stream (sb-sys:make-fd-stream
                    (sb-bsd-sockets:socket-file-descriptor socket)
                    :name "a socket"
                    :dual-channel-p t
                    :input input
                    :output output
                    :element-type element-type
                    :buffering buffering
                    :external-format external-format
                    :timeout timeout
            :auto-close auto-close)))
      (setf (slot-value socket 'stream) stream)
    (sb-ext:cancel-finalization socket)
    stream))

