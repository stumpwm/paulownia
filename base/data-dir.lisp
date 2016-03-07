(defun redirect-all-output (file)
  "Elect to redirect all output to the specified file. For instance,
if you want everything to go to ~/.stumpwm.d/debug-output.txt you would
do:

@example
(redirect-all-output (data-dir-file \"debug-output\" \"txt\"))
@end example
"
  (when (typep *redirect-stream* 'file-stream)
    (close *redirect-stream*))
  (setf *redirect-stream* (open file :direction :output :if-exists :append :if-does-not-exist :create)
        *error-output*    *redirect-stream*
        *standard-output* *redirect-stream*
        *trace-output*    *redirect-stream*
        *debug-stream*    *redirect-stream*))

(defvar *data-dir* nil
  "The directory used by stumpwm to store data between sessions.")

(defun data-dir-file (name &optional type)
  "Return a pathname inside stumpwm's data dir with the specified name and type"
  (ensure-directories-exist *data-dir*)
  (make-pathname :name name :type type :defaults *data-dir*))

(defmacro with-data-file ((s file &rest keys &key (if-exists :supersede) &allow-other-keys) &body body)
  "Open a file in StumpWM's data directory. keyword arguments are sent
directly to OPEN. Note that IF-EXISTS defaults to :supersede, instead
of :error."
  (declare (ignorable if-exists))
  `(progn
     (ensure-directories-exist *data-dir*)
     (with-open-file (,s ,(merge-pathnames file *data-dir*)
                         ,@keys)
       ,@body)))
