(in-package :paulownia)

(export '(*debug-level*
	  *debug-expose-events*
	  *debug-stream*
	  dformat))


(defvar *debug-level* 0
  "Set this variable to a number > 0 to turn on debugging. The greater the number the more debugging output.")

(defvar *debug-expose-events* nil
  "Set this variable for a visual indication of expose events on internal StumpWM windows.")

(defvar *debug-stream* *error-output*
  "This is the stream debugging output is sent to. It defaults to
*error-output*. It may be more convenient for you to pipe debugging
output directly to a file.")

(defun dformat (level fmt &rest args)
  (when (>= *debug-level* level)
    (multiple-value-bind (sec m h) (decode-universal-time (get-universal-time))
      (format *debug-stream* "~2,'0d:~2,'0d:~2,'0d " h m sec))
    ;; strip out non base-char chars quick-n-dirty like
    (write-string (map 'string (lambda (ch)
                                 (if (typep ch 'standard-char)
                                     ch #\?))
                       (apply 'format nil fmt args))
                  *debug-stream*)
    (force-output *debug-stream*)))
