;; Defines the generic methods for interacting with a display server.
;; Signatures are modeled after clx/xlib, but as long as the
;; equivalent is implemented in the native OS's language with the same
;; function signature, paulownia can operate
(in-package :paulownia)
(export '(*last-unhandled-error*))
(defvar *last-unhandled-error* nil
  "If an unrecoverable error occurs, this variable will contain the
  condition and the backtrace.")

(defun parse-display-string (display)
  "Parse an X11 DISPLAY string and return the host and display from it."
  (ppcre:register-groups-bind (protocol host ('parse-integer display screen))
			      ("^(?:(.*?)/)?(.*?)?:(\\d+)(?:\\.(\\d+))?" display :sharedp t)
    (values 
     ;; clx doesn't like (vector character *)
     (coerce (or host "")
	     '(simple-array character (*)))
     display screen
     (cond (protocol
	    (intern1 protocol :keyword))
	   ((or (string= host "")
		(string-equal host "unix"))
	    :local)
	   (t :internet)))))


(defun error-handler (display error-key &rest key-vals &key asynchronous &allow-other-keys)
  "Handle X errors"
  (cond 
    ;; ignore asynchronous window errors
    ((and asynchronous
          (find error-key '(xlib:window-error xlib:drawable-error xlib:match-error)))
     ;; (dformat 4 "Ignoring error: ~s~%" error-key)
     )
    ((eq error-key 'xlib:access-error)
     (write-line "Another window manager is running.")
     (throw :top-level :quit))
     ;; all other asynchronous errors are printed.
     ;; (asynchronous
     ;;  (message "Caught Asynchronous X Error: ~s ~s" error-key key-vals))
     (t
      (apply 'error error-key :display display :error-key error-key key-vals))))
