(in-package :paulownia)
(export '(error-handler 
	  parse-display-string
	  load-rc-file))

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

(defun load-rc-file (&optional (catch-errors t))
  "Load the user's .paulowniarc file or the system wide one if that
doesn't exist. Returns a values list: whether the file loaded (t if no
rc files exist), the error if it didn't, and the rc file that was
loaded. When CATCH-ERRORS is nil, errors are left to be handled
further up. "
  (let* ((xdg-config-dir
	  (let ((dir (uiop:getenv "XDG_CONFIG_HOME")))
	    (if (or (not dir) (string= dir ""))
		(merge-pathnames  #p".config/" (user-homedir-pathname))
		dir)))
         (user-rc
	  (probe-file (merge-pathnames #p".paulowniarc" (user-homedir-pathname))))
         (dir-rc
	  (probe-file (merge-pathnames #p".paulownia.d/init.lisp" (user-homedir-pathname))))
         (conf-rc
	  (probe-file (merge-pathnames #p"paulownia/config" xdg-config-dir)))
         (etc-rc (probe-file #p"/etc/paulowniarc"))
         (rc (or user-rc dir-rc conf-rc etc-rc)))
    (if rc
        (if catch-errors
            (handler-case (load rc)
              (error (c) (values nil (format nil "~a" c) rc))
              (:no-error (&rest args) (declare (ignore args)) (values t nil rc)))
            (progn
              (load rc)
              (values t nil rc)))
        (values t nil nil))))

(defun intern1 (thing &optional (package *package*) (rt *readtable*))
  "A DWIM intern."
  (intern
   (ecase (readtable-case rt)
     (:upcase (string-upcase thing))
     (:downcase (string-downcase thing))
     ;; Prooobably this is what they want? It could make sense to
     ;; upcase them as well.
     (:preserve thing)
     (:invert (string-downcase thing)))
   package))

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

