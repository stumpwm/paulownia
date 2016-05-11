(in-package :paulownia)
(export '(load-rc-file))

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
