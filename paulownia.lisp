(in-package :paulownia)
(export '(paulownia
	  *last-unhandled-error*
          *display*
	  load-rc-file))
(defvar *display* nil
  "The display for the X server")
(defvar *last-unhandled-error* nil
  "If an unrecoverable error occurs, this variable will contain the
  condition and the backtrace.")
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

(defun paulownia-internal (display-str)
  (multiple-value-bind (host display screen protocol) (parse-display-string display-str)
    (declare (ignore screen))
    (setf *display* (xlib:open-display host :display display :protocol protocol)
          (xlib:display-error-handler *display*) 'error-handler)
    (with-simple-restart (quit-paulownia "Quit Paulownia")
      ;; In the event of an error, we always need to close the display
      (unwind-protect
           (progn
             (let ((*initializing* t))
               ;; we need to do this first because init-screen grabs keys
               ;; (update-modifier-map)
               ;; Initialize all the screens
               ;; (setf *screen-list* (loop for i in (xlib:display-roots *display*)
               ;;                        for n from 0
               ;;                        collect (init-screen i n host)))
               ;; (xlib:display-finish-output *display*)
               ;; Load rc file
               ;; (let ((*package* (find-package *default-package*)))
               ;;   (multiple-value-bind (success err rc) (load-rc-file)
               ;;     (if success
               ;;         (and *startup-message* (message *startup-message* (print-key *escape-key*)))
               ;;         (message "^B^1*Error loading ^b~A^B: ^n~A" rc err))))
               ;; (when *last-unhandled-error*
               ;;   (message-no-timeout "^B^1*Paulownia Crashed With An Unhandled Error!~%Copy the error to the clipboard with the 'copy-unhandled-error' command.~%^b~a^B^n~%~%~a"
               ;;            (first *last-unhandled-error*) (second *last-unhandled-error*)))
               ;; (mapc 'process-existing-windows *screen-list*)
               ;; We need to setup each screen with its current window. Go
               ;; through them in reverse so the first screen's frame ends up
               ;; with focus.
               ;; (dolist (s (reverse *screen-list*))
               ;;   ;; map the current group's windows
               ;;   (mapc 'unhide-window (reverse (group-windows (screen-current-group s))))
               ;;   ;; update groups
               ;;   (dolist (g (reverse (screen-groups s)))
               ;;     (dformat 3 "Group windows: ~S~%" (group-windows g))
               ;;     (group-startup g))
               ;;   ;; switch to the (old) current group.
               ;;   (let ((netwm-id (first (xlib:get-property (screen-root s) :_NET_CURRENT_DESKTOP))))
               ;;     (when (and netwm-id (< netwm-id (length (screen-groups s))))
               ;;       (switch-to-group (elt (sort-groups s) netwm-id))))
               ;;   (redraw-current-message (current-screen)))
	       )
             ;; Let's manage.
             ;; (let ((*package* (find-package *default-package*)))
             ;;   (run-hook *start-hook*)
             ;;   (paulownia-internal-loop))
	     )
        (xlib:close-display *display*))))
  ;; what should the top level loop do?
  :quit)

(defun paulownia (&optional (display-str (or (getenv "DISPLAY") ":0")))
  "Start the window manager, this is the \"main\" of the program"
  

  ;; Setup variables that need a global state for pauwlonia to run
  ;; Setup the data directory for logging/modules
  (setf *data-dir*
        (make-pathname :directory (append (pathname-directory (user-homedir-pathname))
                                          (list ".paulownia.d"))))
  ;; Setup the load-path for modules
  ;; (init-load-path (merge-pathnames *data-dir* "modules/"))
  
  
  ;; Start the top level loop.  We have to follow the standard unix
  ;; interfaces and respond to events when we're suspended
  ;; (hup-process)
  (loop
     (let ((ret (catch :top-level
                  (paulownia-internal display-str))))
       (setf *last-unhandled-error* nil)
       (cond ((and (consp ret)
                   (typep (first ret) 'condition))
              (format t "~&Caught '~a' at the top level. Please report this.~%~a" 
                      (first ret) (second ret))
              (setf *last-unhandled-error* ret))
             ;; we need to jump out of the event loop in order to hup
             ;; the process because otherwise we get errors.
             ((eq ret :hup-process)
              (apply 'execv (first (argv)) (argv)))
             ((eq ret :restart))
             (t 
              (run-hook *quit-hook*)
              ;; the number is the unix return code
              (return-from paulownia 0)))))  
)
