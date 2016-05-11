;; Defines the generic methods for gui interaction so that paulownia
;; can call out to various constructs without worrying about how they
;; are created and rendered.  paulownia provides its own
;; implementation in xlib, but others can be written in any gui
;; framework available in common lisp
(in-package :paulownia)
(export '(message
	  ))

(defgeneric message (control-string &rest args)
  (:documentation "Run @var{control-string} and @{args} through
  `format' and echo the result on the screen. "))
(defgeneric restarts-menu (err)
  (:documentation "Display a menu with the active restarts and let the
user pick one. @var{err} is the error being recovered from. If the
user aborts the menu, the error is re-signaled."))

(defmacro with-restarts-menu (&body body)
  "Execute BODY. If an error occurs allow the user to pick a
restart from a menu of possible restarts. If a restart is not
chosen, resignal the error."
  (let ((c (gensym)))
    `(handler-bind
         ((warning #'muffle-warning)
          ((or serious-condition error)
           (lambda (,c)
             (restarts-menu ,c)
             (signal ,c))))
       ,@body)))

(defmethod message (control-string &rest args)
  "Dummy method flushes message to stdout rather than displaying it in
a gui window"
  (apply 'format t control-string args))
