;; Defines the generic methods for gui interaction so that paulownia
;; can call out to various constructs without worrying about how they
;; are created and rendered.  paulownia provides its own
;; implementation in xlib, but others can be written in any gui
;; framework available in common lisp

(defgeneric message (control-string &rest args)
  (:documentation "Run @var{control-string} and @{args} through
  `format' and echo the result on the screen. "))

(defmethod message (control-string &rest args)
  "Dummy method flushes message to stdout rather than displaying it in
a gui window"
  (apply 'format t control-string args))
