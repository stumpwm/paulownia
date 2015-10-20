(in-package :paulownia)

(export '(run-with-timer
	  cancel-timer
	  timer-p))

(defvar *timer-list* nil
  "List of active timers.")

(defstruct timer
  time repeat function args)

(defun run-with-timer (secs repeat function &rest args)
  "Perform an action after a delay of SECS seconds.
Repeat the action every REPEAT seconds, if repeat is non-nil.
SECS and REPEAT may be reals.
The action is to call FUNCTION with arguments ARGS."
  (check-type secs (real 0 *))
  (check-type repeat (or null (real 0 *)))
  (check-type function (or function symbol))
  (let ((timer (make-timer
                :repeat repeat
                :function function
                :args args)))
    (schedule-timer timer secs)
    (setf *timer-list* (merge 'list *timer-list* (list timer) #'< :key #'timer-time))
    timer))

(defun cancel-timer (timer)
  "Remove TIMER from the list of active timers."
  (check-type timer timer)
  (setf *timer-list* (remove timer *timer-list*)))

(defun schedule-timer (timer when)
  (setf (timer-time timer) (+ (get-internal-real-time)
                              (* when internal-time-units-per-second))))

(defun run-expired-timers ()
  (let ((now (get-internal-real-time))
	(timers *timer-list*)
	(pending '())
	(remaining '()))
    (setf *timer-list*
	  (dolist (timer timers (sort remaining #'< :key #'timer-time))
	    (if (<= (timer-time timer) now)
		(progn (push timer pending)
		       (when (timer-repeat timer)
			 (schedule-timer timer (timer-repeat timer))
			 (push timer remaining)))
		(push timer remaining))))
    (dolist (timer pending)
      (apply (timer-function timer) (timer-args timer)))))

(defun get-next-timeout (timers)
  "Return the number of seconds until the next timeout or nil if there are no timers."
  (when timers
    (max (/ (- (timer-time (car timers)) (get-internal-real-time))
            internal-time-units-per-second)
         0)))
