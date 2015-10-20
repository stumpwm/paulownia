(in-package :cl-user)
(defpackage timers-test
  (:use :cl
	:paulownia
	:prove))
(in-package :timers-test)
(plan nil)

(is (paulownia::get-next-timeout paulownia::*timer-list*) nil)

(defvar *test-timer* (run-with-timer 10 nil (lambda ()
					     (format t "Hello ~%")) 
				     nil))
(isnt (paulownia::schedule-timer *test-timer* 10) nil)
(isnt (paulownia::get-next-timeout paulownia::*timer-list*) nil)
(is (cancel-timer *test-timer*) nil)

(finalize)
