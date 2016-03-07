(in-package :cl-user)
(defpackage hooks-test
  (:use :cl
	:paulownia
	:prove))
(in-package :hooks-test)
(plan nil)
(defvar *test-hook* '()
  "A test hook")
(defun test-fn ()
  (format nil "Hi!"))
(is (add-hook *test-hook* 'test-fn) (list 'test-fn))
(is (remove-hook *test-hook* 'test-fn) nil)
(add-hook *test-hook* 'test-fn)

;; This doesn't really capture all the states that the run-hook code
;; can be in. Hopefully the other tests will exercise the parts of the
;; code this doesn't
(is (run-hook *test-hook*) nil)


(finalize)
