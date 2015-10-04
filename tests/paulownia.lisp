(in-package :cl-user)
(defpackage paulownia-test
  (:use :cl
	:paulownia
	:prove))
(in-package :paulownia-test)
(plan nil)

(ok (not (find 4 '(1 2 3))))
(is 4 4)
(isnt 1 #\1)

(finalize)
