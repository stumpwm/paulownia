(in-package :cl-user)
(defpackage paulownia-test
  (:use :cl
	:paulownia
	:prove))
(in-package :paulownia-test)
(plan nil)
;; Check if rc file exists, and if so don't touch it
(let ((rc-file (merge-pathnames #p".paulowniarc" 
					    (user-homedir-pathname))))
(if (probe-file rc-file)
    ;; one of the user's rc file(s) exists so we expect to be able to
    ;; load it without errors, and that the file we got back isn't nil
    (multiple-value-bind (success error file) (load-rc-file)
      (ok success)
      (ok (not error))
      (ok file))
    (progn
      ;; First test when the rc file doesn't exist 
      (multiple-value-bind (success error file) (load-rc-file t)
	(ok success)
	(ok (not error))
	(is file nil))
      ;; now make one and say that we'll deal with the errors
      (with-open-file (stream rc-file
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
	(format stream "foo ~%"))
      (multiple-value-bind (success error file) (load-rc-file t)
	(ok (not success))
	(isnt error nil)
	(is file rc-file))
      ;; now delete it and make another one that is a valid lisp
      ;; program
      (uiop:delete-file-if-exists rc-file)
      (with-open-file (stream rc-file
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
      	(format stream "(eq 'foo 'foo) ~%"))
      (multiple-value-bind (success error file) (load-rc-file t)
      	(ok success)
      	(is error nil)
      	(is file rc-file))
    ;; file didn't exist so remove it
    (uiop:delete-file-if-exists rc-file))))

(finalize)
