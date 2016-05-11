(in-package :paulownia)


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
