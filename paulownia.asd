;;; -*- Mode: Lisp -*-

(defpackage :paulownia-system
  (:use :cl :asdf :uiop))
(in-package :paulownia-system)

(defsystem :paulownia
  :name "Paulownia"
  :author "David Bjergaard <dbjergaard@gmail.com>, Shawn Betts <sabetts@vcn.bc.ca>"
  :version "2.0.0"
  :maintainer "David Bjergaard <dbjergaard@gmail.com>"
  :license "GNU General Public License"
  :description "A tiling, keyboard driven window manager" 
  :serial t
  :depends-on (:cl-ppcre 
	       #+sbcl :sb-posix
	       :uiop
	       :clx)
  :components ((:file "package")
	       (:file "base/helpers")
	       (:file "base/debug")
	       (:file "base/load-rc")
	       (:file "base/timers")
	       (:file "base/hooks")
	       (:file "base/data-dir")
	       (:file "backends/display-server")
	       (:file "backends/gui")
               ;; keep this last so it always gets recompiled if
               ;; anything changes
               (:file "version")
	       ))

