;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(cl:in-package :cl-user)

(defpackage #:cl-yalog-system
  (:use :cl :asdf)
  (:export #:*load-as-production-p*))

(in-package #:cl-yalog-system)

(defvar *load-as-production-p* t)

(defsystem :cl-yalog
  :version "0.1"
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
	   "Tamás Borbély <tomi.borbely@gmail.com>"
	   "Levente Mészáros <levente.meszaros@gmail.com>")
  :maintainer ("Attila Lendvai <attila.lendvai@gmail.com>"
               "Tamás Borbély <tomi.borbely@gmail.com>"
	       "Levente Mészáros <levente.meszaros@gmail.com>")
  :licence "BSD / Public domain"
  :depends-on (:alexandria
               )
  :components
  ((:module "src"
            :components
            ((:file "package")
             (:file "duplicates" :depends-on ("package"))
             (:file "yalog" :depends-on ("duplicates" "package"))))))

(defsystem :cl-yalog-test
  :description "Tests for cl-yalog."
  :default-component-class cl-source-file-with-readtable
  :class system-with-readtable
  :setup-readtable-function "cl-yalog::setup-readtable"
  :depends-on (:cl-yalog
               :stefil
               :cl-def
               :cl-syntax-sugar
               )
  :components
  ((:module :test
	    :components
            ((:file "package")
             (:file "suite" :depends-on ("package"))
             (:file "simple" :depends-on ("suite"))))))

(defmethod perform ((op test-op) (system (eql (find-system :cl-yalog))))
  (operate 'load-op :cl-yalog-test)
  (in-package :cl-yalog-test)
  (pushnew :debug *features*)
  (declaim (optimize (debug 3)))
  (warn "Pushed :debug in *features* and (declaim (optimize (debug 3))) was issued to help later C-c C-c'ing")
  (eval (read-from-string "(progn
                             (stefil:funcall-test-with-feedback-message 'test))"))
  (values))

(defmethod operation-done-p ((op test-op) (system (eql (find-system :cl-yalog))))
  nil)
