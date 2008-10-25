;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(cl:in-package :cl-user)

;;; try to load asdf-system-connections
(eval-when (:compile-toplevel :load-toplevel :execute)
  (flet ((try (system)
           (unless (asdf:find-system system nil)
             (warn "Trying to install required dependency: ~S" system)
             (when (find-package :asdf-install)
               (funcall (read-from-string "asdf-install:install") system))
             (unless (asdf:find-system system nil)
               (error "The ~A system requires ~A." (or *compile-file-pathname* *load-pathname*) system)))
           (asdf:operate 'asdf:load-op system)))
    (try :asdf-system-connections)))

(defpackage #:cl-yalog-system
  (:use :cl :asdf)
  (:export #:*load-as-production-p*))

(in-package #:cl-yalog-system)

(defvar *load-as-production-p* t)

(defsystem :cl-yalog
  :version "0.1"
  :description "cl-yet-another-logger, based on the logging code from Arnesi"
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
             (:file "yalog" :depends-on ("duplicates" "package"))
             (:file "appenders" :depends-on ("yalog"))))))

(defsystem-connection cl-yalog-and-bordeaux-threads
  :requires (:cl-yalog :bordeaux-threads)
  :components
  ((:module "src"
            :components ((:file "appenders-with-threading")))))

(defsystem :cl-yalog-test
  :description "Tests for cl-yalog."
  :depends-on (:cl-yalog
               :stefil
               :iterate
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
