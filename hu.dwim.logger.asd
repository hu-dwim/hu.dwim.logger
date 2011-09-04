;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.logger
  :class hu.dwim.system
  :description "Generic purpose logger utility."
  :depends-on (:bordeaux-threads
               :hu.dwim.def.namespace
               :hu.dwim.def+hu.dwim.common
               :hu.dwim.defclass-star+hu.dwim.def
               :hu.dwim.util
               :hu.dwim.util.threads
               #-allegro :iolib.os
               :local-time
               :trivial-garbage)
  :components ((:module "source"
                :components ((:file "api" :depends-on ("package"))
                             (:file "appender" :depends-on ("api" "formatter" "logger"))
                             (:file "formatter" :depends-on ("api" "logger"))
                             (:file "logger" :depends-on ("api"))
                             (:file "package")
                             (:file "root-logger" :depends-on ("logger" "api" "appender"))))))
