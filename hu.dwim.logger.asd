;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.logger
  :class hu.dwim.system
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
           "Levente Mészáros <levente.meszaros@gmail.com>"
           "Tamás Borbély <tomi.borbely@gmail.com>")
  :licence "BSD / Public domain"
  :description "Logger"
  :depends-on (:bordeaux-threads
               :hu.dwim.def.namespace
               :hu.dwim.def+hu.dwim.common
               :hu.dwim.defclass-star+hu.dwim.def
               :hu.dwim.util
               :hu.dwim.util.threads
               :iolib.os
               :local-time
               :trivial-garbage)
  :components ((:module "source"
                :components ((:file "appender" :depends-on ("logger"))
                             (:file "logger" :depends-on ("package"))
                             (:file "package")
                             (:file "standard-logger" :depends-on ("logger"))))))
