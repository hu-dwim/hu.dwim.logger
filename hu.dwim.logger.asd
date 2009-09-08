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
  :depends-on (:alexandria
               :local-time)
  :components ((:module "source"
                :components ((:file "package")
                             (:file "duplicates" :depends-on ("package"))
                             (:file "logger" :depends-on ("duplicates" "package"))
                             (:file "appenders" :depends-on ("logger"))))))
