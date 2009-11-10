;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.logger+threads
  :class hu.dwim.system
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
           "Tamás Borbély <tomi.borbely@gmail.com>"
           "Levente Mészáros <levente.meszaros@gmail.com>")
  :licence "BSD / Public domain"
  :depends-on (:bordeaux-threads
               :hu.dwim.logger
               :iolib.os
               :trivial-garbage)
  :components ((:module "source"
                :components ((:file "caching-appender")
                             (:file "thread-safe-file-appender" :depends-on ("caching-appender"))))))
