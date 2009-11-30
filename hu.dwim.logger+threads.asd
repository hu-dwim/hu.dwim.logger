;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.logger+threads
  :class hu.dwim.system
  :depends-on (:bordeaux-threads
               :hu.dwim.logger
               :hu.dwim.util.threads
               :iolib.os
               :trivial-garbage)
  :components ((:module "source"
                :components ((:file "caching-appender")
                             (:file "thread-safe-file-appender" :depends-on ("caching-appender"))))))
