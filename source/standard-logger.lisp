;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.logger)

;;;;;;
;;; Standard logger

(def (logger e) standard-logger ()
  :runtime-level (if *load-as-production?* +info+ +debug+)
  :appenders ((debug-only* (make-instance 'brief-stream-appender))))
