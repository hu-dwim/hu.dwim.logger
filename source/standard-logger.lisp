;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.logger)

;;;;;;
;;; Standard logger

(def (logger e) standard-logger () :appenders ((debug-only* (make-instance 'brief-stream-appender :stream *debug-io*))))
