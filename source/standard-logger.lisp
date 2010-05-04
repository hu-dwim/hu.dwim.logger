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
  :compile-time-level (if *load-as-production?* +debug+ +dribble+)
  :appenders ((debug-only* (make-instance 'brief-stream-appender))))

(def (function e) setup-logging-for-production (log-directory)
  (assert (truename log-directory) log-directory "Log directory does not exist or is not accessible. Tried: ~S" log-directory)
  (setf *log-directory* log-directory)
  (bind ((standard-logger (find-logger 'standard-logger)))
    (setf (hu.dwim.logger::appenders-of standard-logger)
          (list (make-level-filtering-appender +warn+ (make-thread-safe-file-appender "error.log"))
                (make-thread-safe-file-appender "root.log")))
    (setf (log-level standard-logger) +info+)))
