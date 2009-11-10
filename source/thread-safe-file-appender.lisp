;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.logger)

(def (class* e) thread-safe-file-appender (caching-appender file-appender)
  ()
  (:default-initargs :async-flushing nil)) ; TODO #f

(def method flush-caching-appender-messages ((appender thread-safe-file-appender) lines)
  (with-output-to-file-appender-file (output appender)
    (loop
      :for line :across lines
      :do (write-string line output)))
  (values))

(def (function e) make-thread-safe-file-appender (file-name)
  (make-instance 'thread-safe-file-appender :log-file file-name))
