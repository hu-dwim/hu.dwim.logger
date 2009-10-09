;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.logger)

;;;;;;
;;; Thread safe file appender

(def (class* e) thread-safe-file-appender (file-appender)
  ((lock (bordeaux-threads:make-lock "thread-safe-file-appender"))))

(def method append-message ((logger logger) (appender thread-safe-file-appender) message level)
  ;; TODO implement buffering and flushing to lower contention. needs a timer.
  (bordeaux-threads:with-lock-held ((lock-of appender))
    (with-open-file (log-file (merge-pathnames (log-file-of appender) *log-directory*)
                              :if-exists :append :if-does-not-exist :create :direction :output)
      (let ((*package* #.(find-package :hu.dwim.logger)))
        (format log-file "(~S ~S @~A ~S ~S ~S)~%"
                (machine-instance)
                (sb-thread:thread-name sb-thread:*current-thread*)
                (local-time:now)
                (name-of logger)
                level
                message)))))

(def (function e) make-thread-safe-file-appender (file-name)
  (make-instance 'thread-safe-file-appender :log-file file-name))
