;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.logger)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :bordeaux-threads :hu.dwim.logger)
  (use-package :trivial-garbage :hu.dwim.logger))

;; TODO delme, and use something in iolib.os once it's comitted...
(def (function io) get-monotonic-time ()
  "Returns a time in seconds as a double-float that constantly grows (unaffected by setting the system clock)."
  (isys:%sys-get-monotonic-time))

;;;;;;
;;; Thread safe file appender

(def constant +file-appender-cache-maximum-size+ 128)

;; this is not thread-safe as is, but initialization of the loggers should happen without threads, so it's good enough...
(def special-variable *thread-safe-file-appenders* (make-weak-hash-table :weakness :value))

(def (class* e) thread-safe-file-appender (file-appender)
  ((lock (bordeaux-threads:make-lock "thread-safe-file-appender"))
   (last-flushed-at (get-monotonic-time))
   (cache (make-array +file-appender-cache-maximum-size+ :adjustable t :fill-pointer 0))))

(def constructor thread-safe-file-appender
  (setf (gethash (log-file-of -self-) *thread-safe-file-appenders*) -self-))

(def with-macro with-lock-held-on-file-appender (appender)
  (with-recursive-lock-held ((lock-of appender))
    (-body-)))

(def (function e) flush-file-appender-caches ()
  (loop
    :for appender :being :the :hash-values :of *thread-safe-file-appenders*
    :do (flush-file-appender-cache appender)))

(def (function e) flush-file-appender-cache (appender)
  (bind ((lines nil))
    (with-lock-held-on-file-appender appender
      (bind ((cache (cache-of appender)))
        (setf lines (make-array (length cache) :initial-contents cache))
        (setf (fill-pointer cache) 0))
      (setf (last-flushed-at-of appender) (get-monotonic-time)))
    (with-output-to-file-appender-file (output appender)
      (loop
        :for line :across lines
        :do (write-string line output))))
  (values))

(def method append-message ((logger logger) (appender thread-safe-file-appender) message level)
  (with-lock-held-on-file-appender appender
    (bind ((cache (cache-of appender)))
      (when (>= (length cache) (array-dimension cache 0))
        (flush-file-appender-cache appender))
      (assert (zerop (length cache)))
      (vector-push-extend (bind ((*package* #.(find-package :hu.dwim.logger)))
                            (format nil "(~S ~S @~A ~S ~S ~S)~%"
                                    (machine-instance)
                                    (sb-thread:thread-name sb-thread:*current-thread*)
                                    (local-time:now)
                                    (name-of logger)
                                    level
                                    message))
                          cache))))

(def (function e) make-thread-safe-file-appender (file-name)
  (make-instance 'thread-safe-file-appender :log-file file-name))
