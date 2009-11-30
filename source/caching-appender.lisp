;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.logger)

(def generic format-caching-appender-message (logger appender message level))
(def generic flush-caching-appender-messages (appender lines))

;; TODO delme, and use something in iolib.os once it's comitted...
(def (function io) get-monotonic-time ()
  "Returns a time in seconds as a double-float that constantly grows (unaffected by setting the system clock)."
  (isys:%sys-get-monotonic-time))

(def constant +caching-appender/maximum-cache-size+ 128)

;; this is not thread-safe as is, but initialization of the loggers should happen without threads, so it's good enough...
(def special-variable *caching-appenders* (trivial-garbage:make-weak-hash-table :weakness :key))

(def (class* e) caching-appender ()
  ((lock (bordeaux-threads:make-lock "a caching-appender of hu.dwim.logger"))
   (last-flushed-at (get-monotonic-time))
   (cache (make-array +caching-appender/maximum-cache-size+ :adjustable #t :fill-pointer 0))
   (async-flushing #f :accessor async-flushing? :type boolean)))

(def constructor caching-appender
  (setf (gethash -self- *caching-appenders*) #t))

(def with-macro with-lock-held-on-caching-appender (appender)
  (bordeaux-threads:with-recursive-lock-held ((lock-of appender))
    (-body-)))

(def method flush-caching-appender-messages :after ((appender caching-appender) lines)
  (setf (last-flushed-at-of appender) (get-monotonic-time)))

(def (function e) flush-caching-appenders ()
  (loop
    :for appender :being :the :hash-keys :of *caching-appenders*
    :do (flush-caching-appender appender)))

(def (function e) flush-caching-appender (appender)
  (bind ((lines nil)
         (flushed? #f))
    (flet ((ensure-flushed ()
             (when (and lines
                        (not flushed?))
               (setf flushed? #t)
               (flush-caching-appender-messages appender lines))))
      (with-lock-held-on-caching-appender appender
        (bind ((cache (cache-of appender))
               (cache-size (length cache)))
          (unless (zerop cache-size)
            (setf lines (make-array cache-size :initial-contents cache))
            (setf (fill-pointer cache) 0)))
        (unless (async-flushing? appender)
          (ensure-flushed)))
      (ensure-flushed)))
  (values))

(def method format-caching-appender-message ((logger logger) (appender caching-appender) message level)
  (bind ((*package* #.(find-package :hu.dwim.logger)))
    (format nil "(@~A ~3S ~8S ~S ~S ~S)~%"
            (local-time:now)
            (human-readable-thread-id)
            level
            (name-of *toplevel-logger*)
            message
            ;; TODO this should eventually be replaced with some smartness coming from with-activity
            (bordeaux-threads:thread-name (bordeaux-threads:current-thread)))))

(def method append-message ((logger logger) (appender caching-appender) message level)
  (with-lock-held-on-caching-appender appender
    (bind ((cache (cache-of appender)))
      (when (>= (length cache) (array-dimension cache 0))
        (flush-caching-appender appender)
        ;; we have the lock, it must be empty
        (assert (zerop (length cache))))
      (vector-push-extend (format-caching-appender-message logger appender message level) cache))))
