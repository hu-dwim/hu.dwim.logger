;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.logger)

(def (special-variable e) *log-output* *debug-io*)

(def function logger-name-for-output (logger)
  (fully-qualified-symbol-name (name-of logger) :separator "::"))

(def function format-or-write-string (stream message-control message-arguments)
  (if message-arguments
      (apply #'format stream message-control message-arguments)
      (write-string message-control stream)))

;;;;;;
;;; Stream appender

(def constant +max-logger-name-length+ 15)

(def (class* e) stream-appender (appender)
  ((stream '*log-output*))
  (:documentation "Human readable logger."))

(def method stream-of :around ((self appender))
  (bind ((result (call-next-method)))
    (if (symbolp result)
        (symbol-value result)
        result)))

(def print-object stream-appender
  (prin1 (slot-value -self- 'stream)))

(def method make-instance ((class (eql (find-class 'stream-appender))) &rest initargs)
  (declare (ignore initargs))
  (error "STREAM-APPENDER is an abstract class. You must use either BRIEF-STREAM-APPENDER or VERBOSE-STREAM-APPENDER objects."))

(def method append-message (logger (appender stream-appender) level message-control message-arguments)
  (format-message logger appender level (stream-of appender) message-control message-arguments))

(def (class* e) brief-stream-appender (stream-appender)
  ((last-message-year :initform 0)
   (last-message-month :initform 0)
   (last-message-day :initform 0))
  (:documentation "A subclass of STREAM-APPENDER with minimal overhead text in messages. This amounts to: not printing the package names of loggers and log levels and a more compact printing of the current time."))

(def (class* e) verbose-stream-appender (stream-appender)
  ()
  (:documentation "A subclass of STREAM-APPENDER which attempts to be as precise as possible, logger names and log level names are printed with a package prefix and the time is printed in long format."))

(def method append-message :around ((logger logger) (appender stream-appender) level message-control message-arguments)
  (restart-case
      (multiple-value-prog1
          (call-next-method)
        (finish-output (stream-of appender)))
    (use-debug-io ()
      :report (lambda (stream)
                (format stream "Set the output stream of ~A (invoked through ~A) to '*debug-io* and then try again appending this message" appender *toplevel-logger*))
      (setf (stream-of appender) '*debug-io*)
      (append-message logger appender level message-control message-arguments))
    (use-standard-output ()
      :report (lambda (stream)
                (format stream "Set the output stream of ~A (invoked through ~A) to '*standard-output* and then try again appending this message" appender *toplevel-logger*))
      (setf (stream-of appender) '*standard-output*)
      (append-message logger appender level message-control message-arguments))
    (silence-logger ()
      :report (lambda (stream)
                (format stream "Set the output stream of ~A (invoked through ~A) to a deadend" appender *toplevel-logger*))
      (setf (stream-of appender) (make-broadcast-stream)))))

(def method format-message ((logger logger) (appender brief-stream-appender) level stream message-control message-arguments)
  (local-time:with-decoded-timestamp (:minute minute :hour hour :day day :month month :year year)
      (local-time:now)
    (with-slots (last-message-year last-message-month last-message-day) appender
      (unless (and (= year last-message-year)
                   (= month last-message-month)
                   (= day last-message-day))
        (format stream "--TIME MARK ~4,'0D-~2,'0D-~2,'0D--~%"
                year month day)
        (setf last-message-year year
              last-message-month month
              last-message-day day)))
    (bind ((logger-name (symbol-name (name-of *toplevel-logger*)))
           (logger-name-length (length logger-name))
           (level-name (symbol-name level)))
      (format stream
              #.(concatenate 'string
                             "~2,'0D:~2,'0D ~"
                             (princ-to-string +max-logger-name-length+)
                             "@A ~7A ")
              hour minute
              (subseq logger-name
                      (max 0 (- logger-name-length
                                +max-logger-name-length+))
                      logger-name-length)
              (subseq level-name 1 (1- (length level-name)))))
    (format-or-write-string stream message-control message-arguments)
    (terpri stream)))

(def method format-message ((logger logger) (appender verbose-stream-appender) level stream message-control message-arguments)
  (format stream
          "~A ~S ~S: "
          (local-time:now)
          (logger-name-for-output *toplevel-logger*)
          level)
  (format-or-write-string stream message-control message-arguments)
  (terpri stream))

(def (function e) make-stream-appender (&rest args &key (stream '*log-output*) (verbosity 2) &allow-other-keys)
  (check-type verbosity number)
  (remove-from-plistf args :stream :verbosity)
  (apply #'make-instance (case verbosity
                           ((0 1) 'brief-stream-appender)
                           (t 'verbose-stream-appender))
         :stream stream
         args))

;;;;;;
;;; File appender

(def (special-variable e) *log-directory*)

(def (class* e) file-appender (stream-appender)
  ((log-file :documentation "Name of the file to write log messages to."))
  (:documentation "Logs to a file. The output of the file logger is not meant to be read directly by a human."))

(def print-object file-appender
  (prin1 (file-appender-output-file -self-)))

(def function file-appender-output-file (appender)
  (bind ((log-file (log-file-of appender)))
    (if (eq (first (pathname-directory log-file)) :absolute)
        log-file
        (merge-pathnames log-file *log-directory*))))

(def (with-macro* :macro-only-arguments (stream-var-name))
    with-output-to-file-appender-file (stream-var-name appender)
  (loop
    (with-simple-restart (retry-writing-log-file "Try to run the entire WITH-OPEN-FILE block again (and potentially emit screwed up or duplicate log entries!)")
      (with-open-file (stream (file-appender-output-file appender) :direction :output :if-exists :append :if-does-not-exist :create)
        (-with-macro/body- (stream stream-var-name)))
      (return))))

(def method append-message ((logger logger) (appender file-appender) level message-control message-arguments)
  (with-output-to-file-appender-file (output appender)
    (format-message logger appender level output message-control message-arguments)))

(def method format-message ((logger logger) (appender file-appender) level stream message-control message-arguments)
  (bind ((*package* #.(find-package :hu.dwim.logger)))
    (write-string "(\"" stream)
    (local-time:format-rfc3339-timestring stream (local-time:now))
    (format stream "\" ~3S ~8S ~A \""
            (human-readable-thread-id)
            level
            (logger-name-for-output *toplevel-logger*))
    (format-or-write-string stream message-control message-arguments)
    (format stream "\" ~S)~%"
            ;; TODO this should eventually be replaced with some smartness coming from with-activity
            (bordeaux-threads:thread-name (bordeaux-threads:current-thread)))))

(def (function e) make-file-appender (file-name)
  (make-instance 'file-appender :log-file file-name))

;;;;;;
;;; Level filter appender

(def (class* e) level-filtering-appender (stream-appender)
  ((minimum-level +debug+)
   (chained-appenders))
  (:documentation "Drops messages below MINIMUM-LEVEL and forwards the others to CHAINED-APPENDERS."))

(def method append-message ((logger logger) (appender level-filtering-appender) level message-control message-arguments)
  (when (>= (etypecase level
              (number level)
              (symbol (symbol-value level)))
            (minimum-level-of appender))
    (dolist (chained-appender (chained-appenders-of appender))
      (append-message logger chained-appender level message-control message-arguments))))

(def (function e) make-level-filtering-appender (minimum-level &rest chained-appenders)
  (make-instance 'level-filtering-appender :minimum-level minimum-level :chained-appenders chained-appenders))

;;;;;;
;;; caching appender

(def generic flush-caching-appender-messages (appender lines))

;; TODO delme, and use something in iolib.os once it's comitted...
;; TODO or at least move it to hu.dwim.util if that's not the way to go...
(def (function io) get-monotonic-time ()
  "Returns a time in seconds as a double-float that constantly grows (unaffected by setting the system clock)."
  #-allegro (isys:%sys-get-monotonic-time)
  #+allegro (get-internal-real-time))

(def constant +caching-appender/maximum-cache-size+ 128)

(def (namespace :weakness :key) caching-appender)

(def (class* e) caching-appender ()
  ((lock (bordeaux-threads:make-lock "a caching-appender of hu.dwim.logger"))
   (last-flushed-at (get-monotonic-time))
   (cache (make-array +caching-appender/maximum-cache-size+ :adjustable #f :fill-pointer 0))
   (async-flushing #f :accessor async-flushing? :type boolean)))

(def constructor caching-appender
  (setf (find-caching-appender -self-) #t))

(def with-macro with-lock-held-on-caching-appender (appender)
  (bordeaux-threads:with-recursive-lock-held ((lock-of appender))
    (-body-)))

(def method flush-caching-appender-messages :after ((appender caching-appender) lines)
  (setf (last-flushed-at-of appender) (get-monotonic-time)))

(def (function e) flush-caching-appenders ()
  (bind ((appenders '()))
    (iterate-caching-appender-namespace
     (lambda (appender value)
       (declare (ignore value))
       (push appender appenders)))
    ;; flush without the namespace lock...
    ;; TODO use surround-body-when
    (if *ignore-logging-errors*
        (ignore-errors
          (map nil 'flush-caching-appender appenders))
        (map nil 'flush-caching-appender appenders))))

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

(def method append-message ((logger logger) (appender caching-appender) level message-control message-arguments)
  (bind ((formatted-message (format-message logger appender level nil message-control message-arguments)))
    (with-lock-held-on-caching-appender appender
      (bind ((cache (cache-of appender)))
        (vector-push-extend formatted-message cache)
        (when (>= (length cache) (array-dimension cache 0))
          (flush-caching-appender appender)
          ;; we have the lock, so it must be empty here
          (assert (zerop (length cache))))))))

;;;;;;
;;; thread safe file appender

(def (class* e) thread-safe-file-appender (caching-appender file-appender)
  ()
  (:default-initargs :async-flushing #f))

(def method flush-caching-appender-messages ((appender thread-safe-file-appender) lines)
  (with-output-to-file-appender-file (output appender)
    (loop
      :for line :across lines
      :do (write-string line output)))
  (values))

(def (function e) make-thread-safe-file-appender (file-name)
  (make-instance 'thread-safe-file-appender :log-file file-name))
