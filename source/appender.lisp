;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.logger)

;;;;;;
;;; Stream appender

(def constant +max-logger-name-length+ 12)

(def (class* e) appender ()
  ((verbosity 2)))

(def (class* e) stream-appender (appender)
  ((stream))
  (:documentation "Human readable logger."))

(def method make-instance ((class (eql (find-class 'stream-appender))) &rest initargs)
  (declare (ignore initargs))
  (error "STREAM-APPENDER is an abstract class. You must use either BRIEF-STREAM-APPENDER or VERBOSE-STREAM-APPENDER objects."))

(def method append-message :around (logger (appender stream-appender) (message cons) level)
  (append-message logger appender (apply #'format nil message) level))

(def (class* e) brief-stream-appender (stream-appender)
  ((last-message-year :initform 0)
   (last-message-month :initform 0)
   (last-message-day :initform 0))
  (:documentation "A subclass of STREAM-APPENDER with minimal overhead text in messages. This amounts to: not printing the package names of loggers and log levels and a more compact printing of the current time."))

(def (class* e) verbose-stream-appender (stream-appender)
  ()
  (:documentation "A subclass of STREAM-APPENDER which attempts to be as precise as possible, logger names and log level names are printed with a package prefix and the time is printed in long format."))

(def method append-message :around ((logger logger) (s stream-appender) message level)
  (restart-case
      (call-next-method)
    (use-*debug-io* ()
      :report "Use the current value of *debug-io*"
      (setf (stream-of s) *debug-io*)
      (append-message logger s message level))
    (use-*standard-output* ()
      :report "Use the current value of *standard-output*"
      (setf (stream-of s) *standard-output*)
      (append-message logger s message level))
    (silence-logger ()
      :report "Ignore all future messages to this logger."
      (setf (stream-of s) (make-broadcast-stream)))))

(def method append-message ((logger logger) (appender brief-stream-appender) message level)
  (local-time:with-decoded-timestamp (:minute minute :hour hour :day day :month month :year year)
      (local-time:now)
    (with-slots (last-message-year last-message-month last-message-day) appender
      (unless (and (= year last-message-year)
                   (= month last-message-month)
                   (= day last-message-day))
        (format (stream-of appender) "--TIME MARK ~4,'0D-~2,'0D-~2,'0D--~%"
                year month day)
        (setf last-message-year year
              last-message-month month
              last-message-day day)))
    (let* ((logger-name (symbol-name (name-of logger)))
           (level-name (symbol-name level))
           (logger-length (length logger-name)))
      (format (stream-of appender)
              #.(concatenate 'string
                             "~2,'0D:~2,'0D ~"
                             (princ-to-string +max-logger-name-length+)
                             "@A ~7A ")
              hour minute
              (subseq logger-name
                      (max 0 (- logger-length
                                +max-logger-name-length+))
                      logger-length)
              (subseq level-name 1 (1- (length level-name)))))
    (format (stream-of appender) "~A~%" message)))

(def method append-message ((logger logger) (s verbose-stream-appender) message level)
  (format (stream-of s)
          "~A ~S ~S: ~A~%"
          (local-time:now) (name-of logger) level message))

(def (function e) make-stream-appender (&rest args &key (stream *debug-io*) (verbosity 2) &allow-other-keys)
  (check-type verbosity number)
  (remove-from-plistf args :stream :verbosity)
  (apply #'make-instance (case verbosity
                           ((0 1) 'brief-stream-appender)
                           (t 'verbose-stream-appender))
         :stream stream
         :verbosity verbosity
         args))

;;;;;;
;;; File appender

(def (special-variable e) *log-directory*)

(def (class* e) file-appender (stream-appender)
  ((log-file :documentation "Name of the file to write log messages to."))
  (:documentation "Logs to a file. The output of the file logger is not meant to be read directly by a human."))

(def method append-message ((logger logger) (appender file-appender) message level)
  (with-open-file (output (merge-pathnames (log-file-of appender) *log-directory*)
                          :direction :output :if-exists :append :if-does-not-exist :create)
    (format output "(~S ~A ~S ~S)~%" level (local-time:now) (name-of logger) message)))

(def (function e) make-file-appender (file-name)
  (make-instance 'file-appender :log-file file-name))

;;;;;;
;;; Level filter appender

(def (class* e) level-filter-appender (stream-appender)
  ((minimum-level +debug+)
   (chained-appenders))
  (:documentation "Drops messages below MINIMUM-LEVEL and forwards the others to CHAINED-APPENDERS."))

(def method append-message ((logger logger) (appender level-filter-appender) message level)
  (when (>= (etypecase level
              (number level)
              (symbol (symbol-value level)))
            (minimum-level-of appender))
    (dolist (chained-appender (chained-appenders-of appender))
      (append-message logger chained-appender message level))))

(def (function e) make-level-filter-appender (minimum-level &rest chained-appenders)
  (make-instance 'level-filter-appender :minimum-level minimum-level :chained-appenders chained-appenders))
