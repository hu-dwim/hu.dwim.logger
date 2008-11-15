;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2007 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-yalog)

;;;; *** Stream log appender

(defclass appender ()
  ((verbosity :initform 2 :initarg :verbosity :accessor verbosity-of)))

(defclass stream-log-appender (appender)
  ((stream :initarg :stream :accessor log-stream))
  (:documentation "Human readable to the console logger."))

(defmethod make-instance ((class (eql (find-class 'stream-log-appender)))
                          &rest initargs)
  (declare (ignore initargs))
  (error "STREAM-LOG-APPENDER is an abstract class. You must use either brief-stream-log-appender or verbose-stream-log-appender objects."))

(defmethod append-message :around (category (appender stream-log-appender) (message cons) level)
  (append-message category appender (apply #'format nil message) level))

(defclass brief-stream-log-appender (stream-log-appender)
  ((last-message-year :initform 0)
   (last-message-month :initform 0)
   (last-message-day :initform 0))
  (:documentation "A subclass of stream-log-appender with minimal 'overhead' text in log messages. This amounts to: not printing the package names of log categories and log levels and a more compact printing of the current time."))

(defclass verbose-stream-log-appender (stream-log-appender)
  ()
  (:documentation "A subclass of stream-log-appender which attempts to be as precise as possible, category names and log level names are printed with a package prefix and the time is printed in long format."))

(defmethod append-message :around ((category log-category) (s stream-log-appender)
                                   message level)
  (restart-case
      (call-next-method)
    (use-*debug-io* ()
      :report "Use the current value of *debug-io*"
      (setf (log-stream s) *debug-io*)
      (append-message category s message level))
    (use-*standard-output* ()
      :report "Use the current value of *standard-output*"
      (setf (log-stream s) *standard-output*)
      (append-message category s message level))
    (silence-logger ()
      :report "Ignore all future messages to this logger."
      (setf (log-stream s) (make-broadcast-stream)))))

(defmethod append-message ((category log-category) (s brief-stream-log-appender)
                           message level)
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (get-universal-time))
    (declare (ignore second))
    (with-slots (last-message-year last-message-month last-message-day)
        s
      (unless (and (= year last-message-year)
                   (= month last-message-month)
                   (= day last-message-day))
        (format (log-stream s) "--TIME MARK ~4,'0D-~2,'0D-~2,'0D--~%"
                year month day)
        (setf last-message-year year
              last-message-month month
              last-message-day day)))
    (let* ((category-name (symbol-name (name-of category)))
           (level-name (symbol-name level))
           (category-length (length category-name)))
      (format (log-stream s)
              #.(concatenate 'string
                             "~2,'0D:~2,'0D ~"
                             (princ-to-string +max-category-name-length+)
                             "@A ~7A ")
              hour minute
              (subseq category-name
                      (max 0 (- category-length
                                +max-category-name-length+))
                      category-length)
              (subseq level-name 1 (1- (length level-name)))))
    (format (log-stream s) "~A~%" message)))

(defmethod append-message ((category log-category) (s verbose-stream-log-appender)
                            message level)
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time (get-universal-time))
    (format (log-stream s)
            "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D.~2,'0D ~S/~S: "
            year month date hour minute second
            (name-of category) level)
    (format (log-stream s) "~A~%" message)))

(defun make-stream-log-appender (&rest args &key (stream *debug-io*) (verbosity 2) &allow-other-keys)
  (remove-from-plistf args :stream :verbosity)
  (apply #'make-instance (case verbosity
                           ((0 1) 'brief-stream-log-appender)
                           (t 'verbose-stream-log-appender))
         :stream stream
         :verbosity verbosity
         args))

;;;;;;;;;;
;;; file-log-appender

(defvar *log-directory*)

(defclass file-log-appender (stream-log-appender)
  ((log-file :initarg :log-file :accessor log-file-of
             :documentation "Name of the file to write log messages to."))
  (:documentation "Logs to a file. the output of the file logger is not meant to be read directly by a human."))

(defmethod append-message ((category log-category) (appender file-log-appender)
                           message level)
  (with-open-file (output (merge-pathnames (cl-yalog::log-file-of appender) *log-directory*)
                          :direction :output :if-exists :append :if-does-not-exist :create)
    (format output "(~S ~D ~S ~S)~%" level (get-universal-time) (name-of category) message)))

(defun make-file-log-appender (file-name)
  (make-instance 'file-log-appender :log-file file-name))

;;;;;;;;;;
;;; level-filter-appender

(defclass level-filter-appender (stream-log-appender)
  ((minimum-level :initform +debug+ :initarg :minimum-level :accessor minimum-level-of)
   (chained-appender :initarg :chained-appender :accessor chained-appender-of))
  (:documentation "Drops messages below MINIMUM-LEVEL and forwards the others to CHAINED-APPENDER."))

(defmethod append-message ((category log-category) (appender level-filter-appender)
                           message level)
  (when (>= (etypecase level
              (number level)
              (symbol (symbol-value level)))
            (minimum-level-of appender))
    (append-message category (chained-appender-of appender) message level)))

(defun make-level-filter-appender (minimum-level chained-appender)
  (make-instance 'level-filter-appender :minimum-level minimum-level :chained-appender chained-appender))
