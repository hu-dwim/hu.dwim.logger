;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.logger)

;;;;;;
;;; Default logger levels

(def (constant e) +dribble+ 0)
(def (constant e) +debug+   1)
(def (constant e) +info+    2)
(def (constant e) +warn+    3)
(def (constant e) +error+   4)
(def (constant e) +fatal+   5)

(def (constant e :test 'equalp) +log-level-names+ #(+dribble+ +debug+ +info+ +warn+ +error+ +fatal+))

;;;;;;
;;; Variables

(def (namespace :finder-name %find-logger) logger)

(def (special-variable e) *default-compile-time-level* (if *load-as-production?* +debug+ +dribble+))

;;;;;;
;;; Namespace

(def (function e) find-logger (name &key (otherwise nil otherwise?))
  (if otherwise?
      (%find-logger name :otherwise otherwise)
      (%find-logger name)))

(def compiler-macro find-logger (&whole whole name &key (otherwise nil otherwise?))
  (if (quoted-symbol? name)
      `(load-time-value (%find-logger ,name ,@(when otherwise? `(:otherwise ,otherwise))))
      whole))

(def (function e) (setf find-logger) (logger name)
  (check-type logger logger)
  (setf (%find-logger name) logger))

;;;;;;
;;; Logger

(def (class* e) logger ()
  ((parents nil :documentation "The parent logger this logger inherits from.")
   (children nil :documentation "The loggers which inherit from this logger.")
   (appenders nil :documentation "A list of appender objects this logger should send messages to.")
   (runtime-level nil :type (or null integer) :documentation "The runtime log level determines whether an actual log message shows up at runtime.")
   (compile-time-level *default-compile-time-level* :type integer :documentation "The compile time log level is a compile time filter. Log expressions below this level will macro-expand to NIL at compile time.")
   (name)))

(def method make-load-form ((self logger) &optional env)
  (declare (ignore env))
  (let ((name (name-of self)))
    `(let ((result (find-logger ',name)))
       (assert result () ,(format nil "There must be some load-order issue with your loggers, because ~S was not found at load-time."
                                  name))
       result)))

(def print-object logger
  (with-keyword-package
    (write (name-of -self-))))

(def method shared-initialize :after ((self logger) slot-names &key parents)
  (declare (ignore slot-names))
  (dolist (parent parents)
    (pushnew self (children-of parent) :test (lambda (a b)
                                               (eql (name-of a) (name-of b))))))

;;;;;;
;;; Runtime level

(def function at-runtime-enabled? (logger level)
  (>= level (log-level/runtime logger)))

(def (function e) log-level (logger)
  (log-level/runtime logger))

(def function (setf log-level) (new-value logger)
  (setf (log-level/runtime logger) new-value))

(def (function e) log-level/runtime (logger)
  (if (symbolp logger)
      (log-level/runtime (find-logger logger))
      (or (runtime-level-of logger)
          (if (parents-of logger)
              (loop
                :for parent :in (parents-of logger)
                :minimize (log-level/runtime parent))
              (error "Can't determine runtime level for ~S" logger)))))

(def function (setf log-level/runtime) (new-level logger &key recursive)
  (if (symbolp logger)
      (setf (log-level/runtime (find-logger logger) :recursive recursive) new-level)
      (progn
        (setf (runtime-level-of logger) new-level)
        (when recursive
          (dolist (child (children-of logger))
            (setf (log-level/runtime child) new-level)))
        new-level)))

;;;;;;
;;; Compile time level

;; the following is a bit of a copy-paste, but let's just skip macrology for only two instances...
(def function at-compile-time-enabled? (logger level)
  (>= level (log-level/compile-time logger)))

(def (function e) log-level/compile-time (logger)
  (if (symbolp logger)
      (log-level/compile-time (find-logger logger))
      (or (compile-time-level-of logger)
          (if (parents-of logger)
              (loop
                :for parent :in (parents-of logger)
                :minimize (log-level/compile-time parent))
              (error "Can't determine compile-time level for ~S" logger)))))

(def function (setf log-level/compile-time) (new-level logger &key recursive)
  (if (symbolp logger)
      (setf (log-level/compile-time (find-logger logger) :recursive recursive) new-level)
      (progn
        (setf (compile-time-level-of logger) new-level)
        (when recursive
          (dolist (child (children-of logger))
            (setf (log-level/compile-time child) new-level)))
        new-level)))

(def (macro e) with-logger-level ((logger-name new-level) &body body)
  "Set the runtime level of the listed logger(s) to NEW-LEVEL and restore the original value in an unwind-protect."
  (cond ((consp logger-name)
         `(with-logger-level (,(pop logger-name) ,new-level)
            ,(if logger-name
                 `(with-logger-level (,logger-name ,new-level)
                    ,@body)
                 `(progn
                    ,@body))))
        ((symbolp logger-name)
         (with-unique-names (logger old-level)
           `(bind ((,logger (find-logger ',logger-name))
                   (,old-level (runtime-level-of ,logger)))
              (setf (runtime-level-of ,logger) ,new-level)
              (unwind-protect
                   (progn ,@body)
                (setf (runtime-level-of ,logger) ,old-level)))))
        (t (error "Don't know how to interpret ~S as a logger name" logger-name))))

;;;;;;
;;; Handling messages

(def macro with-logging-io (&body body)
  `(let ((*print-right-margin* most-positive-fixnum)
         (*print-readably* nil)
         (*print-length* 64)
         (*package* #.(find-package "COMMON-LISP")))
     ,@body))

(def (special-variable :documentation "While inside HANDLE-LOG-MESSAGE, this variable is bound to the toplevel logger")
  *toplevel-logger*)

(def function call-handle-log-message (logger message level)
  (assert (not (boundp '*toplevel-logger*)))
  (with-logging-io
    (bind ((*toplevel-logger* logger))
      (bind (((:values ok? error) (ignore-errors
                                    (handle-log-message logger message level)
                                    t)))
        (unless ok?
          (warn "Ignoring error comding from inside HANDLE-LOG-MESSAGE: ~A" error)))))
  (values))

(def (generic e) handle-log-message (logger message level)
  (:documentation "Message is either a string or a list. When it's a list and the first element is a string then it's processed as args to cl:format.")
  (:method ((self logger) message level)
    (if (appenders-of self)
        (dolist (appender (appenders-of self))
          (append-message self appender message level))
        (dolist (parent (parents-of self))
          (handle-log-message parent message level)))))

(def (generic e) append-message (logger appender message level))

(def function collect-helper-names (loggern-name)
  (flet ((make (suffix)
           (format-symbol (symbol-package loggern-name) "~A.~A" (symbol-name loggern-name) (symbol-name suffix))))
    (list (make '#:dribble)
          (make '#:debug)
          (make '#:info)
          (make '#:warn)
          (make '#:error)
          (make '#:fatal))))

(def (macro e) deflogger (name parents &key compile-time-level runtime-level appender appenders documentation)
  (declare (ignore documentation)
           (type symbol name))
  (unless (eq (symbol-package name) *package*)
    (simple-style-warning "When defining a logger named ~A, the home package of the symbol is not *package* (not (eq ~A ~A))"
                          (fully-qualified-symbol-name name)
                          (symbol-package name) *package*))
  (when appender
    (setf appenders (append appenders (list appender))))
  (let ((parents (or (mapcar (lambda (parent)
                               `(or (find-logger ',parent :otherwise #f)
                                    (error "Attempt to define a sub-logger of the undefined logger ~S." ',parent)))
                             parents)
                     (unless (eq name 'standard-logger)
                       '((find-logger 'standard-logger))))))
    (flet ((make-log-helper (suffix level)
             (let ((logger-macro-name (format-symbol (symbol-package name) "~A.~A" (symbol-name name) (symbol-name suffix))))
               `(progn
                  (setf (get ',logger-macro-name 'logger) ',name)
                  (def macro ,logger-macro-name (message-control &rest message-args)
                    ;; first check at compile time
                    (if (at-compile-time-enabled? (find-logger ',name) ,level)
                        ;; then check at runtime
                        (with-unique-names (logger)
                          `(bind ((,logger (load-time-value (find-logger ',',name))))
                             (when (at-runtime-enabled? ,logger ,',level)
                               ,(if message-args
                                    `(call-handle-log-message ,logger (list ,message-control ,@message-args) ',',level)
                                    `(call-handle-log-message ,logger ,message-control ',',level)))
                             (values)))
                        `(values)))))))
      (with-unique-names (logger)
        `(progn
           (eval-when (:compile-toplevel :load-toplevel :execute)
             (unless (find-logger ',name :otherwise #f)
               (setf (find-logger ',name) (make-instance 'logger
                                                         :name ',name
                                                         ,@(when compile-time-level
                                                                 `(:compile-time-level ,compile-time-level))))))
           (eval-when (:load-toplevel :execute)
             (let ((,logger (find-logger ',name)))
               ,(when runtime-level
                      `(setf (runtime-level-of ,logger) ,runtime-level))
               ,(when compile-time-level
                      `(setf (compile-time-level-of ,logger) ,compile-time-level))
               (setf (appenders-of ,logger) (remove nil (list ,@appenders)))
               (setf (parents-of ,logger) (list ,@parents))))
           ,(make-log-helper '#:dribble '+dribble+)
           ,(make-log-helper '#:debug '+debug+)
           ,(make-log-helper '#:info '+info+)
           ,(make-log-helper '#:warn '+warn+)
           ,(make-log-helper '#:error '+error+)
           ,(make-log-helper '#:fatal '+fatal+)
           (values))))))

(def (definer e :available-flags "e") logger (name parents &key compile-time-level runtime-level appender appenders documentation)
  `(progn
     (deflogger ,name ,parents
       :runtime-level ,runtime-level
       :compile-time-level ,compile-time-level
       :appender ,appender
       :appenders ,appenders
       :documentation ,documentation)
     ,@(when (getf -options- :export)
             `((export ',(cons name (collect-helper-names name)))))))
