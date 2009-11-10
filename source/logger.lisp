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

(def (constant e) +log-level-names+ '(+dribble+ +debug+ +info+ +warn+ +error+ +fatal+))

;;;;;;
;;; Variables

(def special-variable *loggers* (make-hash-table :test 'eq))

(def (special-variable e) *default-compile-time-level* (if *load-as-production?* +debug+ +dribble+))

;;;;;;
;;; Namespace

(def function logger? (value)
  (typep value 'logger))

(def function %find-logger (name &optional (errorp t))
  (if (logger? name)
      name
      (let ((logger (gethash name *loggers*)))
        (when (and errorp
                   (null logger))
          (error "Couldn't find a logger by the name ~S" name))
        logger)))

(def (function e) find-logger (name &optional (errorp t))
  (%find-logger name errorp))

(def compiler-macro find-logger (&whole whole name &optional (errorp t))
  (if (and (consp name)
           (eq 'quote (first name))
           (symbolp (second name)))
      `(load-time-value (%find-logger ,name ,errorp))
      whole))

(def function (setf find-logger) (logger name)
  (assert (typep logger 'logger))
  (if logger
      (progn
        (when (gethash name *loggers*)
          (simple-style-warning "Redefining logger ~S" name))
        (setf (gethash name *loggers*) logger))
      (remhash name *loggers*)))

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

(def method print-object ((self logger) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (let ((*standard-output* stream))
      (block printing
        (handler-bind ((error (lambda (error)
                                (declare (ignore error))
                                (write-string "<<error printing object>>") (return-from printing))))
          (format stream "~S" (name-of self)))))))

(def method shared-initialize :after ((self logger) slot-names &key parents)
  (declare (ignore slot-names))
  (dolist (parent parents)
    (pushnew self (children-of parent) :test (lambda (a b)
                                               (eql (name-of a) (name-of b))))))

;;;;;;
;;; Runtime level

(def function at-runtime-enabled? (logger level)
  (>= level (log-level logger)))

(def (generic e) log-level (logger)
  (:method ((self logger))
    (or (runtime-level-of self)
        (if (parents-of self)
            (loop
               :for parent :in (parents-of self)
               :minimize (log-level parent))
            (error "Can't determine level for ~S" self))))

  (:method ((self null))
    (error "NIL is not a valid log logger name"))

  (:method ((self symbol))
    (log-level (find-logger self))))

(def generic (setf log-level) (new-level self &optional recursive)
  (:method (new-level (self logger) &optional (recursive t))
    "Change the log level of LOGGER to NEW-LEVEL. If RECUSIVE is T the setting is also applied to the sub loggers of LOGGER."
    (setf (slot-value self 'runtime-level) new-level)
    (when recursive
      (dolist (child (children-of self))
        (setf (log-level child) new-level)))
    new-level)

  (:method (new-level (self symbol) &optional (recursive t))
    (setf (log-level (find-logger self) recursive) new-level))

  (:method (new-level (self null) &optional (recursive t))
    (declare (ignore new-level self recursive))
    (error "NIL does not specify a logger.")))

;;;;;;
;;; Compile time level

(def function at-compile-time-enabled? (logger level)
  (>= level (compile-time-level logger)))

(def (generic e) compile-time-level (logger)
  (:method ((self logger))
    (or (compile-time-level-of self)
        (if (parents-of self)
            (loop
               :for parent :in (parents-of self)
               :minimize (compile-time-level parent))
            (error "Can't determine compile time level for ~S" self))))

  (:method ((self symbol))
    (compile-time-level (find-logger self))))

(def generic (setf compile-time-level) (new-level logger &optional recursive)
  (:method (new-level (self logger) &optional (recursive t))
    "Change the compile time log level of LOGGER to NEW-LEVEL. If RECUSIVE is T the setting is also applied to the sub loggers of LOGGER."
    (setf (slot-value self 'compile-time-level) new-level)
    (when recursive
      (dolist (child (children-of self))
        (setf (compile-time-level child) new-level)))
    new-level)

  (:method (new-level (self symbol) &optional (recursive t))
    (setf (compile-time-level (find-logger self) recursive) new-level))

  (:method (new-level (self null) &optional (recursive t))
    (declare (ignore new-level self recursive))
    (error "NIL does not specify a logger.")))

(def (macro e) with-logger-level (logger-name new-level &body body)
  "Set the level of the listed logger(s) to NEW-LEVEL and restore the original value in an unwind-protect."
  (cond ((consp logger-name)
         `(with-logger-level ,(pop logger-name) ,new-level
            ,(if logger-name
                 `(with-logger-level ,logger-name ,new-level
                    ,@body)
                 `(progn
                    ,@body))))
        ((symbolp logger-name)
         (with-unique-names (logger old-level)
           `(let* ((,logger (find-logger ',logger-name))
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

;; while inside HANDLE-LOG-MESSAGE it is bound to the toplevel logger
(def special-variable *toplevel-logger*)

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
                          (let ((*package* (find-package "KEYWORD")))
                            (format nil "~S" name))
                          (symbol-package name) *package*))
  (when appender
    (setf appenders (append appenders (list appender))))
  (let ((parents (or (mapcar (lambda (parent)
                               `(or (find-logger ',parent nil)
                                    (error "Attempt to define a sub logger of the undefined logger ~S." ',parent)))
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
             (unless (find-logger ',name nil)
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
