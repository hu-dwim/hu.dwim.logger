;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2007 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-yalog)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +dribble+ 0)
  (defconstant +debug+   1)
  (defconstant +info+    2)
  (defconstant +warn+    3)
  (defconstant +error+   4)
  (defconstant +fatal+   5)

  (define-constant +max-category-name-length+ 12)
  (define-constant +log-level-names+ #(+dribble+ +debug+ +info+ +warn+ +error+ +fatal+) :test #'equalp))

(defparameter *log-categories* (make-hash-table :test 'eq))

(defun find-logger (name)
  (gethash name *log-categories*))

(defun (setf find-logger) (logger name)
  (assert (typep logger 'log-category))
  (if logger
      (progn
        (when (gethash name *log-categories*)
          (simple-style-warning "Redefining logger ~S" name))
        (setf (gethash name *log-categories*) logger))
      (remhash name *log-categories*)))

(defclass log-category ()
  ((ancestors :initform ()
              :accessor ancestors-of
              :initarg :ancestors
              :documentation "The log categories this category inherits from.")
   (children :initform ()
             :accessor children-of
             :initarg :children
             :documentation "The log categories which inherit from this category.")
   (appenders :initform ()
              :accessor appenders-of
              :initarg :appenders
              :documentation "A list of appender objects this category should send messages to.")
   (level :initform nil
          :initarg :level
          :accessor level-of
          :type (or null integer)
          :documentation "This category's log level.")
   (compile-time-level :initform +dribble+
                       :initarg :compile-time-level
                       :accessor compile-time-level-of
                       :type integer
                       :documentation "This category's compile time log level. Any log expression below this level will macro-expand to NIL.")
   (name :initarg :name
         :accessor name-of)))

(defmethod make-load-form ((self log-category) &optional env)
  (declare (ignore env))
  (let ((name (name-of self)))
    `(let ((result (find-logger ',name)))
       (assert result () ,(format nil "There must be some load-order issue with your loggers, because ~S was not found at load-time."
                                  name))
       result)))

#+nil
(cl-def:def cl-def:print-object log-category
    (format stream "~S" (name-of self)))

(defmethod print-object ((self log-category) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (let ((*standard-output* stream))
      (block printing
        (handler-bind ((error (lambda (error)
                                (declare (ignore error))
                                (write-string "<<error printing object>>") (return-from printing))))
          (format stream "~S" (name-of self)))))))

(defmethod shared-initialize :after ((self log-category) slot-names &key ancestors)
  (declare (ignore slot-names))
  (dolist (ancestor ancestors)
    (pushnew self (children-of ancestor) :test (lambda (a b)
                                                 (eql (name-of a) (name-of b))))))

;;; Runtime levels
(defun enabled-p (cat level)
  (>= level (log-level cat)))

(defgeneric log-level (category)
  (:method ((cat log-category))
    (or (level-of cat)
        (if (ancestors-of cat)
            (loop
               :for ancestor :in (ancestors-of cat)
               :minimize (log-level ancestor))
            (error "Can't determine level for ~S" cat))))
  (:method ((cat-name symbol))
    (log-level (find-logger cat-name))))

(defgeneric (setf log-level) (new-level cat &optional recursive)
  (:method (new-level (cat log-category)
               &optional (recursive t))
    "Change the log level of CAT to NEW-LEVEL. If RECUSIVE is T the setting is also applied to the sub categories of CAT."
    (setf (slot-value cat 'level) new-level)
    (when recursive
      (dolist (child (children-of cat))
        (setf (log-level child) new-level)))
    new-level)
  (:method (new-level (cat-name symbol) &optional (recursive t))
    (setf (log-level (find-logger cat-name) recursive) new-level))
  (:method (new-level (cat-name null) &optional (recursive t))
    (declare (ignore new-level cat-name recursive))
    (error "NIL does not specify a category.")))

;;; Compile time levels
(defun compile-time-enabled-p (cat level)
  (>= level (compile-time-log-level cat)))

(defgeneric compile-time-log-level (category)
  (:method ((cat log-category))
    (or (compile-time-level-of cat)
        (if (ancestors-of cat)
            (loop for ancestor in (ancestors-of cat)
               minimize (compile-time-log-level ancestor))
            (error "Can't determine compile time level for ~S" cat))))
  (:method ((cat-name symbol))
    (compile-time-log-level (find-logger cat-name))))

(defgeneric (setf compile-time-log-level) (new-level category &optional recursive)
  (:method (new-level (cat log-category) &optional (recursive t))
    "Change the compile time log level of CAT to NEW-LEVEL. If RECUSIVE is T the setting is also applied to the sub categories of CAT."
    (setf (slot-value cat 'compile-time-level) new-level)
    (when recursive
      (dolist (child (children-of cat))
        (setf (compile-time-log-level child) new-level)))
    new-level)
  (:method (new-level (cat-name symbol) &optional (recursive t))
    (setf (compile-time-log-level (find-logger cat-name) recursive) new-level))
  (:method (new-level (cat-name null) &optional (recursive t))
    (declare (ignore new-level cat-name recursive))
    (error "NIL does not specify a category.")))

(defmacro with-logger-level (logger-name new-level &body body)
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
                   (,old-level (level-of ,logger)))
             (setf (level-of ,logger) ,new-level)
             (unwind-protect
                  (progn ,@body)
               (setf (level-of ,logger) ,old-level)))))
        (t (error "Don't know how to interpret ~S as a logger name" logger-name))))

;;;; ** Handling Messages

(defmacro with-logging-io (&body body)
  `(let ((*print-right-margin* most-positive-fixnum)
         (*print-readably* nil)
         (*print-length* 64)
         (*package* #.(find-package "COMMON-LISP")))
    ,@body))

(defgeneric handle-log-message (category message level)
  (:documentation "Message is either a string or a list. When it's a list and the first element is a string then it's processed as args to cl:format."))

(defmethod handle-log-message :around ((cat log-category) message level)
  ;; turn off line wrapping for the entire time while inside the loggers
  (with-logging-io
    (call-next-method)))

(defmethod handle-log-message ((cat log-category) message level)
  (if (appenders-of cat)
      ;; if we have any appenders send them the message
      (dolist (appender (appenders-of cat))
        (append-message cat appender message level))
      ;; send the message to our ancestors
      (dolist (ancestor (ancestors-of cat))
        (handle-log-message ancestor message level))))

(defgeneric append-message (category log-appender message level)
  (:method :around (category log-appender message level)
    ;; what else should we do?
    (ignore-errors
      (call-next-method))))

(defmacro deflogger (name ancestors &key compile-time-level level appender appenders documentation)
  (declare (ignore documentation)
           (type symbol name))
  (unless (eq (symbol-package name) *package*)
    (simple-style-warning "When defining a logger named ~A, the home package of the symbol is not *package* (not (eq ~A ~A))"
                          (let ((*package* (find-package "KEYWORD")))
                            (format nil "~S" name))
                          (symbol-package name) *package*))
  (when appender
    (setf appenders (append appenders (list appender))))
  (let ((ancestors (mapcar (lambda (ancestor-name)
                             `(or (find-logger ',ancestor-name)
                                  (error "Attempt to define a sub logger of the undefined logger ~S."
                                         ',ancestor-name)))
                           ancestors)))
    (flet ((make-log-helper (suffix level)
             (let ((logger-macro-name (intern (concatenate 'string (symbol-name name) "." (symbol-name suffix)))))
               `(progn
                 (setf (get ',logger-macro-name 'logger) ',name)
                 (defmacro ,logger-macro-name (message-control &rest message-args)
                     ;; first check at compile time
                     (if (compile-time-enabled-p (find-logger ',name) ,level)
                         ;; then check at runtime
                         `(progn
                           (when (enabled-p (load-time-value (find-logger ',',name)) ,',level)
                             ;; this is problematic with call/cc (it has no multiple-value-prog1, yet)
                             #+nil(handler-case
                                      ,(if message-args
                                           `(handle-log-message (find-logger ',',name) (list ,message-control ,@message-args)
                                             ',',level)
                                           `(handle-log-message (find-logger ',',name) ,message-control ',',level))
                                    (serious-condition (error)
                                                       (warn "Ignoring serious-condition ~A while logging message ~S" error ,message-control)))
                             ,(if message-args
                                  `(handle-log-message (load-time-value (find-logger ',',name)) (list ,message-control ,@message-args)
                                    ',',level)
                                  `(handle-log-message (load-time-value (find-logger ',',name)) ,message-control ',',level)))
                           (values))
                         `(values)))))))
      `(progn
         (eval-when (:load-toplevel :execute)
           (setf (find-logger ',name) (make-instance 'log-category
                                                    :name ',name
                                                    ,@(cond (level
                                                             `(:level ,level))
                                                            ((not ancestors)
                                                             `(:level +debug+))
                                                            (t '()))
                                                    ,@(when compile-time-level
                                                        `(:compile-time-level ,compile-time-level))
                                                    :appenders (remove nil (list ,@appenders))
                                                    :ancestors (list ,@ancestors))))
         ,(make-log-helper '#:dribble '+dribble+)
         ,(make-log-helper '#:debug '+debug+)
         ,(make-log-helper '#:info '+info+)
         ,(make-log-helper '#:warn '+warn+)
         ,(make-log-helper '#:error '+error+)
         ,(make-log-helper '#:fatal '+fatal+)
        (values)))))
