;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2007 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-user)

(defpackage :cl-yalog
  (:use :common-lisp
        :alexandria
        )

  (:export
   #:find-logger
   #:log-category
   #:deflogger
   #:with-logger-level
   #:log-level
   #:compile-time-log-level
   #:handle-log-message
   #:append-message
   #:ancestors-of
   #:appenders-of
   #:children-of

   #:+dribble+
   #:+debug+
   #:+info+
   #:+warn+
   #:+error+
   #:+fatal+

   #:stream-log-appender
   #:brief-stream-log-appender
   #:verbose-stream-log-appender
   #:make-stream-log-appender
   #:make-slime-repl-log-appender
   #:file-log-appender
   #:make-file-log-appender
   ))
