;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :common-lisp-user)

(defpackage :hu.dwim.logger
  (:use :hu.dwim.common-lisp)

  (:export #:find-logger
           #:log-category
           #:log-appender
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
           #:*log-directory*
           #:file-log-appender
           #:make-file-log-appender
           #:make-level-filter-appender))
