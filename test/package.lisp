;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-yalog)

(defpackage :cl-yalog-test
  (:use :common-lisp
        :alexandria
        :stefil
        :cl-def
        :iterate
        :cl-syntax-sugar
        :cl-yalog
        )
  (:export
   #:test))

(import
 '()
 :cl-yalog-test)
