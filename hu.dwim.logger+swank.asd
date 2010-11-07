;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2010 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.logger+swank
  :class hu.dwim.system
  :depends-on (:hu.dwim.logger
               :swank)
  :components ((:module "integration"
                :components ((:file "swank")))))
