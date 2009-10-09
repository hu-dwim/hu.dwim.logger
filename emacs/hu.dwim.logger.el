;;; -*- encoding: utf-8 -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(provide 'hu.dwim.logger)

;; usage example in your init.el:
;;
;; (add-to-list 'load-path (expand-file-name "~/workspace/hu.dwim.logger/emacs/"))
;;
;; (require 'hu.dwim.logger)

(defgroup hu.dwim.logger.faces nil
  "Faces installed by hu.dwim.logger"
  :prefix "hu.dwim.logger"
  :group 'applications)

(defface font-lock-logger-expression-face
   '((t (:inherit font-lock-comment-face)))
   ;;'((((class color) (background light)) (:foreground "#aaa")))
  "Face for the (foo-bar.debug ...) log statements."
  :group 'hu.dwim.logger.faces)

(defun hu.dwim.logger.lisp-mode-hook ()
  (font-lock-add-keywords
   'lisp-mode
   `(("(\\(\\w+\\.\\(dribble\\|debug\\|info\\|warn\\|error\\|fatal\\) .*\\))" 1 'font-lock-logger-expression-face t))))

(add-hook 'lisp-mode-hook 'hu.dwim.logger.lisp-mode-hook)
