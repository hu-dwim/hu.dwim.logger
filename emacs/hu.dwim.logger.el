;;; -*- encoding: utf-8 -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(provide 'hu.dwim.logger)

;; TODO make it work when the (log.debug ...) sexp is multiline (it's not trivial).
;; https://stackoverflow.com/questions/9452615/emacs-is-there-a-clear-example-of-multi-line-font-locking
;; https://www.emacswiki.org/emacs/MultilineRegexp
;; use rx macro instead of regex
;; to test matching:
;;  (progn
;;  (setq-local font-lock-keywords '(("\\(Operations:\\)\\s-*\\(.*\\)$" (2 font-lock-keyword-face))))
;;  (font-lock-fontify-buffer))

;; usage example in your init.el:
;; (setq dwim-workspace (getenv "DWIM_WORKSPACE"))
;; (add-to-list 'load-path (expand-file-name (concat dwim-workspace "/hu.dwim.logger/emacs/")))
;;
;; (require 'hu.dwim.logger)

(defgroup hu.dwim.logger.faces nil
  "Faces installed by hu.dwim.logger"
  :prefix "hu.dwim.logger"
  :group 'applications)

(defface font-lock-logger-expression-face
   ;;'((t (:inherit font-lock-comment-face)))
   '((((class color) (background light)) (:foreground "#aaa")))
  "Face for the (foo-bar.debug ...) log statements."
  :group 'hu.dwim.logger.faces)

(defun hu.dwim.logger.lisp-mode-hook ()
  (font-lock-add-keywords
   'lisp-mode
   ;; TODO this is broken for ( ( (log.debug))), all but the last closing paren gets the face
   `(("(\\(\\w+\\.\\(dribble\\|debug\\|info\\|warn\\|error\\|fatal\\) .*\\))" 1 'font-lock-logger-expression-face t))))

(add-hook 'lisp-mode-hook 'hu.dwim.logger.lisp-mode-hook)
