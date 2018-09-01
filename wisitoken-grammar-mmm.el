;;; Define multi-major-mode stuff for wisitoken-grammar mode.  -*- lexical-binding:t -*-

(require 'mmm-mode)

(mmm-add-classes
 '((wisi-action
    :match-submode wisi-mmm-submode
    :face mmm-code-submode-face
    :front "%("
    :back ")%"
    :insert ((?a wisi-action nil @ "%(" @ "" _ "" @ ")%")))
   ))

(defvar wisitoken-grammar-action-mode) ;; in wisitoken-grammar-mode.el
(defun wisi-mmm-submode (_delim)
  "for :match-submode"
  wisitoken-grammar-action-mode)

(add-to-list 'mmm-mode-ext-classes-alist '(wisitoken-grammar-mode nil wisi-action))

(provide 'wisitoken-grammar-mmm)
;;; end of file
