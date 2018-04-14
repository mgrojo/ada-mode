;;; Define multi-major-mode stuff for wisi_grammar mode.  -*- lexical-binding:t -*-

(require 'mmm-mode)

(mmm-add-classes
 '((wisi-action
    :match-submode wisi-mmm-submode
    :face mmm-code-submode-face
    :front "%("
    :back ")%"
    :insert ((?a wisi-action nil @ "%(" @ "" _ "" @ ")%")))
   ))

(defvar wisi-grammar-action-mode) ;; in wisi-grammar-mode.el
(defun wisi-mmm-submode (_delim)
  "for :match-submode"
  wisi-grammar-action-mode)

(add-to-list 'mmm-mode-ext-classes-alist '(wisi-grammar-mode nil wisi-action))

(provide 'wisi-grammar-mmm)
;;; end of file
