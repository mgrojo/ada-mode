;;; An indentation engine for gpr mode, using the wisent LALR parser
;;
;; [1] GNAT user guide (info "gnat_ugn")
;;
;; Copyright (C) 2013 Free Software Foundation, Inc.
;;
;; Author: Stephen Leake <stephen_leake@member.fsf.org>
;; Keywords: languages ada
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; History: first version Jan 2013
;;
;;; code style
;;
;; not using lexical-binding or cl-lib because we support Emacs 23
;;
;; 'ada-wisi' is short for "Ada mode wisent indentation engine".
;;
;; I don't use 'pcase', because it gives _really_ confusing errors
;; when I forget a ')' somewhere. Even worse, the error message is
;; given when you use edebug on a defun, not when you eval it. This
;; code is hard enough to debug!
;;
;;;;

;; we reuse some stuff from ada-mode
(require 'ada-indent-user-options)
(require 'gpr-grammar-wy)
(require 'semantic/wisent)
(require 'wisi)

(defun gpr-wisi-indent-calculate ()
  (let ((cache (wisi-get-cache (point))))
    (if cache
	;; point is at start of a wisi keyword
	(gpr-wisi-before-keyword cache)

      ;; else move to previous keyword
      (setq cache (wisi-prev-keyword))
      (if cache
	  (gpr-wisi-after-keyword)
	;; bob
	0))
  ))

(defun gpr-wisi-before-keyword (cache)
  (ecase (wisi-cache-class cache)
    ((block-start block-end) (wisi-indent-statement-start 0))
    (other (wisi-indent-statement-start ada-indent))
    ))

(defun gpr-wisi-after-keyword (cache)
  (ecase (wisi-cache-class cache)
    (block-start (wisi-indent-current ada-indent))
    ((block-end statement-end) (wisi-indent-current 0))
    (other ;; hanging
     (wisi-indent-statement-start ada-indent-broken))
    ))

;;; debugging
(defun gpr-wisi-debug-keys ()
  "Add debug key definitions to `gpr-mode-map'."
  (interactive)
  (define-key gpr-mode-map "\M-j" 'wisi-show-cache)
  )
;;;###autoload
(defun gpr-wisi-setup ()
  "Set up a buffer for parsing Ada files with wisi."
  (wisi-setup 'gpr-wisi-indent-calculate
	      gpr-grammar-wy--keyword-table
	      gpr-grammar-wy--parse-table)

  ;; FIXME: could just invalidate after point; let's see how slow this is
  (add-hook 'after-change-functions 'wisi-invalidate-cache nil t)

  (set (make-local-variable 'comment-indent-function) 'ada-wisi-comment-indent)

  ;; FIXME: implement?
  ;; (set (make-local-variable 'ada-next-statement-keyword) 'gpr-wisi-forward-statement-keyword-1)
  ;; (set (make-local-variable 'ada-prev-statement-keyword) 'gpr-wisi-backward-statement-keyword-1)
  )

(add-hook 'gpr-mode-hook 'gpr-wisi-setup)

(provide 'gpr-wisi)
(provide 'gpr-indent-engine)

;; end of file
