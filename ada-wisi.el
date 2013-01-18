;;; An indentation engine for Ada mode, using the wisent LALR parser
;;
;; [1] ISO/IEC 8652:2012(E); Ada 2012 reference manual
;;
;; Copyright (C) 2012, 2013  Free Software Foundation, Inc.
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
;;; History: first experimental version Oct 2012
;;
;;; code style
;;
;; not using lexical-binding or cl-lib because we support Emacs 23
;;
;; I don't use 'pcase', because it gives _really_ confusing errors
;; when I forget a ')' somewhere. Even worse, the error message is
;; given when you use edebug on a defun, not when you eval it. This
;; code is hard enough to debug!
;;
;;;;

(require 'ada-indent-user-options)
(require 'ada-grammar-wy)
(require 'wisi)

(defun ada-wisi-before-keyword ()
  (let ((cache (wisi-get-cache (point))))
    (when cache
      (ecase (wisi-cache-class cache)
	(block-start (wisi-indent-statement-start ada-indent (car (wisi-prev-cache))))
	(block-end (wisi-indent-statement-start 0 cache))
	(close-paren (wisi-indent-paren 0))
	((open-paren statement-start) nil); let after-keyword handle it
	(statement-middle (wisi-indent-statement-start ada-indent-when cache))
	))
    ))

(defun ada-wisi-after-keyword ()
  (let ((cache (car (wisi-prev-cache))))
    (if (not cache)
	;; bob
	0
      (ecase (wisi-cache-class cache)
	(block-end
	 (wisi-indent-current 0))

	(block-start
	 (wisi-indent-current ada-indent))

	(open-paren
	 (1+ (current-column)))

	(statement-end
	 (wisi-indent-statement-start 0 cache))

	(statement-middle;; when, else
	 (wisi-indent-current ada-indent))

	((statement-start close-paren)
	 ;; hanging
	 (wisi-indent-statement-start ada-indent-broken cache))
	))
    ))

;;; debugging
(defun ada-wisi-debug-keys ()
  "Add debug key definitions to `ada-mode-map'."
  (interactive)
  (define-key ada-mode-map "\M-j" 'wisi-show-cache)
  (define-key ada-mode-map "\M-k" 'wisi-show-token)
  )

;;;###autoload
(defun ada-wisi-setup ()
  "Set up a buffer for parsing Ada files with wisi."
  (wisi-setup '(gpr-wisi-before-keyword
		gpr-wisi-after-keyword)
	      ada-grammar-wy--keyword-table
	      ada-grammar-wy--token-table
	      ada-grammar-wy--parse-table)

  (set (make-local-variable 'comment-indent-function) 'wisi-comment-indent)

  (set (make-local-variable 'ada-which-function) 'ada-wisi-which-function)
  (set (make-local-variable 'ada-in-paramlist-p) 'ada-wisi-in-paramlist-p)
  (set (make-local-variable 'ada-scan-paramlist) 'ada-wisi-scan-paramlist)
  (set (make-local-variable 'ada-goto-declaration-start) 'ada-wisi-goto-declaration-start)
  (set (make-local-variable 'ada-goto-declarative-region-start) 'ada-wisi-goto-declarative-region-start)
  (set (make-local-variable 'ada-next-statement-keyword) 'ada-wisi-forward-statement-keyword-1)
  (set (make-local-variable 'ada-prev-statement-keyword) 'ada-wisi-backward-statement-keyword-1)
  (set (make-local-variable 'ada-make-subprogram-body) 'ada-wisi-make-subprogram-body)
  )

(add-hook 'ada-mode-hook 'ada-wisi-setup)

(provide 'ada-wisi)
(provide 'ada-indent-engine)

;; end of file
