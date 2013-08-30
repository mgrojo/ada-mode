;;; gnat-xref.el --- minor-mode for navigating sources using GNAT
;;; cross reference tool.
;;
;;; Copyright (C) 2013  Free Software Foundation, Inc.

;; Author: Stephen Leake <stephen_leake@member.fsf.org>
;; Maintainer: Stephen Leake <stephen_leake@member.fsf.org>
;; Keywords: languages ada

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Usage:
;;
;; M-x gnat-xref-mode

(defgroup gnat-xref nil
  "Minor mode for navigating sources using GNAT cross reference tool."
  :group 'languages)

(defun gnat-xref-goto-declaration (other-window-frame &optional parent)
  "Move to the declaration or body of the identifier around point.
If at the declaration, go to the body, and vice versa.

OTHER-WINDOW-FRAME (default nil, set by interactive prefix)
controls window and frame choice:

nil     : show in current window
C-u     : show in other window
C-u C-u : show in other frame
"
  (interactive "P")
  (let ((target (ada-gnat-xref-other
		 (thing-at-point 'symbol)
		 (file-name-nondirectory (buffer-file-name))
		 (line-number-at-pos)
		 (car (bounds-of-thing-at-point 'symbol))
		 parent)))

    (ada-goto-source (nth 0 target)
		     (nth 1 target)
		     (nth 2 target)
		     other-window-frame)
    ))

(defun gnat-xref-show-references ()
  "Show declaration and all references of identifier at point."
  (interactive)
  (ada-gnat-xref-all (symbol-at-point)))


(defun gnat-xref-goto-declaration-parent ()
  "Move to the parent type declaration of the type identifier around point."
  (interactive)
  (gnat-xref-goto-declaration t))

(defvar-local gnat-xref-map
  (let ((map (make-sparse-keymap)))
    ;; C-c <letter> are reserved for users

    (define-key map "\C-c\C-d" 'gnat-xref-goto-declaration)
    (define-key map "\C-c\M-d" 'gnat-xref-goto-declaration-parent)
    (define-key map "\C-c\C-r" 'gnat-xref-all-references)
    map
  )  "Local keymap used for GNAT xref minor mode.")

(define-minor-mode gnat-xref
  "Minor mode for navigating sources using GNAT cross reference tool."
  :initial-value t
  :lighter       " gnat-xref"   ;; mode line
  )

;;; end of file
