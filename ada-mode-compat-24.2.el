;;; ada-mode-compat-24.2.el --- Implement current Emacs features not present in Emacs 24.2

;; Copyright (C) 2014 Free Software Foundation, Inc.

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

(require 'cl)

(defalias 'cl-acons (symbol-function 'acons))
(defalias 'cl-caddr (symbol-function 'caddr))
(defalias 'cl-case (symbol-function 'case))
(defalias 'cl-defstruct (symbol-function 'defstruct))

;; 'defalias' doesn't work here; not clear why
(defun cl-delete-if (cl-pred cl-list &rest cl-keys)
  "Remove all items satisfying PREDICATE in SEQ.
This is a destructive function; it reuses the storage of SEQ whenever possible.
\nKeywords supported:  :key :count :start :end :from-end
\n(fn PREDICATE SEQ [KEYWORD VALUE]...)"
  (apply 'delete* nil cl-list :if cl-pred cl-keys))

(defalias 'cl-dolist (symbol-function 'dolist))
(defalias 'cl-ecase (symbol-function 'ecase))

(defmacro defvar-local (var val &optional docstring)
  "Define VAR as a buffer-local variable with default value VAL.
Like `defvar' but additionally marks the variable as being automatically
buffer-local wherever it is set."
  (declare (debug defvar) (doc-string 3))
  ;; Can't use backquote here, it's too early in the bootstrap.
  (list 'progn (list 'defvar var val docstring)
        (list 'make-variable-buffer-local (list 'quote var))))

(defun file-name-base (&optional filename)
  "Return the base name of the FILENAME: no directory, no extension.
FILENAME defaults to `buffer-file-name'."
  (file-name-sans-extension
   (file-name-nondirectory (or filename (buffer-file-name)))))


(provide 'ada-mode-compat-24.2)

;; end of file
