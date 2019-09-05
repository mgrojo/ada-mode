;; xref-ada.el --- ada-mode backend for xref.el -*-lexical-binding:t-*-
;;
;; Copyright (C) 2018 - 2019  Free Software Foundation, Inc.

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

;;; Code:

(require 'ada-core)
(require 'wisi)
(require 'xref)

(cl-defmethod xref-backend-definitions ((_backend (eql xref-ada)) identifier)
  (wisi-xref-ident-make
   identifier
   (lambda (ident file line column)
     (wisi-check-current-project file)
     (let ((target
	    (wisi-xref-other (ada-prj-xref (ada-prj-require-prj))
			    ident file line column)))
       ;; FIXME: change wisi-xref-other to return xref-file-location?
       (list
	(xref-make
	 ident
	 (xref-make-file-location
	  (nth 0 target) ;; file
	  (nth 1 target) ;; line
	  (nth 2 target)) ;; column
	 ))))))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql xref-ada)))
  (save-excursion
    (condition-case nil
	(let ((ident (wisi-prj-identifier-at-point (project-current)))) ;; moves point to start of ident
	  (put-text-property
	   0 1
	   'xref-identifier
	   (list ':file (buffer-file-name)
		 ':line (line-number-at-pos)
		 ':column (current-column))
	   ident)
	  ident)
	(error
	 ;; from wisi-prj-identifier-at-point; no identifier
	 nil))))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql xref-ada)))
  (wisi-xref-names))

(provide 'xref-ada)
