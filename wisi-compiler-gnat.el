;;; wisi-compiler-gnat.el --- wisi-compiler defmethods for GNAT compiler  -*- lexical-binding:t -*-
;;;
;; GNAT is provided by AdaCore; see http://libre.adacore.com/
;;
;;; Copyright (C) 2012 - 2019  Free Software Foundation, Inc.
;;
;; Author: Stephen Leake <stephen_leake@member.fsf.org>
;; Maintainer: Stephen Leake <stephen_leake@member.fsf.org>
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

(require 'cl-lib)
(require 'gnat-core)

(cl-defmethod wisi-compiler-parse-one ((compiler gnat-compiler) project name value)
  "Handle gnat-specific wisi project file settings.
If NAME recognized, update PROJECT, return t. Else return nil.
See also `gnat-prj-parse-emacs-final'."
  (cond
   ((or
     (string= name "ada_project_path") ;; backward compatibility
     (string= name "gpr_project_path"))
    (let ((process-environment
	   (append
	    (wisi-prj-compile-env project)
	    (wisi-prj-file-env project))));; reference, for substitute-in-file-name
      (gnat-prj-add-prj-dir project (expand-file-name (substitute-in-file-name value))))
    t)

   ((string= name "gpr_file")
    ;; The gpr file is parsed in `wisi-compiler-parse-final', so it
    ;; sees all file environment vars.
    (let ((process-environment
	   (append
	    (wisi-prj-compile-env project)
	    (wisi-prj-file-env project))));; reference, for substitute-in-file-name
      (setf (gnat-compiler-gpr-file compiler)
	    (or
	     (expand-file-name (substitute-in-file-name value))
	     (locate-file (substitute-in-file-name value)
			  (gnat-compiler-project-path compiler)))))
    t)
   ))

(cl-defmethod wisi-compiler-parse-final ((compiler gnat-compiler) project prj-file-name)
  (setf (gnat-compiler-run-buffer-name compiler) (gnat-run-buffer-name prj-file-name))

  (if (gnat-compiler-gpr-file compiler)
      (gnat-parse-gpr (gnat-compiler-gpr-file compiler) project)

    ;; add the compiler libraries to project.source-path
    (gnat-get-paths project)
    ))

(cl-defmethod wisi-compiler-select-prj ((_compiler gnat-compiler) _project)
  (add-to-list 'completion-ignored-extensions ".ali") ;; gnat library files
  (setq compilation-error-regexp-alist '(gnat))
  )

(cl-defmethod wisi-compiler-deselect-prj ((_compiler gnat-compiler) _project)
  (setq completion-ignored-extensions (delete ".ali" completion-ignored-extensions))
  (setq compilation-environment nil)
  (setq compilation-error-regexp-alist (mapcar #'car compilation-error-regexp-alist-alist))
  )

(cl-defmethod wisi-compiler-show-prj-path ((compiler gnat-compiler))
    (if (gnat-compiler-project-path compiler)
      (progn
	(pop-to-buffer (get-buffer-create "*project file search path*"))
	(erase-buffer)
	(dolist (file (gnat-compiler-project-path compiler))
	  (insert (format "%s\n" file))))
    (message "no project file search path set")
    ))

(provide 'wisi-compiler-gnat)
;; wisi-compiler-gnat.el ends here
