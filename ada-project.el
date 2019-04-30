;; ada-project.el - project.el backend for ada-mode projects -*- lexical-binding: t -*-
;;
;; Copyright (C) 2017 - 2019  Free Software Foundation, Inc.
;;
;; Author: Stephen Leake <stephen_leake@stephe-leake.org>
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

(require 'ada-mode)
(require 'env-project)
(require 'path-iterator)
(require 'uniquify-files)

(cl-defstruct (ada-project
	       (:include env-project)
	       (:constructor nil) ;; no default
	       (:constructor make-ada-project
			     (&key
			      env-vars
			      ada-prj-file
			      &aux
			      (ada-prj (expand-file-name ada-prj-file))))
	       )
  ada-prj
  ;; The ada-mode project file name (absolute).
  )

(cl-defmethod project-id ((prj ada-project))
  ;; project-id is experimental
  (ada-project-ada-prj prj))

(cl-defmethod project-roots ((_prj ada-project))
  ;; Not meaningful
  nil)

(cl-defmethod project-file-completion-table ((_prj ada-project) _dirs)
  ;; (ada-prj-get 'src_dir) is more accurate than project-*roots
  (cond
   ((fboundp 'uniq-file-get-data-string)
    ;; elpa package
    (let ((iter (make-path-iterator
		 :user-path-non-recursive (ada-prj-get 'src_dir)
		 :user-path-recursive nil
		 :ignore-function nil)))
      ;; FIXME: exclude .exe
      (apply-partially #'uniq-file-completion-table iter)
      ))

   ((fboundp 'uniq-file-uniquify)
    ;; emacs 27
    (let* ((iter (make-path-iterator
		:user-path-non-recursive (ada-prj-get 'src_dir)
		:user-path-recursive nil
		:ignore-function nil))
	   (files
	    (path-iter-files
	     iter
	     (lambda (abs-file-name)
	       (not (string-equal "exe" (file-name-extension abs-file-name))))))
	   (alist (uniq-file-uniquify files)))
      (apply-partially #'uniq-file-completion-table alist)
      ))
   ))

(cl-defmethod project-select :after ((prj ada-project))
  ;; :after ensures env-project project-select is run first, setting env vars.
  (ada-select-prj-file (ada-project-ada-prj prj)))

(cl-defmethod project-deselect :before ((prj ada-project))
  ;; :before ensures env vars are not erased before we are done with them.
  (ada-deselect-prj (ada-project-ada-prj prj)))

(cl-defmethod project-refresh ((_prj ada-project) full)
  ;; assume prj is current
  (ada-refresh-prj-file)
  (ada-xref-refresh full))

(provide 'ada-project)

;; end of file
