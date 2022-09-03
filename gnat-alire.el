;;; gnat-alire.el --- Support for building with Alire -*- lexical-binding:t -*-
;;
;;; Copyright (C) 2012 - 2022  Free Software Foundation, Inc.
;;
;; Author: Stephen Leake <stephen_leake@member.fsf.org>
;; Maintainer: Stephen Leake <stephen_leake@member.fsf.org>
;; Version: 1.0
;; package-requires: ((emacs "25.3") (wisi "4.0"))
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

;; See https://alire.ada.dev/

(require 'gnat-compiler)

(defun alire-get-env (project)
  "Set PROJECT slots from Alire."

  ;; WORKAROUND: alire 1.2.0 inherits GPR_PROJECT_PATH, which is just
  ;; wrong. So empty it here.
  (let ((process-environment (copy-sequence process-environment))
	value-string
	(local-exec-path exec-path))
    (setenv "GPR_PROJECT_PATH" "")

    (with-temp-buffer
      (call-process "alr" nil (current-buffer) nil "printenv")

      (goto-char (point-min))
      (search-forward "GPR_PROJECT_PATH=")
      (setq value-string (buffer-substring-no-properties (1+ (point)) (1- (line-end-position))))

      (setf (wisi-prj-file-env project)
	    (list (concat "GPR_PROJECT_PATH=" value-string)))

      ;; gnat-compiler use same compiler as Alire
      (goto-char (point-min))
      (search-forward "GNAT_NATIVE_ALIRE_PREFIX=")
      (setq value-string (buffer-substring-no-properties (1+ (point)) (1- (line-end-position))))
      (push (concat value-string "/bin") local-exec-path)
      (push (concat "PATH=" (mapconcat 'identity local-exec-path  path-separator))
	    (wisi-prj-file-env project))
      )))

;;;###autoload
(cl-defun create-alire-project (&key prj-name prj-file gpr-file)
  ;; WORKAROUND: there is no way to get the gpr-file named in
  ;; alire.toml, so we require it as an argument; must be absolute or
  ;; relative to Alire root directory.
  ;;
  ;; prj-file should _not_ specify the gpr-file or gpr-project-path;
  ;; it is only used for casing. We get GPR_PROJECT_PATH from the
  ;; Alire environment.
  "Return a wisi project for the Alire workspace containing `default-directory'"
  (let* ((default-directory (locate-dominating-file default-directory "alire.toml"))
	 (abs-gpr-file (expand-file-name gpr-file))
	 (project (make-wisi-prj :name prj-name))
	 )

    (alire-get-env project)

    ;; We use a gnat-compiler to set compilation-search-path.
    (setf (wisi-prj-compiler project)
	  (create-gnat-compiler
	   :gpr-file abs-gpr-file
	   :run-buffer-name (gnat-run-buffer-name abs-gpr-file)))
    (gnat-get-paths project)

    ;; Now we parse prj-file; casing can use (wisi-prGPR_PROJECT_PATH
    (wisi-prj-parse-file
     :prj-file prj-file
     :init-prj project
     :cache t)))

(provide 'gnat-alire)
;;; gnat-alire.el ends here
