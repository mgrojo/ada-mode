;; Support for running GNAT tools, which support multiple programming
;; languages.
;;
;; GNAT is provided by AdaCore; see http://libre.adacore.com/
;;
;;; Copyright (C) 2012, 2013  Free Software Foundation, Inc.
;;
;; Author: Stephen Leake <stephen_leake@member.fsf.org>
;; Maintainer: Stephen Leake <stephen_leake@member.fsf.org>
;; Keywords: languages compilers tools ada C C++
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

;; We use cl-delete-if, defined in cl-seq.el. cl-seq.el has no
;; 'provide'.  autoload for cl-delete-if is defined in
;; cl-loaddefs.el, which is not pre-loaded, so we load it here.
;; FIXME: asking on emacs-devel if this is the right way
(eval-and-compile (load "cl-loaddefs.el"))

;;;;; code

;;;; project file handling

;; FIXME: move more project functions here from ada-mode.el

(defun gnat-prj-add-prj-dir (dir project)
  "Add DIR to 'prj_dir and to GPR_PROJECT_PATH in 'proc_env. Return new project."
  (let ((prj-dir (plist-get project 'prj_dir))
	(proc-env (plist-get project 'proc_env)))

    (cond
     ((listp prj-dir)
	(add-to-list 'prj-dir dir))

     (prj-dir
      (setq prj-dir (list dir)))

     (t nil))

    (setq project (plist-put project 'prj_dir prj-dir))

    (add-to-list 'proc-env
		 (concat "GPR_PROJECT_PATH="
			 (mapconcat 'identity
				    (plist-get project 'prj_dir)
				    (plist-get project 'path_sep))))

    (setq project (plist-put project 'proc_env proc-env))

    project))

(defun gnat-prj-parse-emacs-one (name value project)
  "Handle gnat-specific Emacs Ada project file settings.
Return new PROJECT if NAME recognized, nil otherwise.
See also `ada-gnat-parse-emacs-final'."
  (let ((process-environment (plist-get project 'proc_env))); for substitute-in-file-name
    (cond
     ((or
       ;; we allow either name here for backward compatibility
       (string= name "gpr_project_path")
       (string= name "ada_project_path"))
      ;; We maintain two project values for this;
      ;; 'prj_dir - a list of directories, for gpr-ff-special-with
      ;; GPR_PROJECT_PATH in 'proc_env, for gnat-run
      (gnat-prj-add-prj-dir (expand-file-name (substitute-in-file-name value)) project))

     ((string= (match-string 1) "gpr_file")
      ;; The file is parsed in `gnat-parse-emacs-prj-file-final', so
      ;; it can add to user-specified src_dir.
      (setq project
	    (plist-put project
		       'gpr_file
		       (expand-file-name (substitute-in-file-name value))))
      project)
     )))

(defun gnat-prj-parse-emacs-final (project)
  "Final processing of gnat-specific Emacs Ada project file settings."
  (when (buffer-live-p (get-buffer (gnat-run-buffer-name)))
    (kill-buffer (gnat-run-buffer-name))); things may have changed, force re-create

  (if (ada-prj-get 'gpr_file project)
      (set 'project (gnat-parse-gpr (ada-prj-get 'gpr_file project) project))

    ;; add the compiler libraries to src_dir
    (setq project (gnat-get-paths project))
    )

  ;; This is only needed when actually running the gnat compiler;
  ;; parsing a gnat project is a crude proxy for that. Could set in an
  ;; 'ada-compile' function, but there's no good way to know when to
  ;; clear it. Same for compilation-error-regexp-alist. So we do this
  ;; here, and assume other modes will set these variables
  ;; appropriately.
  ;;
  ;; One possible approach is per-project compilation buffers; then
  ;; these variables could be buffer-local.
  ;;
  ;; Or can we use compilation-[start|finish]-functions to set and remove this?
  ;; FIXME: move to gnat-select-prj-compile?
  (setq compilation-filter-hook nil)
  (add-hook 'compilation-filter-hook 'ada-gnat-compilation-filter)

  ;; ada-mode.el project file parser sets this to other compilers used
  ;; in the project, so we only add here.
  (add-to-list 'compilation-error-regexp-alist 'gnat)

  project)

(defun gnat-get-paths (project)
  "Add project and/or compiler source, object paths to PROJECT src_dir."
  (with-current-buffer (gnat-run-buffer)
    (let ((status (gnat-run-gnat (list "list" "-v")))
	  (src-dirs (ada-prj-get 'src_dir project))
	  (prj-dirs (ada-prj-get 'prj_dir project)))

      ;; gnat list -P -v returns 0 in nominal cases
      ;; gnat list -v return 4, but still lists compiler dirs
      (when (not (member status '(0 4)))
	(pop-to-buffer (current-buffer))
	(error "gnat list returned status %d" status))

      (goto-char (point-min))

      (condition-case nil
	  (progn
	    ;; Source path
	    (search-forward "Source Search Path:")
	    (forward-line 1)
	    (while (not (looking-at "^$")) ; terminate on blank line
	      (back-to-indentation) ; skip whitespace forward
	      (if (looking-at "<Current_Directory>")
		  (add-to-list 'src-dirs  (directory-file-name default-directory))
		(add-to-list 'src-dirs
			     (expand-file-name ; canonicalize path part
			      (directory-file-name
			       (buffer-substring-no-properties (point) (point-at-eol))))))
	      (forward-line 1))

	    ;; Project path
	    (search-forward "Project Search Path:")
	    (forward-line 1)
	    (while (not (looking-at "^$"))
	      (back-to-indentation)
	      (if (looking-at "<Current_Directory>")
		  (add-to-list 'prj-dirs ".")
		(add-to-list 'prj-dirs
			     (expand-file-name
			      (buffer-substring-no-properties (point) (point-at-eol)))))
	      (forward-line 1))

	    )
	('error
	 (pop-to-buffer (current-buffer))
	 ;; search-forward failed
	 (error "parse gpr failed")
	 ))

      (setq project (plist-put project 'src_dir (reverse src-dirs)))
      (mapc (lambda (dir) (gnat-prj-add-prj-dir dir project))
	    (reverse prj-dirs))
      ))
  project)

(defun gnat-parse-gpr (gpr-file project)
  "Append to src_dir and prj_dir in PROJECT by parsing GPR-FILE.
Return new value of PROJECT.
GPR-FILE must be full path to file, normalized.
src_dir will include compiler runtime."
  ;; this can take a long time; let the user know what's up
  (message "Parsing %s ..." gpr-file)

  (if (ada-prj-get 'gpr_file project)
      ;; gpr-file defined in Emacs Ada mode project file
      (when (not (equal gpr-file (ada-prj-get 'gpr_file project)))
	(error "Ada project file %s defines a different GNAT project file than %s"
	       ada-prj-current-file
	       gpr-file))

    ;; gpr-file is top level Ada mode project file
    (setq project (plist-put project 'gpr_file gpr-file))
    )

  (setq project (gnat-get-paths project))

  (message "Parsing %s ... done" gpr-file)
  project)

;;;; command line tool interface

(defun gnat-run-buffer-name ()
  (concat " *gnat-run-"
	  (or (ada-prj-get 'gpr_file)
	      ada-prj-current-file)
	  "*"))

(defun gnat-run-buffer ()
  "Return a buffer suitable for running gnat command line tools for the current project."
  (ada-require-project-file)
  (let* ((buffername (gnat-run-buffer-name))
	 (buffer (get-buffer buffername)))
    (if buffer
	buffer
      (setq buffer (get-buffer-create buffername))
      (with-current-buffer buffer
	(setq default-directory
	      (file-name-directory
	       (or (ada-prj-get 'gpr_file)
		   ada-prj-current-file)))
	)
      buffer)))

(defun gnat-run (exec command)
  "Run a gnat command line tool, as \"EXEC COMMAND\".
EXEC must be an executable found on `exec-path'. COMMAND must be a list of strings.
Return process status.
Assumes current buffer is (gnat-run-buffer)"
  (set 'buffer-read-only nil)
  (erase-buffer)

  (setq command (cl-delete-if 'null command))

  (let ((process-environment (ada-prj-get 'proc_env))) ;; for GPR_PROJECT_PATH
    (insert (format "GPR_PROJECT_PATH=%s\n%s" (getenv "GPR_PROJECT_PATH") exec)); for debugging
    (mapc (lambda (str) (insert (concat str " "))) command); for debugging
    (newline)
    (apply 'call-process exec nil t nil command)
    ))

(defun gnat-run-gnat (command &optional switches-args)
  "Run the \"gnat\" command line tool, as \"gnat COMMAND -P<prj> SWITCHES-ARGS\".
COMMAND and SWITCHES-ARGS must be a string or list of strings.
Return process status.
Assumes current buffer is (ada-gnat-run-buffer)"
  (let* ((project-file-switch
	  (when (ada-prj-get 'gpr_file)
	    (concat "-P" (file-name-nondirectory (ada-prj-get 'gpr_file)))))
	 (cmd (append command (list project-file-switch) switches-args)))

    (gnat-run "gnat" cmd)
    ))

(defun gnat-run-no-prj (command &optional dir)
  "Run the gnat command line tool, as \"gnat COMMAND\", with DIR as current directory.
Return process status.  Assumes current buffer
is (ada-gnat-run-buffer)"
  (set 'buffer-read-only nil)
  (erase-buffer)

  (let ((default-directory (or dir default-directory)))

    (setq command (cl-delete-if 'null command))
    (mapc (lambda (str) (insert (concat str " "))) command)
    (newline)
    (apply 'call-process "gnat" nil t nil command)
    ))

(provide 'gnat-core)

;; end of file
