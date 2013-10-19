;;; ada-build.el --- extensions to ada-mode for compiling and running
;;; Ada projects (without 'make' or similar tool)
;;
;;; Copyright (C) 1994, 1995, 1997 - 2013  Free Software Foundation, Inc.
;;
;; Author: Stephen Leake <stephen_leake@member.fsf.org>
;; Maintainer: Stephen Leake <stephen_leake@member.fsf.org>
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
;;; Usage:
;;
;; Add (require 'ada-build) to your .emacs

;;; Design:
;;
;; Separate from ada-mode.el because sophisticated users don't need
;; this (they use 'make' or similar tool), so it would just get in the
;; way, particularly for fixing bugs in the core capabilities of
;; ada-mode.

;;; History:
;;
;; see ada-mode.el; the current code is a complete rewrite of the
;; compiling and running capabilities in Ada mode 4.01, done in 2013 by
;; Stephen Leake <stephen_leake@stephe-leake.org>.

(require 'ada-mode)

;;;; User customization

(defgroup ada-build nil
  "Major mode for compiling and running Ada projects in Emacs."
  :group 'ada)

(defcustom ada-build-prompt-prj 'default
  "Policy for finding a project file when none is currently selected."
  :type '(choice (const default)
		 (const prompt-default)
		 (const prompt-exist)
		 (const error))
  :group 'ada-build
  :safe  'symbolp)

(defcustom ada-build-confirm-command nil
  "If non-nil, prompt for confirmation/edit of each command before it is run."
  :type  'boolean
  :group 'ada-build
  :safe  'booleanp)

(defcustom ada-build-check-cmd
  (concat "${cross_prefix}gnatmake -u -c -gnatc ${gnatmake_opt} ${full_current} -cargs ${comp_opt}")
  "Default command to syntax check a single file.
Overridden by project variable 'check_cmd'."
  :type 'string
  :group 'ada-build)

(defcustom ada-build-comp-cmd
  (concat "${cross_prefix}gnatmake -u -c ${gnatmake_opt} ${full_current} -cargs ${comp_opt}")
  "Default command to compile a single file.
Overridden by project variable 'comp_cmd'."
  :type 'string
  :group 'ada-build)

(defcustom ada-build-make-cmd
  (concat "${cross_prefix}gnatmake -o ${main} ${main} ${gnatmake_opt} "
	  "-cargs ${comp_opt} -bargs ${bind_opt} -largs ${link_opt}")
  "Default command to compile the application.
Overridden by project variable 'make_cmd'."
  :type 'string
  :group 'ada-build)

(defcustom ada-build-run-cmd "./${main}"
  "Default command to run the application, in a spawned shell.
Overridden by project variable 'run_cmd'."
  :type 'string
  :group 'ada-build)

;;;; code

(defun ada-build-replace-vars (cmd-string)
  "Recursively expand variable references in CMD-STRING.
${var} is a project variable or environment variable, $var an
environment variable.

If the value of the project variable is a list, a prefix may be
specified with the format '-<prefix>${var}'; then the value is
expanded with the prefix prepended to each list element. For
example, if src_dir contains 'dir_1 dir_2', '-I${src_dir}' expands
to '-Idir_1 -Idir_2'.

As a special case, ${full_current} is replaced by the name
including the directory and extension."

  (while (string-match "\\(\\<-[^-]\\)?\${\\([^}]+\\)}" cmd-string)
    (let ((prefix (match-string 1 cmd-string))
	  (name (match-string 2 cmd-string))
	  value)

      (when (string= name "full_current")
	(setq value (buffer-file-name)))

      (when (null value)
	(setq value (ada-prj-get (intern name))))

      (when (null value)
	(setq value (getenv name)))

      (cond
       ((null value)
	(setq cmd-string (replace-match "" t t cmd-string)))

       ((stringp value)
	(setq cmd-string (replace-match value t t cmd-string)))

       ((listp value)
	(setq cmd-string (replace-match
			  (mapconcat (lambda (x) (concat prefix " " value)) value " ")
			    t t cmd-string)))
       )))

  (substitute-in-file-name cmd-string))

(defun ada-build-select-default-prj ()
  "Create and select a new default project, with current buffer as main program."
  (let ((prj-file (expand-file-name "default.adp"))
	project)

    (when (null (assoc prj-file ada-prj-alist))
      (setq
       project
       (append
	(ada-prj-default)
	(list
	 'check_cmd       ada-build-check-cmd
	 'comp_cmd        ada-build-comp-cmd
	 'main            (file-name-nondirectory
			   (file-name-sans-extension (buffer-file-name)))
	 'make_cmd        ada-build-make-cmd
	 'run_cmd         ada-build-run-cmd
	 )))

      (add-to-list 'ada-prj-alist (cons prj-file project))
      )

    (ada-select-prj-file prj-file)
  ))

(defun ada-build-require-project-file ()
  "Ensure that a project file is selected.
Action when no project file is currently selected is determined
by `ada-build-prompt-prj':

default - just use a default project; no gpr file, current
directory only, current file as main.

prompt-default - prompt for a project file; empty result gives
the default project.

prompt-exist - prompt for a project file; one must be
selected (or error)

error - throw an error (no prompt, no default project)."
  (unless ada-prj-current-file
    (cl-ecase ada-build-prompt-prj
      (default
	(ada-build-select-default-prj)))
    ))

;;;; user keys, menu, functions

(defvar ada-build-map
  (let ((map (make-sparse-keymap)))
    ;; C-c <letter> are reserved for users
    (define-key map "\C-c\C-v"  'ada-build-check)

    map)
  "Local keymap used for Ada build minor mode.")

(defvar ada-build-menu (make-sparse-keymap "Ada Build"))
(easy-menu-define ada-build-menu ada-build-map "Menu keymap for Ada Build minor mode"
  '("Build"
    ["Check syntax"               ada-build-check       t]
    ["Show main"                  ada-build-show-main   t]
    ["Build"                      ada-build-make        t]
    ["Set main and Build"         ada-build-set-make    t]
    ["Run"                        ada-build-run         t]
    ))

(defun ada-build-run-cmd (prj-field confirm prompt)
  "Run the command in the PRJ-FIELD project variable.
If CONFIRM or `ada-build-confirm-command' are non-nil, ask for
user confirmation of the command, using PROMPT."
  (ada-build-require-project-file)
  (let ((cmd (ada-prj-get prj-field))
	(process-environment (ada-prj-get 'proc_env)))

    (unless cmd
      (setq cmd '("")
	    confirm t))

    (when (or ada-build-confirm-command confirm)
      (setq cmd (read-from-minibuffer (concat prompt ": ") cmd)))

    (compile (ada-build-replace-vars cmd))))

(defun ada-build-check (&optional confirm)
  "Run the check_cmd project variable.
By default, this checks the current file for syntax errors.
If CONFIRM is non-nil, prompt for user confirmation of the command."
  (interactive "P")
  (ada-build-run-cmd 'check_cmd confirm "check command"))

(defun ada-build-make (&optional confirm)
  "Run the make_cmd project variable.
By default, this compiles and links the main program.
If CONFIRM is non-nil, prompt for user confirmation of the command."
  (interactive "P")
  (ada-build-run-cmd 'make_cmd confirm "make command"))

(defun ada-build-set-make (&optional confirm)
  "Set the main project variable to the current file, then run the make_cmd project variable.
By default, this compiles and links the new main program.
If CONFIRM is non-nil, prompt for user confirmation of the command."
  (interactive "P")
  (ada-prj-put 'main (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))
  (ada-build-run-cmd 'make_cmd confirm "make command"))

(defun ada-build-run (&optional confirm)
  "Run the run_cmd project variable.
By default, this runs the main program.
If CONFIRM is non-nil, prompt for user confirmation of the command."
  (interactive "P")
  (ada-build-run-cmd 'run_cmd confirm "run command"))

(defun ada-build-show-main ()
  "Show current project main program filename."
  (interactive)
  (message "Ada mode main: %s"(ada-prj-get 'main)))

;;;; setup

(define-minor-mode ada-build
  "Minor mode for compiling and running Ada projects.
Enable mode if ARG is positive"
  :initial-value t
  :lighter       " build"   ;; mode line

  ;; just enable the menu and keymap
  )

(provide 'ada-build)
;; end of file
