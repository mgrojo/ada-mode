;; gnat-core.el --- Support for running GNAT tools, which support multiple programming  -*- lexical-binding:t -*-
;; languages.
;;
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
(require 'ada-core)

(defvar gpr-query--sessions nil) ;; gpr-query.el
(declare-function gpr-query-kill-session "gpr-query.el" (session) )

;;;;; code

(defcustom ada-gnat-debug-run nil
  "If t, compilation buffers containing a GNAT command will show
the command.  Otherwise, they will show only the output of the
command.  This applies e.g. to *gnatfind* buffers."
  :type 'boolean
  :safe  #'booleanp
  :group 'ada)

;;;; project file handling

(defun gnat-prj-add-prj-dir (dir project)
  "Add DIR to 'prj_dir and to GPR_PROJECT_PATH in PROJECT plist 'proc_env."
  (let ((prj-dir (plist-get (ada-prj-plist project) 'prj_dir)))

    (cond
     ((listp prj-dir)
      (cl-pushnew dir prj-dir :test #'equal))

     (prj-dir
      (setq prj-dir (list dir)))

     (t nil))

    (setf (ada-prj-plist project) (plist-put (ada-prj-plist project) 'prj_dir prj-dir))

    (let ((process-environment (cl-copy-list (plist-get (ada-prj-plist project) 'proc_env))))
      (setenv "GPR_PROJECT_PATH"
	      (mapconcat 'identity
			 (plist-get (ada-prj-plist project) 'prj_dir)
			 (plist-get (ada-prj-plist project) 'path_sep)))

      (setf (ada-prj-plist project) (plist-put (ada-prj-plist project) 'proc_env (cl-copy-list process-environment)))
      )))

(defun gnat-prj-show-prj-path ()
  "For `ada-prj-show-prj-path'."
    (interactive)
  (if (plist-get 'prj_dir (ada-prj-plist ada-prj-current-project))
      (progn
	(pop-to-buffer (get-buffer-create "*GNAT project file search path*"))
	(erase-buffer)
	(dolist (file (plist-get (ada-prj-plist ada-prj-current-project) 'prj_dir))
	  (insert (format "%s\n" file))))
    (message "no project file search path set")
    ))

(defun ada-gnat-default-prj (prj)
  "For `ada-prj-default-list'."
  (gnat-prj-add-prj-dir default-directory prj))

(defun gnat-prj-parse-emacs-one (name value project)
  "Handle gnat-specific Emacs Ada project file settings.
If NAME recognized, update PROJECT, return t. Else return nil.
See also `gnat-prj-parse-emacs-final'."
  (let ((process-environment (cl-copy-list (plist-get (ada-prj-plist project) 'proc_env)))); for substitute-in-file-name
    (cond
     ((or
       ;; we allow either name here for backward compatibility
       (string= name "gpr_project_path")
       (string= name "ada_project_path"))
      ;; We maintain two project values for this;
      ;; 'prj_dir - a list of directories, for gpr-ff-special-with
      ;; GPR_PROJECT_PATH in 'proc_env, for gnat-run
      (gnat-prj-add-prj-dir (expand-file-name (substitute-in-file-name value)) project)
      t)

     ((string= (match-string 1) "gpr_file")
      ;; The file is parsed in `gnat-parse-emacs-prj-file-final', so
      ;; it can add to user-specified src_dir.
      (setf (ada-prj-plist project)
	    (plist-put (ada-prj-plist project)
		       'gpr_file
		       (or
			(locate-file (substitute-in-file-name value)
				     (plist-get (ada-prj-plist ada-prj-current-project) 'prj_dir))
			(expand-file-name (substitute-in-file-name value)))))
      t)
     )))

(defun gnat-prj-parse-emacs-final (project)
  "Final processing of gnat-specific Emacs Ada project file settings."
  ;; things may have changed, force re-create gnat or gpr-query sessions.
  (cl-ecase (plist-get (ada-prj-plist project) 'xref_tool)
    (gnat
     (when (buffer-live-p (get-buffer (gnat-run-buffer-name project)))
       (kill-buffer (gnat-run-buffer-name project))))

    (gpr_query
     (let ((session (cdr (assoc ada-prj-current-file gpr-query--sessions))))
       (when session
	 (gpr-query-kill-session session))))
     )

  (if (plist-get (ada-prj-plist project) 'gpr_file)
      (gnat-parse-gpr (plist-get (ada-prj-plist project) 'gpr_file) project)

    ;; add the compiler libraries to src_dir
    (gnat-get-paths project)
    ))

(defun gnat-get-paths-1 (project src-dirs obj-dirs prj-dirs)
  "Append list of source, project and object dirs in current gpr project to SRC-DIRS,
OBJ-DIRS and PRJ-DIRS. Uses `gnat list'.  Returns new (SRC-DIRS OBJ-DIRS PRJ-DIRS)."
  (with-current-buffer (gnat-run-buffer project)
    ;; gnat list -v -P can return status 0 or 4; always lists compiler dirs
    ;;
    ;; WORKAROUND: GNAT 7.2.1 gnatls does not support C++ fully; it
    ;; does not return src_dirs from C++ projects (see AdaCore ticket
    ;; M724-045). The workaround is to include the src_dirs in an
    ;; Emacs Ada mode project.
    (gnat-run-gnat project "list" (list "-v") '(0 4))

    (goto-char (point-min))

    (condition-case nil
	(progn
	  ;; Source path
	  (search-forward "Source Search Path:")
	  (forward-line 1)
	  (while (not (looking-at "^$")) ; terminate on blank line
	    (back-to-indentation) ; skip whitespace forward
            (cl-pushnew
	     (if (looking-at "<Current_Directory>")
		 (directory-file-name default-directory)
	       (expand-file-name ; Canonicalize path part.
		(directory-file-name
		 (buffer-substring-no-properties (point) (point-at-eol)))))
	     src-dirs
	     :test #'equal)
	    (forward-line 1))

          ;; Object path
          (search-forward "Object Search Path:")
          (forward-line 1)
	  (while (not (looking-at "^$")) ; terminate on blank line
	    (back-to-indentation) ; skip whitespace forward
            (cl-pushnew
	     (if (looking-at "<Current_Directory>")
		 (directory-file-name default-directory)
	       (expand-file-name ; Canonicalize path part.
		(directory-file-name
		 (buffer-substring-no-properties (point) (point-at-eol)))))
	     obj-dirs
	     :test #'equal)
	    (forward-line 1))

	  ;; Project path
	  ;;
	  ;; These are also added to src_dir, so compilation errors
	  ;; reported in project files are found.
	  (search-forward "Project Search Path:")
	  (forward-line 1)
	  (while (not (looking-at "^$"))
	    (back-to-indentation)
	    (if (looking-at "<Current_Directory>")
                (cl-pushnew (directory-file-name default-directory) prj-dirs :test #'equal)
              (let ((f (expand-file-name
                        (buffer-substring-no-properties (point) (point-at-eol)))))
                (cl-pushnew f prj-dirs :test #'equal)
                (cl-pushnew f src-dirs :test #'equal)))
	    (forward-line 1))

	  )
      (error
       (pop-to-buffer (current-buffer))
       ;; search-forward failed
       (error "parse gpr failed")
       ))
    (list (cl-remove-duplicates src-dirs) (cl-remove-duplicates obj-dirs) (cl-remove-duplicates prj-dirs))))

;; FIXME: use a dispatching function instead, with autoload, to
;; avoid "require" here, and this declare. or drop 'gnat', assume gpr-query.
;; Using 'require' at top level gives the wrong default ada-xref-tool
;; FIXME: test with gnatxref, or delete 'gnat'
(declare-function gpr-query-get-src-dirs "gpr-query.el" (src-dirs))
(declare-function gpr-query-get-prj-dirs "gpr-query.el" (prj-dirs))
(defun gnat-get-paths (project)
  "Add project and/or compiler source, object, project paths to PROJECT src_dir, obj_dir and/or prj_dir."
  (let ((src-dirs (plist-get (ada-prj-plist project) 'src_dir))
        (obj-dirs (plist-get (ada-prj-plist project) 'obj_dir))
	(prj-dirs (plist-get (ada-prj-plist project) 'prj_dir)))

    (cl-ecase (plist-get (ada-prj-plist project) 'xref_tool)
      (gnat
       (let ((res (gnat-get-paths-1 project src-dirs obj-dirs prj-dirs)))
	 (setq src-dirs (pop res))
         (setq obj-dirs (pop res))
	 (setq prj-dirs (pop res))))

      (gpr_query
       (when (plist-get (ada-prj-plist project) 'gpr_file)
	 (require 'gpr-query)
	 (setq src-dirs (gpr-query-get-src-dirs src-dirs))
	 (setq obj-dirs nil) ;; gpr-query does not provide obj-dirs
	 (setq prj-dirs (gpr-query-get-prj-dirs prj-dirs))))
      )

    (setf (ada-prj-plist project) (plist-put (ada-prj-plist project) 'src_dir (reverse src-dirs)))
    (setf (ada-prj-plist project) (plist-put (ada-prj-plist project) 'obj_dir (reverse obj-dirs)))
    (mapc (lambda (dir) (gnat-prj-add-prj-dir dir project))
	  (reverse prj-dirs))
    ))

(defun gnat-parse-gpr (gpr-file project)
  "Append to src_dir and prj_dir in PROJECT by parsing GPR-FILE.
Return new value of PROJECT.
GPR-FILE must be full path to file, normalized.
src_dir will include compiler runtime."
  ;; this can take a long time; let the user know what's up
  (message "Parsing %s ..." gpr-file)

  (if (plist-get (ada-prj-plist project) 'gpr_file)
      ;; gpr-file defined in Emacs Ada mode project file
      (when (not (equal gpr-file (plist-get (ada-prj-plist project) 'gpr_file)))
	(error "Ada project file %s defines a different GNAT project file than %s"
	       ada-prj-current-file
	       gpr-file))

    ;; gpr-file is top level Ada mode project file
    (plist-put (ada-prj-plist project) 'gpr_file gpr-file)
    )

  (condition-case-unless-debug nil
      ;; Can fail due to gpr_query not installed, or bad gpr file
      ;; syntax; allow .prj file settings to still work.
      (gnat-get-paths project)
    (message "Parsing %s ... done" gpr-file)
    (error
       (message "Parsing %s ... error" gpr-file))
    )

  project)

;;;; command line tool interface

(defun gnat-run-buffer-name (project &optional prefix)
  (concat (or prefix " *gnat-run-")
	  (or (plist-get (ada-prj-plist project) 'gpr_file)
	      ada-prj-current-file)
	  "*"))

(defun gnat-run-buffer (project &optional buffer-name-prefix)
  "Return a buffer suitable for running gnat command line tools for the current project."
  (ada-require-project-file)
  (let* ((name (gnat-run-buffer-name project buffer-name-prefix))
	 (buffer (get-buffer name)))
    (if buffer
	buffer
      (setq buffer (get-buffer-create name))
      (with-current-buffer buffer
	(setq default-directory
	      (file-name-directory
	       (or (plist-get (ada-prj-plist project) 'gpr_file)
		   ada-prj-current-file)))
	)
      buffer)))

(defun ada-gnat-show-run-buffer ()
  (interactive)
  (pop-to-buffer (gnat-run-buffer ada-prj-current-project)))

(defun gnat-run (project exec command &optional err-msg expected-status)
  "Run a gnat command line tool, as \"EXEC COMMAND\".
EXEC must be an executable found on `exec-path'.
COMMAND must be a list of strings.
ERR-MSG must be nil or a string.
EXPECTED-STATUS must be nil or a list of integers; throws an error if
process status is not a member.

Return process status.
Assumes current buffer is (gnat-run-buffer)"
  (set 'buffer-read-only nil)
  (erase-buffer)

  (setq command (cl-delete-if 'null command))

  (let ((process-environment (cl-copy-list (plist-get (ada-prj-plist project) 'proc_env))) ;; for GPR_PROJECT_PATH
	status)

    (when ada-gnat-debug-run
      (insert (format "GPR_PROJECT_PATH=%s\n%s " (getenv "GPR_PROJECT_PATH") exec))
      (mapc (lambda (str) (insert (concat str " "))) command)
      (newline))

    (setq status (apply 'call-process exec nil t nil command))
    (cond
     ((memq status (or expected-status '(0))); success
      nil)

     (t ; failure
      (pop-to-buffer (current-buffer))
      (if err-msg
	  (error "%s %s failed; %s" exec (car command) err-msg)
	(error "%s %s failed" exec (car command))
	))
     )))

(defun gnat-run-gnat (project command &optional switches-args expected-status)
  "Run the \"gnat\" command line tool, as \"gnat COMMAND -P<prj> SWITCHES-ARGS\".
COMMAND must be a string, SWITCHES-ARGS a list of strings.
EXPECTED-STATUS must be nil or a list of integers.
Return process status.
Assumes current buffer is (gnat-run-buffer)"
  (let* ((project-file-switch
	  (when (plist-get (ada-prj-plist project) 'gpr_file)
	    (concat "-P" (file-name-nondirectory (plist-get (ada-prj-plist project) 'gpr_file)))))
         (target-gnat (concat (plist-get (ada-prj-plist project) 'target) "gnat"))
         ;; gnat list understands --RTS without a fully qualified
         ;; path, gnat find (in particular) doesn't (but it doesn't
         ;; need to, it uses the ALI files found via the GPR)
         (runtime
          (when (and (plist-get (ada-prj-plist project) 'runtime) (string= command "list"))
            (list (concat "--RTS=" (plist-get (ada-prj-plist project) 'runtime)))))
	 (cmd (append (list command) (list project-file-switch) runtime switches-args)))

    (gnat-run project target-gnat cmd nil expected-status)
    ))

(defun gnat-run-no-prj (command &optional dir)
  "Run the gnat command line tool, as \"gnat COMMAND\", with DIR as current directory.
Return process status.  Process output goes to current buffer,
which is displayed on error."
  (set 'buffer-read-only nil)
  (erase-buffer)

  (when ada-gnat-debug-run
    (setq command (cl-delete-if 'null command))
    (mapc (lambda (str) (insert (concat str " "))) command)
    (newline))

  (let ((default-directory (or dir default-directory))
	status)

    (setq status (apply 'call-process "gnat" nil t nil command))
    (cond
     ((= status 0); success
      nil)

     (t ; failure
      (pop-to-buffer (current-buffer))
      (error "gnat %s failed" (car command)))
     )))

;;;; gnatprep utils

(defun gnatprep-indent ()
  "If point is on a gnatprep keyword, return indentation column
for it. Otherwise return nil.  Intended to be added to
`wisi-indent-calculate-functions' or other indentation function
list."
  ;; gnatprep keywords are:
  ;;
  ;; #if identifier [then]
  ;; #elsif identifier [then]
  ;; #else
  ;; #end if;
  ;;
  ;; they are all indented at column 0.
  (when (equal (char-after) ?\#) 0))

(defun gnatprep-syntax-propertize (start end)
  (goto-char start)
  (save-match-data
    (while (re-search-forward
	    "^[ \t]*\\(#\\(?:if\\|else\\|elsif\\|end\\)\\)"; gnatprep keywords.
	    end t)
      (cond
       ((match-beginning 1)
	(put-text-property
	 (match-beginning 1) (match-end 1) 'syntax-table '(11 . ?\n)))
       )
      )))

(defun gnatprep-setup ()
  (when (boundp 'wisi-indent-calculate-functions)
    (add-to-list 'wisi-indent-calculate-functions 'gnatprep-indent))
  )

;;;; support for xref tools
(defun ada-gnat-file-name-from-ada-name (ada-name)
  "For `ada-file-name-from-ada-name'."
  (let ((result nil))

    (while (string-match "\\." ada-name)
      (setq ada-name (replace-match "-" t t ada-name)))

    (setq ada-name (downcase ada-name))

    (with-current-buffer (gnat-run-buffer ada-prj-current-project)
      (gnat-run-no-prj
       (list
	"krunch"
	ada-name
	;; "0" means only krunch GNAT library names
	"0"))

      (goto-char (point-min))
      (when ada-gnat-debug-run (forward-line 1)); skip  cmd
      (setq result (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
      )
    result))

(defconst ada-gnat-predefined-package-alist
  '(
    ("a-chahan" . "Ada.Characters.Handling")
    ("a-comlin" . "Ada.Command_Line")
    ("a-contai" . "Ada.Containers")
    ("a-direct" . "Ada.Directories")
    ("a-except" . "Ada.Exceptions")
    ("a-ioexce" . "Ada.IO_Exceptions")
    ("a-finali" . "Ada.Finalization")
    ("a-numeri" . "Ada.Numerics")
    ("a-nuflra" . "Ada.Numerics.Float_Random")
    ("a-stream" . "Ada.Streams")
    ("a-ststio" . "Ada.Streams.Stream_IO")
    ("a-string" . "Ada.Strings")
    ("a-strmap" . "Ada.Strings.Maps")
    ("a-strunb" . "Ada.Strings.Unbounded")
    ("a-stwiun" . "Ada.Strings.Wide_Unbounded")
    ("a-textio" . "Ada.Text_IO")
    ("g-comlin" . "GNAT.Command_Line")
    ("g-dirope" . "GNAT.Directory_Operations")
    ("g-socket" . "GNAT.Sockets")
    ("i-c"      . "Interfaces.C")
    ("i-cstrin" . "Interfaces.C.Strings")
    ("interfac" . "Interfaces")
    ("s-stoele" . "System.Storage_Elements")
    )
  "Alist (filename . package name) of GNAT file names for predefined Ada packages.")

(defun ada-gnat-ada-name-from-file-name (file-name)
  "For `ada-ada-name-from-file-name'."
  (let* ((ada-name (file-name-sans-extension (file-name-nondirectory file-name)))
	 (predefined (cdr (assoc ada-name ada-gnat-predefined-package-alist))))

    (if predefined
        predefined
      (while (string-match "-" ada-name)
	(setq ada-name (replace-match "." t t ada-name)))
      ada-name)))

(defun ada-gnat-make-package-body (project body-file-name)
  "For `ada-make-package-body'."
  ;; WORKAROUND: gnat stub 7.1w does not accept aggregate project files,
  ;; and doesn't use the gnatstub package if it is in a 'with'd
  ;; project file; see AdaCore ticket LC30-001. On the other hand we
  ;; need a project file to specify the source dirs so the tree file
  ;; can be generated. So we use gnat-run-no-prj, and the user
  ;; must specify the proper project file in gnat_stub_opts.
  ;; FIXME: update to gnat 2019
  ;;
  ;; gnatstub always creates the body in the current directory (in the
  ;; process where gnatstub is running); the -o parameter may not
  ;; contain path info. So we pass a directory to gnat-run-no-prj.
  (let ((start-buffer (current-buffer))
	(start-file (buffer-file-name))
	(opts (when (plist-get (ada-prj-plist project) 'gnat_stub_opts)
		(split-string (plist-get (ada-prj-plist project) 'gnat_stub_opts))))
	(switches (when (plist-get (ada-prj-plist project) 'gnat_stub_switches)
		    (split-string (plist-get (ada-prj-plist project) 'gnat_stub_switches))))
	(process-environment (cl-copy-list (plist-get (ada-prj-plist project) 'proc_env))) ;; for GPR_PROJECT_PATH
	)

    ;; Make sure all relevant files are saved to disk.
    (save-some-buffers t)
    (with-current-buffer (gnat-run-buffer project)
      (gnat-run-no-prj
       (append (list "stub") opts (list start-file "-cargs") switches)
       (file-name-directory body-file-name))

      (find-file body-file-name)
      (indent-region (point-min) (point-max))
      (save-buffer)
      (set-buffer start-buffer)
      )
    nil))

(defun ada-gnat-syntax-propertize (start end)
  (goto-char start)
  (save-match-data
    (while (re-search-forward
	    (concat
	     "[^a-zA-Z0-9)]\\('\\)\\[[\"a-fA-F0-9]+\"\\]\\('\\)"; 1, 2: non-ascii character literal, not attributes
	     "\\|\\(\\[\"[a-fA-F0-9]+\"\\]\\)"; 3: non-ascii character in identifier
	     )
	    end t)
      (cond
       ((match-beginning 1)
	(put-text-property
	 (match-beginning 1) (match-end 1) 'syntax-table '(7 . ?'))
	(put-text-property
	 (match-beginning 2) (match-end 2) 'syntax-table '(7 . ?')))

       ((match-beginning 3)
	(put-text-property
	 (match-beginning 3) (match-end 3) 'syntax-table '(2 . nil)))
       )
      )))

;;; setup
(add-to-list 'ada-prj-default-list 'ada-gnat-default-prj)

(provide 'gnat-core)

;; end of file
