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

;;;;; code

(defcustom ada-gnat-debug-run nil
  "If t, compilation buffers containing a GNAT command will show
the command.  Otherwise, they will show only the output of the
command.  This applies e.g. to *gnatfind* buffers."
  :type 'boolean
  :safe  #'booleanp
  :group 'ada)

;;;; project file handling

(cl-defstruct
    (gnat-compiler
     (:constructor nil)
     (:constructor make-gnat-compiler
		   (&aux
		    (environment (cl-copy-list process-environment))
		    ))
     )
  "Used with ada-compiler-* generic functions."

  gpr-file 	  ;; absolute file name of GNAT project file.
  run-buffer-name ;; string; some compiler objects have no gpr file
  project-path    ;; list of directories for GPR_PROJECT_PATH
  environment 	  ;; copy of process-environment, with project file env vars added.
  target 	  ;; gnat --target argument. FIXME: add to -parse
  runtime 	  ;; gnat --RTS argument. FIXME: add to -parse
  gnat-stub-opts  ;; FIXME: add to -parse
  gnat-stub-cargs ;; FIXME: add to -parse
  )

(defun gnat-compiler-require-prj ()
  "Return current `gnat-compiler' object from current project compiler.
Throw an error if current project does not have a gnat-compiler."
  (let* ((ada-prj (ada-prj-require-prj))
	 (compiler (ada-prj-compiler ada-prj)))
    (if (gnat-compiler-p compiler)
	compiler
      (error "no gnat-compiler in selected project compiler."))))

(defun gnat-prj-add-prj-dir (compiler dir)
  "Add DIR to compiler.project_path, and to GPR_PROJECT_PATH in compiler.environment."
  (cl-pushnew dir (gnat-compiler-project-path compiler) :test #'string-equal)

  (let ((process-environment (cl-copy-list (gnat-compiler-environment compiler))))
    (setenv "GPR_PROJECT_PATH"
	    (mapconcat 'identity
		       (gnat-compiler-project-path compiler) path-separator))
    (setf (gnat-compiler-environment compiler) (cl-copy-list process-environment)))
    )

(defun gnat-prj-show-prj-path ()
  "For `ada-prj-show-prj-path'."
  (interactive)
  ;; FIXME: broken
  (if (plist-get 'prj_dir (ada-prj-plist (project-current)))
      (progn
	(pop-to-buffer (get-buffer-create "*GNAT project file search path*"))
	(erase-buffer)
	(dolist (file (plist-get (ada-prj-plist (project-current)) 'prj_dir))
	  (insert (format "%s\n" file))))
    (message "no project file search path set")
    ))

(defun gnat-get-paths-1 (compiler src-dirs obj-dirs prj-dirs)
  "Append list of source, project and object dirs in current gpr project to SRC-DIRS,
OBJ-DIRS and PRJ-DIRS. Uses `gnat list'.  Returns new (SRC-DIRS OBJ-DIRS PRJ-DIRS)."
  (with-current-buffer (gnat-run-buffer compiler)
    ;; gnat list -v -P can return status 0 or 4; always lists compiler dirs
    ;;
    ;; WORKAROUND: GNAT 7.2.1 gnatls does not support C++ fully; it
    ;; does not return src_dirs from C++ projects (see AdaCore ticket
    ;; M724-045). The workaround is to include the src_dirs in an
    ;; Emacs Ada mode project.
    (gnat-run-gnat compiler "list" (list "-v") '(0 4))

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
	     :test #'string-equal)
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
	     :test #'string-equal)
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
                (cl-pushnew (directory-file-name default-directory) prj-dirs :test #'string-equal)
              (let ((f (expand-file-name
                        (buffer-substring-no-properties (point) (point-at-eol)))))
                (cl-pushnew f prj-dirs :test #'string-equal)
                (cl-pushnew f src-dirs :test #'string-equal)))
	    (forward-line 1))

	  )
      (error
       (pop-to-buffer (current-buffer))
       ;; search-forward failed
       (error "parse gpr failed")
       ))
    (list (cl-remove-duplicates src-dirs) (cl-remove-duplicates obj-dirs) (cl-remove-duplicates prj-dirs))))

(defun gnat-get-paths (project)
  "Add project and/or compiler source, object, project paths to PROJECT src_dir, obj_dir and/or prj_dir."
  (let* ((compiler (ada-prj-compiler project))
	 (src-dirs (plist-get (ada-prj-plist project) 'src_dir))
         (obj-dirs (plist-get (ada-prj-plist project) 'obj_dir))
	 (prj-dirs (cl-copy-list (gnat-compiler-project-path compiler)))
	 (res (gnat-get-paths-1 compiler src-dirs obj-dirs prj-dirs)))

    (setq src-dirs (pop res))
    (setq obj-dirs (pop res))
    (setq prj-dirs (pop res))

    ;; FIXME: we used to call gpr_query to get src_dirs, prj_dirs
    ;; here, but that seems unnecessary. However, gprls (aka gnatls)
    ;; fails if any files are missing string quotes (but not for other
    ;; syntax errors).

    (setf (ada-prj-plist project) (plist-put (ada-prj-plist project) 'src_dir (reverse src-dirs)))
    (setf (ada-prj-plist project) (plist-put (ada-prj-plist project) 'obj_dir (reverse obj-dirs)))
    (mapc (lambda (dir) (gnat-prj-add-prj-dir compiler dir))
	  (reverse prj-dirs))
    ))

(defun gnat-parse-gpr (gpr-file project)
  "Append to src_dir and prj_dir in PROJECT (an `ada-prj' object) by parsing GPR-FILE.
Return new value of PROJECT.
GPR-FILE must be absolute file name.
src_dir will include compiler runtime."
  ;; this can take a long time; let the user know what's up
  (message "Parsing %s ..." gpr-file)

  (let ((compiler (ada-prj-compiler project)))
    (if (gnat-compiler-gpr-file compiler)
	;; gpr-file previously set; new one must match
	(when (not (string-equal gpr-file (gnat-compiler-gpr-file compiler)))
	  (error "project file %s defines a different GNAT project file than %s"
		 (gnat-compiler-gpr-file compiler)
		 gpr-file))


      (setf (gnat-compiler-gpr-file compiler) gpr-file)
      ))

  (condition-case-unless-debug nil
      ;; Can fail due to gpr_query not installed (FIXME: say what?),
      ;; or bad gpr file syntax; allow .prj file settings to still
      ;; work.
      (progn
	(gnat-get-paths project)
	(message "Parsing %s ... done" gpr-file))
    (error
       (message "Parsing %s ... error" gpr-file))
    ))

(defun gnat-parse-gpr-1 (gpr-file project)
  "For `wisi-prj-parser-alist'."
  (setf (gnat-compiler-run-buffer-name (ada-prj-compiler project)) gpr-file)
  (gnat-parse-gpr gpr-file project))

;;;; command line tool interface

(defun gnat-run-buffer-name (prj-file-name &optional prefix)
  ;; We don't use (gnat-compiler-gpr-file compiler), because multiple
  ;; ada-prj files can use one gpr-file.
  (concat (or prefix " *gnat-run-")
	  prj-file-name
	  "*"))

(defun gnat-run-buffer (compiler)
  "Return a buffer suitable for running gnat command line tools for COMPILER"
  (let* ((name (gnat-compiler-run-buffer-name compiler))
	 (buffer (get-buffer name)))

    (unless (buffer-live-p buffer)
      (setq buffer (get-buffer-create name))
      (when (gnat-compiler-gpr-file compiler)
	;; Otherwise assume `default-directory' is already correct (or
	;; doesn't matter).
	(with-current-buffer buffer
	  (setq default-directory
		(file-name-directory
		 (gnat-compiler-gpr-file compiler))))
	))
    buffer))

(defun gnat-run (compiler exec command &optional err-msg expected-status)
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

  (let ((process-environment (cl-copy-list (gnat-compiler-environment compiler)))
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

(defun gnat-run-gnat (compiler command &optional switches-args expected-status)
  "Run the \"gnat\" command line tool, as \"gnat COMMAND -P<prj> SWITCHES-ARGS\".
COMMAND must be a string, SWITCHES-ARGS a list of strings.
EXPECTED-STATUS must be nil or a list of integers.
Return process status.
Assumes current buffer is (gnat-run-buffer)"
  (let* ((gpr-file (gnat-compiler-gpr-file compiler))
	 (project-file-switch
	  (when gpr-file
	    (concat "-P" (file-name-nondirectory gpr-file))))
         (target-gnat (concat (gnat-compiler-target compiler) "gnat"))
         ;; gnat list understands --RTS without a fully qualified
         ;; path, gnat find (in particular) doesn't (but it doesn't
         ;; need to, it uses the ALI files found via the GPR)
         (runtime
          (when (and (gnat-compiler-runtime compiler) (string= command "list"))
            (list (concat "--RTS=" (gnat-compiler-runtime compiler)))))
	 (cmd (append (list command) (list project-file-switch) runtime switches-args)))

    (gnat-run compiler target-gnat cmd nil expected-status)
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

(cl-defmethod ada-compiler-file-name-from-ada-name ((compiler gnat-compiler) ada-name)
  (let ((result nil))

    (while (string-match "\\." ada-name)
      (setq ada-name (replace-match "-" t t ada-name)))

    (setq ada-name (downcase ada-name))

    (with-current-buffer (gnat-run-buffer compiler)
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

(cl-defmethod ada-compiler-ada-name-from-file-name ((_compiler gnat-compiler) file-name)
  (let* ((ada-name (file-name-sans-extension (file-name-nondirectory file-name)))
	 (predefined (cdr (assoc ada-name ada-gnat-predefined-package-alist))))

    (if predefined
        predefined
      (while (string-match "-" ada-name)
	(setq ada-name (replace-match "." t t ada-name)))
      ada-name)))

(cl-defmethod ada-compiler-make-package-body ((compiler gnat-compiler) body-file-name)
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
	(opts (when (gnat-compiler-gnat-stub-opts compiler)
		(split-string (gnat-compiler-gnat-stub-opts compiler))))
	(cargs (when (gnat-compiler-gnat-stub-cargs compiler)
		(split-string (gnat-compiler-gnat-stub-cargs compiler))))
	(process-environment (cl-copy-list (gnat-compiler-environment compiler))) ;; for GPR_PROJECT_PATH
	)

    ;; Make sure all relevant files are saved to disk.
    (save-some-buffers t)
    (with-current-buffer (gnat-run-buffer compiler)
      (gnat-run-no-prj
       (append (list "stub") opts (list start-file "-cargs") cargs)
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

;;;; Initialization

;; These are shared between ada-compiler-gnat and gpr-query.
(add-to-list 'wisi-prj-file-extensions  "gpr")
(add-to-list 'wisi-prj-default-alist '("gpr" . ada-prj-default))
(add-to-list 'wisi-prj-parser-alist  '("gpr" . gnat-parse-gpr-1))

(add-to-list
 'compilation-error-regexp-alist-alist
 '(gnat
   ;; typical:
   ;;   cards_package.adb:45:32: expected private type "System.Address"
   ;;
   ;; with full path Source_Reference pragma :
   ;;   d:/maphds/version_x/1773/sbs-abi-dll_lib.ads.gp:39:06: file "interfaces_c.ads" not found
   ;;
   ;; gnu cc1: (gnatmake can invoke the C compiler)
   ;;   foo.c:2: `TRUE' undeclared here (not in a function)
   ;;   foo.c:2 : `TRUE' undeclared here (not in a function)
   ;;
   ;; we can't handle secondary errors here, because a regexp can't distinquish "message" from "filename"
   "^\\(\\(.:\\)?[^ :\n]+\\):\\([0-9]+\\)\\s-?:?\\([0-9]+\\)?" 1 3 4))

(provide 'gnat-core)
;; end of file
