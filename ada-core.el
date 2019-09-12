;;; ada-core.el --- core facilities for ada-mode -*- lexical-binding:t -*-

;; Copyright (C) 1994, 1995, 1997 - 2017, 2019  Free Software Foundation, Inc.
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

(require 'compile)
(require 'find-file)
(require 'uniquify-files)
(require 'wisi)
(require 'wisi-prj)

;;;; misc

(defgroup ada nil
  "Major mode for editing Ada source code in Emacs."
  :group 'languages)

(defconst ada-operator-re
  "\\+\\|-\\|/\\|\\*\\*\\|\\*\\|=\\|&\\|\\_<\\(abs\\|mod\\|rem\\|and\\|not\\|or\\|xor\\)\\_>\\|<=\\|<\\|>=\\|>"
  "Regexp matching Ada operator_symbol.")

(defconst ada-name-regexp
  "\\(\\(?:\\sw\\|[_.]\\)+\\)")

(defvar ada-compiler 'gnat
  "Default Ada compiler; can be overridden in project files.
Values defined by compiler packages.")

(defvar ada-mode-hook nil
  "List of functions to call when Ada mode is invoked.
This hook is executed after `ada-mode' is fully loaded, but
before file local variables are processed.")

(defvar ada-syntax-propertize-hook nil
  "Hook run from `ada-syntax-propertize'.
Called by `syntax-propertize', which is called by font-lock in
`after-change-functions'.")

(defun ada-in-based-numeric-literal-p ()
  "Return t if point is after a prefix of a based numeric literal."
  (looking-back "\\([0-9]+#[0-9a-fA-F_]+\\)" (line-beginning-position)))

(defun ada-declarative-region-start-p (cache)
  "Return t if cache is a keyword starting a declarative region."
  (memq (wisi-cache-token cache) '(DECLARE IS PRIVATE))
  ;; IS has a cache only if start of declarative region
  )

(defun ada-goto-declarative-region-start ()
  "Goto start of declarative region containing point."
  (interactive)
  (wisi-validate-cache (point-min) (point-max) t 'navigate)

  (let ((done nil)
	start-pos
	(in-package-spec nil)
	(cache (or (wisi-get-cache (point))
		   (wisi-backward-cache))))

    ;; We use backward-cache, not forward-cache, to handle the case
    ;; where point is in the whitespace or comment before a block; we
    ;; want the containing block, not the next block.

    (when cache ;; nil at bob
      ;; If this is called with point in a comment after 'is', then the
      ;; declarative region starts after the comment; don't hang in a
      ;; package spec.
      (setq start-pos (point))
      (while (not done)
	(if (and (or (not in-package-spec)
		     (< (point) start-pos))
		 (ada-declarative-region-start-p cache))
	    (progn
	      (forward-word);; past 'is'
	      (setq done t))
	  (cl-case (wisi-cache-class cache)
	    (motion
	     (setq cache (wisi-goto-containing cache)));; statement-start

	    (statement-end
	     (setq cache (wisi-goto-containing cache)) ;; statement-start
	     (cl-case (wisi-cache-nonterm cache)
	       ((generic_package_declaration
		 package_declaration
		 entry_body package_body package_declaration protected_body subprogram_body task_body
		 protected_type_declaration single_protected_declaration single_task_declaration task_type_declaration)
		;; This is a block scope before the starting point; we want the containing scope
		(setq cache (wisi-goto-containing cache)))

	       (t
		nil)
	       ))

	    (statement-start
	     (cl-case (wisi-cache-nonterm cache)
	       (generic_package_declaration
		(setq in-package-spec t)
		(setq cache (wisi-next-statement-cache cache)) ;; 'package'
		(setq cache (wisi-next-statement-cache cache))) ;; 'is'

	       (package_declaration
		(setq in-package-spec t)
		(setq cache (wisi-next-statement-cache cache))) ;; 'is'

	       ((entry_body package_body package_declaration protected_body subprogram_body task_body)
		(while (not (eq 'IS (wisi-cache-token cache)))
		  (setq cache (wisi-next-statement-cache cache))))

	       ((protected_type_declaration single_protected_declaration single_task_declaration task_type_declaration)
		(while (not (eq 'IS (wisi-cache-token cache)))
		  (setq cache (wisi-next-statement-cache cache)))
		(when (looking-at "\<new\>")
		  (while (not (eq 'WITH (wisi-cache-token cache)))
		    (setq cache (wisi-next-statement-cache cache)))))

	       (t
		(setq cache (wisi-goto-containing cache t)))
	       ))

	    (t
	     (setq cache (wisi-goto-containing cache t)))
	    )))

      ;; point is at start of first code statement after
      ;; declaration-start keyword and comment; move back to end of
      ;; keyword.
      (while (forward-comment -1))
      )))

;;;; additional ada-compiler generic interfaces

(cl-defgeneric ada-compiler-file-name-from-ada-name (compiler project ada-name)
  "Return the filename that would contain the library level ADA-NAME.")

(defun ada-file-name-from-ada-name (ada-name)
  "Return the filename in which ADA-NAME is found."
  (let ((project (ada-prj-require-prj)))
    (ada-compiler-file-name-from-ada-name (ada-prj-compiler project) project ada-name)))

(cl-defgeneric ada-compiler-ada-name-from-file-name (compiler project file-name)
  "Return the Ada library unit name that should be found in FILE-NAME.")

(defun ada-ada-name-from-file-name (file-name)
  "Return the ada-name that should be found in FILE-NAME."
  (let ((project (ada-prj-require-prj)))
    (ada-compiler-ada-name-from-file-name (ada-prj-compiler project) file-name)))

(cl-defgeneric ada-compiler-make-package-body (compiler project body-file-name)
  "Create a package body skeleton from a package spec.
BODY-FILE-NAME is the file name of the body file. Current buffer
is the package spec.")

(defun ada-make-package-body (body-file-name)
  (let ((prj (ada-prj-require-prj)))
    (ada-compiler-make-package-body (ada-prj-compiler prj)
				    prj
				    (expand-file-name body-file-name))))

;;;; refactor

;; Refactor actions; must match wisi-ada.adb Refactor
(defconst ada-refactor-method-object-to-object-method 1)
(defconst ada-refactor-object-method-to-method-object 2)

(defconst ada-refactor-element-object-to-object-index 3)
(defconst ada-refactor-object-index-to-element-object 4)

(defconst ada-refactor-format-paramlist 5)

(defun ada-refactor (action)
  (wisi-validate-cache (line-end-position -7) (line-end-position 7) t 'navigate)
  (save-excursion
    (skip-syntax-backward "w_.\"")
    (let* ((edit-begin (point))
	   (cache (wisi-goto-statement-start))
	   (parse-begin (point))
	   (parse-end (wisi-cache-end cache)))
      (if parse-end
	  (setq parse-end (+ parse-end (wisi-cache-last (wisi-get-cache (wisi-cache-end cache)))))
	;; else there is a syntax error; missing end of statement
	(setq parse-end (point-max)))
      (wisi-refactor wisi--parser action parse-begin parse-end edit-begin)
      )))

(defun ada-refactor-1 ()
  "Refactor Method (Object) => Object.Method.
Point must be in Method."
  (interactive)
  (ada-refactor ada-refactor-method-object-to-object-method))

(defun ada-refactor-2 ()
  "Refactor Object.Method => Method (Object).
Point must be in Object.Method."
  (interactive)
  (ada-refactor ada-refactor-object-method-to-method-object))

(defun ada-refactor-3 ()
  "Refactor Element (Object, Index) => Object (Index).
Point must be in Element"
  (interactive)
  (ada-refactor ada-refactor-element-object-to-object-index))

(defun ada-refactor-4 ()
  "Refactor Object (Index) => Element (Object, Index).
Point must be in Object"
  (interactive)
  (ada-refactor ada-refactor-object-index-to-element-object))

;; refactor-5 in ada-format-paramlist below

(defcustom ada-language-version 'ada2012
  ;; ada-fix-error.el needs this.
  "Ada language version; one of `ada83', `ada95', `ada2005', `ada2012'.
Only affects the keywords to highlight, not which version the
parser accepts; the parser always accepts a superset of ada2012."
  :type '(choice (const ada83)
		 (const ada95)
		 (const ada2005)
		 (const ada2012))
  :safe  #'symbolp)
(make-variable-buffer-local 'ada-language-version)

(defun ada-in-case-expression ()
  "Return non-nil if point is in a case expression."
  (save-excursion
    ;; Used by ada-align; we know we are in a paren.
    (wisi-goto-open-paren 1)
    (while (forward-comment 1))
    (looking-at "case")))

(defun ada-align ()
  "If region is active, apply `align'. If not, attempt to align
current construct."
  (interactive)
  (if (use-region-p)
      (progn
        (align (region-beginning) (region-end))
        (deactivate-mark))

    ;; else see if we are in a construct we know how to align
    (let ((parse-result (syntax-ppss)))
      (cond
       ((ada-in-paramlist-p parse-result)
        (ada-format-paramlist))

       ((and
	 (wisi-in-paren-p parse-result)
	 (ada-in-case-expression))
	;; align '=>'
	(let ((begin (nth 1 parse-result))
	      (end   (scan-lists (point) 1 1)))
	  (align begin end 'entire)))

       (t
	(align-current))
       ))))

(defun ada-in-paramlist-p (parse-result)
  "Return t if point is inside the parameter-list of a subprogram declaration.
PARSE-RESULT must be the result of `syntax-ppss'."
  (wisi-validate-cache (point-min) (point-max) nil 'navigate)
  ;; (info "(elisp)Parser State" "*syntax-ppss*")
  (let (cache)
    (and (> (nth 0 parse-result) 0)
	 ;; cache is nil if the parse failed
	 (setq cache (wisi-get-cache (nth 1 parse-result)))
	 (eq 'formal_part (wisi-cache-nonterm cache)))
    ))

(defun ada-format-paramlist ()
  "Reformat the parameter list point is in."
  (interactive)
  (condition-case nil
      (wisi-goto-open-paren)
    (error
     (user-error "Not in parameter list")))
  (funcall indent-line-function); so new list is indented properly
  (when (not (looking-back "^[ \t]*" (line-beginning-position)))
    (delete-horizontal-space)
    (insert " "))
  (ada-refactor ada-refactor-format-paramlist))

;;;; xref

(defvar ada-xref-tool (if (locate-file "gpr_query" exec-path '("" ".exe")) 'gpr_query 'gnat)
  "Default Ada cross reference tool; can be overridden in project files.")

(defcustom ada-xref-full-path nil
  "If t, cross-references show the full path to source files; if
nil, only the file name."
  :group 'ada
  :type 'boolean
  :safe #'booleanp)

(defun ada-make-subprogram-body ()
  "Convert subprogram specification after point into a subprogram body stub."
  (interactive)
  (wisi-goto-statement-start)
  ;; point is at start of subprogram specification;
  ;; ada-wisi-expand-region will find the terminal semicolon.
  (wisi-validate-cache (point-min) (point-max) t 'navigate)

  (let* ((begin (point))
	 (end (wisi-cache-end (wisi-get-cache (point))))
	 (name (wisi-next-name)))
    (goto-char end)
    (newline)
    (insert " is begin\n\nend ");; legal syntax; parse does not fail
    (insert name)
    (forward-char 1)

    ;; newline after body to separate from next body
    (newline-and-indent)
    (indent-region begin (point))
    (forward-line -2)
    (back-to-indentation)
    ))

(defun ada-ff-create-body ()
  ;; ff-find-other-file calls this with point in an empty buffer for
  ;; the body file; ada-make-package-body expects to be in the
  ;; spec. So go back to the spec, and delete the body buffer so it
  ;; does not get written to disk.
  (let ((body-buffer (current-buffer))
	(body-file-name (buffer-file-name)))

    (set-buffer-modified-p nil);; may have a skeleton; allow silent delete

    (ff-find-the-other-file);; back to spec

    (kill-buffer body-buffer)

    (ada-compiler-make-package-body (ada-prj-compiler (ada-prj-require-prj)) body-file-name)

    ;; back to the new body file, read in from the disk.
    (ff-find-the-other-file)
    (revert-buffer t t))
  )

;;;; project files

(cl-defstruct
    (ada-prj
     (:include wisi-prj)
     (:copier nil)
     (:constructor nil)
     (:constructor make-ada-prj
		   (&key
		    name
		    compile-env
		    (compiler-label ada-compiler)
		    (xref-label ada-xref-tool)
		    source-path
		    plist
		    file-pred
		    &aux
		    (compiler (ada-prj-make-compiler compiler-label))
		    (xref (ada-prj-make-xref xref-label))
		    )))
  compiler-label
  xref-label
  ;; save labels for wisi-prj-default

  plist    ;; user-declared project variables; also obj_dir, mostly as an example.
  )

;;;###autoload
(cl-defun create-ada-prj
    (&key
     name
     compile-env
     (compiler-label ada-compiler)
     (xref-label ada-xref-tool)
     source-path
     plist
     file-pred)
  ;; We declare and autoload this because we can't autoload
  ;; make-ada-prj in emacs < 27. We can't use '(defalias
  ;; 'create-ada-prj 'make-ada-prj); then make-ada-prj is not defined
  ;; by autoload.
  (make-ada-prj
   :name name
   :compile-env compile-env
   :compiler-label compiler-label
   :xref-label xref-label
   :source-path source-path
   :plist plist
   :file-pred file-pred))

(defvar ada-prj-default-list nil
  ;; This is used by ada-build.el; we keep it to allow other similar
  ;; uses.
  "List of functions to add default project variables. Called
with one argument; the project. `default-directory' is set to the
directory containing the project file. Function should update the
project.")

;;;###autoload
(defun ada-prj-default (&optional name src-dir)
  "Return the default `ada-prj' object.
If SRC-DIR is non-nil, use it as the default for project.source-path."
  (let ((project
	 (make-ada-prj
	  :name (or name "_default_")
	  :compiler-label  ada-compiler
	  :xref-label      ada-xref-tool
	  :source-path	  (cond
			   ((null src-dir) nil)
			   ((listp src-dir) src-dir)
			   (t (list src-dir)))
	  )))

    (cl-dolist (func ada-prj-default-list)
      (funcall func project))

    project))

(cl-defmethod wisi-prj-default ((prj ada-prj))
  (let ((project
	 (make-ada-prj
	  :name           (wisi-prj-name prj)
	  :compile-env    (wisi-prj-compile-env prj)
	  :compiler-label (ada-prj-compiler-label prj)
	  :xref-label     (ada-prj-xref-label prj)
	  )))

    (cl-dolist (func ada-prj-default-list)
      (funcall func project))

    project))

;;;###autoload
(defun ada-prj-make-compiler (label)
  ;; We use the autoloaded constructor here
  (require (intern (format "ada-compiler-%s" (symbol-name label))))
  (funcall (intern (format "create-%s-compiler" (symbol-name label)))))

(defun ada-prj-make-xref (label)
  ;; We use the autoloaded constructor here
  (funcall (intern (format "create-%s-xref" (symbol-name label))))
  ;; So far the only ada xref we have is gpr_query, which uses a
  ;; gnat-compiler object, so no new require here.
  )

(defun ada-prj-require-prj ()
  "Return current `ada-prj' object.
Throw an error if current project is not an ada-prj."
  (let ((prj (project-current)))
    (if (ada-prj-p prj)
	prj
      (error "current project is not an ada project."))))

(cl-defmethod wisi-prj-parse-one :after (project name value)
  (cond
   ;; variable name alphabetical order
   ((string= name "ada_compiler")
    (let ((comp (intern value)))
      (setf (ada-prj-compiler project) (ada-prj-make-compiler comp))))

   ((string= name "obj_dir")
    (let ((obj-dir (plist-get (ada-prj-plist project) 'obj_dir)))
      (cl-pushnew (file-name-as-directory (expand-file-name value))
		  obj-dir :test #'equal)
      (setf (ada-prj-plist project) (plist-put (ada-prj-plist project) 'obj_dir obj-dir))
      ))

   ;; FIXME:   ((string= name "xref_tool")

   (t
    ;; Any other field in the file is set as a project file variable.
    ;; eg "comp_opt"
    (setf (ada-prj-plist project) (plist-put (ada-prj-plist project)
					     (intern name) value)))
   ))

;; This is autoloaded because it is often used in Makefiles, and thus
;; will be the first ada-mode function executed.
;;;###autoload
(defun ada-select-prj-file (prj-file)
  ;; not ada-prj-select-file for backward compatibility
  "Select PRJ-FILE as the current project file, parsing it if necessary.
Deselects the current project first."
  (wisi-prj-select-cached prj-file (ada-prj-default "")))
(make-obsolete 'ada-select-prj-file 'wisi-prj-select-cached "ada-mode 7.0")

(cl-defgeneric ada-prj-select-compiler (compiler project)
  "PROJECT has been selected; set any project options that are both Ada and compiler specific.")

(cl-defgeneric ada-prj-deselect-compiler (compiler project)
  "PROJECT has been deselected; unset any project options that are both Ada and compiler specific.")

(cl-defmethod wisi-prj-select :after ((project ada-prj))
  (ada-prj-select-compiler (ada-prj-compiler project) project))

(cl-defmethod wisi-prj-deselect :before ((project ada-prj))
  (ada-prj-deselect-compiler (ada-prj-compiler project) project))

(cl-defmethod wisi-prj-identifier-at-point ((_project ada-prj))
  "Return the identifier around point, move point to start of
identifier.  May be an Ada identifier or operator."
  (when (wisi-in-comment-p)
    ;; We don't abort on string; operator function names are strings.
    (error "Inside comment"))

  ;; Handle adjacent operator/identifer like:
  ;; test/ada_mode-slices.adb
  ;;   D1, D2 : Day := +Sun;

  ;; FIXME: use skip-syntax, to handle non-ascii

  ;; Move to the beginning of the identifier or operator
  (if (looking-at "[a-zA-Z0-9_]")
      ;; In an identifier
      (skip-chars-backward "a-zA-Z0-9_")
    ;; In an operator
    (skip-chars-backward "+\\-\\*/&<>="))

  ;; Just in front of, or inside, a string => we could have an
  ;; operator function declaration.
  (cond
   ((wisi-in-string-p)
    (cond

     ((and (= (char-before) ?\")
	   (progn
	     (forward-char -1)
	     (looking-at (concat "\"\\(" ada-operator-re "\\)\""))))
      (concat "\"" (match-string-no-properties 1) "\""))

     (t
      (error "Inside string or character constant"))
     ))

   ((and (= (char-after) ?\")
	 (looking-at (concat "\"\\(" ada-operator-re "\\)\"")))
    (concat "\"" (match-string-no-properties 1) "\""))

   ((looking-at ada-operator-re)
    ;; Return quoted operator, as this is what the back end expects.
    (concat "\"" (match-string-no-properties 0) "\""))

   ((looking-at "[a-zA-Z0-9_]+")
    (match-string-no-properties 0))

   (t
    (error "No identifier around"))
   ))

;;;; initialization
(push (cons "adp" #'wisi-prj-parse-file-1) wisi-prj-parser-alist)

(add-to-list 'wisi-prj-file-extensions "adp")

(provide 'ada-core)
;; ada-core.el ends here
