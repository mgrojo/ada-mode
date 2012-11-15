;;; ada-mode.el --- major-mode for editing Ada sources
;;
;; FIXME (later): this is just the start of a major rewrite; just enough to
;; test the new smie-based ada-indent.el
;;
;; Also deleted all Xemacs support, and all pre-Emacs 24.2 support.
;;
;; So far, I've copied ada-mode 4.01, and deleted everything that has
;; to do with indentation or casing. imenu, outline, which-function,
;; add-log, format-paramlist, narrow-to-defun also deleted, since
;; they might be able to take advantage of the smie parser.
;;
;; skeleton, templates deleted because it might use semantic
;;
;; ada-xref deleted because it will become more orthogonal (it relies
;; on gnat xref output; it should be easy to use other compiler xref
;; output).
;;
;; Similary ada-prj assumes gnat.
;;
;; ada-prj-edit deleted because it won't be supported any more
;;
;; compile, build, debug deleted because I don't use them, and I'm
;; not sure if they are gnat-dependent.

;;; Copyright (C) 1994, 1995, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012  Free Software Foundation, Inc.

;; Author: Rolf Ebert      <ebert@inf.enst.fr>
;;      Markus Heritsch <Markus.Heritsch@studbox.uni-stuttgart.de>
;;      Emmanuel Briot  <briot@gnat.com>
;;      Stephen Leake <stephen_leake@member.fsf.org>
;; Maintainer: Stephen Leake <stephen_leake@member.fsf.org>
;; Keywords: languages ada

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

;;; Usage:
;; Emacs should enter Ada mode automatically when you load an Ada
;; file, based on the file extension.  The default extensions for Ada
;; files are .ads, .adb, .ada; use ada-add-extensions to add other
;; extensions.
;;
;; By default, ada-mode is configured to take full advantage of the
;; GNAT compiler (the menus will include the cross-referencing
;; features,...).  If you are using another compiler, you
;; should load that compiler's ada-* file first; that will define
;; ada-compiler as a feature, so ada-gnat.el will not be loaded.
;;
;; FIXME (later): see the user guide at ....

;;; History:
;; The first Ada mode for GNU Emacs was written by V. Broman in
;; 1985. He based his work on the already existing Modula-2 mode.
;; This was distributed as ada.el in versions of Emacs prior to 19.29.
;;
;; Lynn Slater wrote an extensive Ada mode in 1989. It consisted of
;; several files with support for dired commands and other nice
;; things.
;;
;; The probably very first Ada mode (called electric-ada.el) was
;; written by Steven D. Litvintchouk and Steven M. Rosen for the
;; Gosling Emacs. L. Slater based his development on ada.el and
;; electric-ada.el.
;;
;; A complete rewrite by M. Heritsch and R. Ebert was done at some
;; point.  Some ideas from the Ada mode mailing list have been added.
;; Some of the functionality of L. Slater's mode has not (yet) been
;; recoded in this new mode.
;;
;; A complete rewrite for Emacs-20 / GNAT-3.11 was done by Ada Core
;; Technologies.
;;
;; A complete rewrite, to restructure the code more orthogonally, and
;; to use smie for the indentation engine, was done in 2012 by Stephen
;; Leake.

;;; Credits:
;;   Many thanks to John McCabe <john@assen.demon.co.uk> for sending so
;;     many patches included in this package.
;;   Christian Egli <Christian.Egli@hcsd.hac.com>:
;;     ada-imenu-generic-expression
;;   Many thanks also to the following persons that have contributed
;;   to the ada-mode
;;     Philippe Waroquiers (PW) <philippe@cfmu.eurocontrol.be> in particular,
;;     woodruff@stc.llnl.gov (John Woodruff)
;;     jj@ddci.dk (Jesper Joergensen)
;;     gse@ocsystems.com (Scott Evans)
;;     comar@gnat.com (Cyrille Comar)
;;     stephen_leake@stephe-leake.org (Stephen Leake)
;;     robin-reply@reagans.org
;;    and others for their valuable hints.

(require 'find-file nil t)
(require 'align nil t)
(require 'which-func nil t)
(require 'compile nil t)

(defun ada-mode-version ()
  "Return Ada mode version."
  (interactive)
  (let ((version-string "5.00"))
    (if (called-interactively-p 'interactive)
	(message version-string)
      version-string)))

(defvar ada-mode-hook nil
  "*List of functions to call when Ada mode is invoked.
This hook is executed after `ada-mode' is fully loaded.  This is
a good place to add Ada environment specific bindings.")

(defgroup ada nil
  "Major mode for editing and compiling Ada source in Emacs."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'languages)

(defcustom ada-language-version 'ada2012
  "*Ada language version; one of `ada83', `ada95', `ada2005'.
Only affects the keywords to highlight."
  :type '(choice (const ada83) (const ada95) (const ada2005) (const ada2012)) :group 'ada)

(defcustom ada-popup-key '[down-mouse-3]
  ;; FIXME (later, when testing menu): don't need a var for this; user can just bind a key
  "*Key used for binding the contextual menu.
If nil, no contextual menu is available."
  :type '(restricted-sexp :match-alternatives (stringp vectorp))
  :group 'ada)

(defcustom ada-search-directories
  (append '(".")
	  (split-string (or (getenv "ADA_INCLUDE_PATH") "") ":"))
  "*Default list of directories to search for Ada files.
See the description for the `ff-search-directories' variable.
This variable is the initial value of
`ada-search-directories-internal'.  It is usually augmented
internally by a project file and/or compiler search paths."
  :type '(repeat (choice :tag "Directory"
			 (const :tag "default" nil)
			 (directory :format "%v")))
  :group 'ada)

;;; ---- end of user configurable variables; see other ada-*.el files for more

(defvar ada-search-directories-internal ada-search-directories
  "Internal version of `ada-search-directories'.
Its value is the concatenation of the search path as read in a
project file, any compiler search path, and the value of the
user-defined `ada-search-directories'.")

;;; keymap and menus

(defvar ada-mode-map
  (let ((map (make-sparse-keymap)))
    ;; C-c <letter> are reserved for users

    (define-key map "\C-c\C-c" 'compile)
    (define-key map "\C-c\C-d" 'ada-goto-declaration)
    (define-key map "\C-c\C-n" 'ada-make-subprogram-body)
    (define-key map "\C-c\C-o" 'ada-find-other-file)
    (define-key map "\C-c\M-o" 'ada-find-other-file-noset)
    map
  )  "Local keymap used for Ada mode.")

(defvar ada-mode-menu (make-sparse-keymap "Ada"))
(easy-menu-define ada-mode-menu ada-mode-map "Menu keymap for Ada mode"
  '("Ada"
    ("Help"
     ["Ada Mode"             (info "ada-mode") t]
     ["Ada Reference Manual" (info "arm2012") t]
     ["Key bindings"         describe-bindings t]
     )

    ["Customize"     (customize-group 'ada)]
    ["------"        nil nil]
    ["Next compilation error"  next-error             t]
    ["Other File"              ada-find-other-file  t]
    ["Other File don't find decl" ada-find-other-file-noset    t]
    ["Goto Declaration/Body"   ada-goto-declaration t]; FIXME: appropriate enable
    ("Edit"
     ["Indent Line"                 indent-for-tab-command  t]
     ["Indent Lines in Selection"   indent-region           t]
     ["Indent Lines in File"        (indent-region (point-min) (point-max))  t]
     ["Align"                       align               t]
     ;; FIXME: does 'align' do:
     ;; 1. ':=', '=>' in code sections
     ;; 2. parameter lists
     ;; 3. comments after code
     ;; 4. 'use' in context clauses
     ;; 5. 'at' in record rep clauses
     ["Comment Selection"           comment-region               t]
     ["Uncomment Selection"         (lambda () (comment-region t)) t]
     ["Fill Comment Paragraph"      fill-paragraph               t]
     ["Fill Comment Paragraph Justify"
      ada-fill-comment-paragraph-justify                         t]; FIXME: test
     ["Fill Comment Paragraph Postfix"
      ada-fill-comment-paragraph-postfix                         t]; FIXME: test
     ["---"                         nil                          nil]
     ["Make body for subprogram"    ada-make-subprogram-body     t]; FIXME: test
     ["Narrow to subprogram"        narrow-to-defun          t]; FIXME: test
     )
    ))

(defvar ada-mode-abbrev-table nil
  "Local abbrev table for Ada mode.")

(defvar ada-align-modes
  '((ada-declaration-assign
     (regexp  . "[^:]\\(\\s-*\\):[^:]")
     (valid   . (lambda() (not (ada-in-comment-p))))
     (repeat . t)
     (modes   . '(ada-mode)))
    (ada-associate
     (regexp  . "[^=]\\(\\s-*\\)=>")
     (valid   . (lambda() (not (ada-in-comment-p))))
     (modes   . '(ada-mode)))
    (ada-comment
     (regexp  . "\\(\\s-*\\)--")
     (modes   . '(ada-mode)))
    (ada-use
     (regexp  . "\\(\\s-*\\)\\<use\\s-")
     (valid   . (lambda() (not (ada-in-comment-p))))
     (modes   . '(ada-mode)))
    (ada-at
     (regexp . "\\(\\s-+\\)at\\>")
     (modes . '(ada-mode))))
  "Rules to use to align different lines.")

(defconst ada-align-region-separate
  (eval-when-compile
    (concat
     "^\\s-*\\($\\|\\("
     "begin\\|"
     "declare\\|"
     "else\\|"
     "end\\|"
     "exception\\|"
     "for\\|"
     "function\\|"
     "generic\\|"
     "if\\|"
     "is\\|"
     "procedure\\|"
     "record\\|"
     "return\\|"
     "type\\|"
     "when"
     "\\)\\>\\)"))
  "See the variable `align-region-separate' for more information.")

;;; context menu

(defvar ada-context-menu-last-point nil)
(defvar ada-context-menu-on-identifier nil)
(defvar ada-context-menu nil)
(defun ada-after-keyword-p () nil);; FIXME: used in ada-popup-menu

(defun ada-call-from-context-menu (function)
  "Execute FUNCTION when called from the contextual menu.
It forces Emacs to change the cursor position."
  (interactive)
  (funcall function)
  (setq ada-context-menu-last-point
	(list (point) (current-buffer))))

(defun ada-popup-menu (position)
  "Pops up a contextual menu, depending on where the user clicked.
POSITION is the location the mouse was clicked on.
Sets `ada-context-menu-last-point' to the current position before
displaying the menu.  When a function from the menu is called,
point is where the mouse button was clicked."
  (interactive "e")

  ;; don't let context menu commands deactivate the mark (which would
  ;; hide the region in transient-mark-mode), even if they normally
  ;; would. FIXME (later, when testing menu): why is this a good idea?
  (let ((deactivate-mark nil))
    (setq ada-context-menu-last-point
	 (list (point) (current-buffer)))
    (mouse-set-point last-input-event)

    (setq ada-context-menu-on-identifier
	  (and (char-after)
	       (or (= (char-syntax (char-after)) ?w)
		   (= (char-after) ?_))
	       (not (ada-in-string-or-comment-p))
	       (save-excursion (skip-syntax-forward "w")
			       (not (ada-after-keyword-p)))
	       ))
    (popup-menu ada-context-menu)

    ;; FIXME (later, when testing menu): is this necessary? what do context menus do by default?
    ;; why not use save-excursion?
    (set-buffer (cadr ada-context-menu-last-point))
    (goto-char (car ada-context-menu-last-point))
    ))

;;; project files

;; An Emacs Ada mode project file can specify several things:
;;
;; - a compiler-specific project file
;;
;; - compiler-specific environment variables
;;
;; - other compiler-specific things (see the compiler support elisp code)
;;
;; - a list of source directories (in addition to those specified in the compiler project file)
;;
;; - a casing exception file
;;
;; All of the data used by Emacs Ada mode functions specified in a
;; project file is stored in a property list. The property list is
;; stored in an alist indexed by the project file name, so multiple
;; project files can be selected without re-parsing them (some
;; compiler project files can take a long time to parse).

(defvar ada-prj-alist nil
  "Alist holding currently parsed Emacs Ada project files. Indexed by absolute project file name.")

(defvar ada-current-prj-file nil
  "Current Emacs Ada project file.")

(defun ada-require-project-file ()
  (unless ada-current-prj-file
    ;; FIXME: provide search for defaults?
    (error "no Emacs Ada project file specified")))

(defvar ada-prj-default-function nil
  "Compiler-specific function to return default Emacs Ada project properties for the current buffer.
Called with one argument; the compiler-independent default
properties list. Function should add to the list and return it.")

(defun ada-prj-default ()
  "Return the default project properties list with the current buffer as main.
Calls `ada-prj-default-function' to extent the list with
compiler-specific objects."

  (let*
      ((file (buffer-file-name nil))
       (props
	(list
	 ;; variable name alphabetical order
	 'casing          (if (listp ada-case-exception-file)
			      ada-case-exception-file
			    (list ada-case-exception-file))
	 'main            (if file
			      (file-name-nondirectory
			       (file-name-sans-extension file))
			    "")
	 'run_cmd         "./${main}"
	 'src_dir         (list ".")
	 )))
    (if ada-prj-default-function
	(funcall ada-prj-default-function props)
      props)
    ))

(defvar ada-prj-parser-alist '(".adp" 'ada-parse-prj-file-1)
  "Alist of parsers for project files.
Default provides the minimal Ada mode parser; compiler support
code may add other parsers.  Parser is called with two arguments;
the project file name and the current project property
list. Parser must modify or add to the property list and return it.")

;;;###autoload
(defun ada-parse-prj-file (prj-file)
  "Read Emacs Ada or compiler-specific project file PRJ-FILE, set project properties in `ada-prj-alist'."
  ;; Not called ada-prj-parse-file for Ada mode 4.01 compatibility
  (let ((project (ada-prj-default))
	(parser (assoc (file-name-extension prj-file) ada-prj-parser-alist)))

    (setq prj-file (expand-file-name prj-file))

    (if parser
	(setq project (funcall parser prj-file project))
      (error "no project file parser defined for '%s'" prj-file))

    ;; Store the project properties
    (if (assoc prj-file ada-prj-alist)
	(setcdr (assoc prj-file ada-prj-alist) project)
      (add-to-list 'ada-prj-alist (cons prj-file project)))

    ;; (ada-xref-update-project-menu) FIXME: implement
    ))

(defun ada-parse-prj-file-1 (prj-file project)
  "Parse the Ada mode project file PRJ-FILE, set project properties in PROJECT.
Return new value of PROJECT."
  (let ((ada-buffer (current-buffer))
	;; fields that are lists or that otherwise require special processing
	casing run_cmd src_dir)

    ;; Give users a chance to use compiler-specific project file formats
    (let ((buffer (run-hook-with-args-until-success
		   'ada-load-project-hook prj-file)))
      (unless buffer
	;; we load the project file with no warnings; if it does not
	;; exist, we stay in the Ada buffer; no project variable
	;; settings will be found. That works for the default
	;; "default.adp", which does not exist as a file.
	(setq buffer (find-file-noselect prj-file nil)))
      (set-buffer buffer))

    (widen)
    (goto-char (point-min))

    ;; process each line
    (while (not (eobp))

      ;; ignore lines that don't have the format "name=value", put
      ;; 'name', 'value' in match-string.
      (if (looking-at "^\\([^=\n]+\\)=\\(.*\\)")
	  (cond
	   ;; FIXME: strip trailing spaces
	   ;; variable name alphabetical order
	   ((string= (match-string 1) "ada_project_path")
	    (add-to-list 'ada_project_path
			 (expand-file-name
			  (substitute-in-file-name (match-string 2)))))

	   ((string= (match-string 1) "build_dir")
	    (set 'project
		 (plist-put project 'build_dir
			    (file-name-as-directory (match-string 2)))))

	   ((string= (match-string 1) "casing")
	    (add-to-list 'casing
			 (expand-file-name (substitute-in-file-name (match-string 2)))))

	   ((string= (match-string 1) "check_cmd")
	    (add-to-list 'check_cmd (match-string 2)))

	   ((string= (match-string 1) "comp_cmd")
	    (add-to-list 'comp_cmd (match-string 2)))

	   ((string= (match-string 1) "debug_post_cmd")
	    (add-to-list 'debug_post_cmd (match-string 2)))

	   ((string= (match-string 1) "debug_pre_cmd")
	    (add-to-list 'debug_pre_cmd (match-string 2)))

	   ((string= (match-string 1) "gpr_file")
	    ;; expand now; path is relative to Emacs project file
	    (setq gpr_file (expand-file-name (match-string 2))))

	   ((string= (match-string 1) "make_cmd")
	    (add-to-list 'make_cmd (match-string 2)))

	   ((string= (match-string 1) "obj_dir")
	    (add-to-list 'obj_dir
			 (file-name-as-directory
			  (expand-file-name (match-string 2)))))

	   ((string= (match-string 1) "run_cmd")
	    (add-to-list 'run_cmd (match-string 2)))

	   ((string= (match-string 1) "src_dir")
	    (add-to-list 'src_dir
			 (file-name-as-directory
			  (expand-file-name (match-string 2)))))

	   (t
	    ;; any other field in the file is just copied
	    (set 'project (plist-put project
				     (intern (match-string 1))
				     (match-string 2))))))

      (forward-line 1))

    ;; done reading file

    ;; back to the user buffer
    (set-buffer ada-buffer)

    ;; process accumulated lists
    (if ada_project_path
	(let ((sep (plist-get project 'ada_project_path_sep)))
	  (setq ada_project_path (reverse ada_project_path))
	  (setq ada_project_path (mapconcat 'identity ada_project_path sep))
	  (set 'project (plist-put project 'ada_project_path ada_project_path))
	  ;; env var needed now for ada-gnat-parse-gpr
	  (setenv "ADA_PROJECT_PATH" ada_project_path)))

    (if debug_post_cmd (set 'project (plist-put project 'debug_post_cmd (reverse debug_post_cmd))))
    (if debug_pre_cmd (set 'project (plist-put project 'debug_pre_cmd (reverse debug_pre_cmd))))
    (if casing (set 'project (plist-put project 'casing (reverse casing))))
    (if check_cmd (set 'project (plist-put project 'check_cmd (reverse check_cmd))))
    (if comp_cmd (set 'project (plist-put project 'comp_cmd (reverse comp_cmd))))
    (if make_cmd (set 'project (plist-put project 'make_cmd (reverse make_cmd))))
    (if run_cmd (set 'project (plist-put project 'run_cmd (reverse run_cmd))))

    (if gpr_file
	(progn
	  (set 'project (ada-gnat-parse-gpr project gpr_file))
	  ;; append Ada source and object directories to others from Emacs project file
	  (setq src_dir (append (plist-get project 'src_dir) src_dir))
	  (setq obj_dir (append (plist-get project 'obj_dir) obj_dir))
	  (setq ada-xref-runtime-library-specs-path '()
		ada-xref-runtime-library-ali-path   '()))
      )

    ;; FIXME: gnatpath.exe doesn't output the runtime libraries, so always call ada-initialize-runtime-library
    ;; if using a gpr_file, the runtime library directories are
    ;; included in src_dir and obj_dir; otherwise they are in the
    ;; 'runtime-library' variables.
    ;; FIXME: always append to src_dir, obj_dir
    (ada-initialize-runtime-library (or (ada-xref-get-project-field 'cross_prefix) ""))
    ;;)

    (if obj_dir (set 'project (plist-put project 'obj_dir (reverse obj_dir))))
    (if src_dir (set 'project (plist-put project 'src_dir (reverse src_dir))))

    project
    ))

(defun ada-select-prj-file (file)
  "Select FILE as the current project file."
  (interactive)
  (setq ada-prj-default-project-file (expand-file-name file))

  (let ((casing (ada-xref-get-project-field 'casing)))
    (if casing
	(progn
	  ;; FIXME: use ada-get-absolute-dir here
	  (setq ada-case-exception-file casing)
	  (ada-case-read-exceptions))))

  (let ((ada_project_path (ada-xref-get-project-field 'ada_project_path)))
    (if ada_project_path
	;; FIXME: use ada-get-absolute-dir, mapconcat here
	(setenv "ADA_PROJECT_PATH" ada_project_path)))

  (setq compilation-search-path (ada-xref-get-src-dir-field))

  (setq ada-search-directories-internal
	;; FIXME: why do we need directory-file-name here?
	(append (mapcar 'directory-file-name compilation-search-path)
		ada-search-directories))

  ;; return 't', for decent display in message buffer when called interactively
  t)


;;; syntax properties

(defvar ada-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; make-syntax-table sets all alphanumeric to w, etc; so we only
    ;; have to add ada-specific things.

    ;; string brackets. `%' is the obsolete alternative string
    ;; bracket (arm J.2); if we make it syntax class ", it throws
    ;; font-lock and indentation off the track, so we use syntax class
    ;; $.
    (modify-syntax-entry ?%  "$" table)
    (modify-syntax-entry ?\" "\"" table)

    ;; punctuation; operators etc
    (modify-syntax-entry ?#  "$" table); based number
    (modify-syntax-entry ?&  "." table)
    (modify-syntax-entry ?*  "." table)
    (modify-syntax-entry ?+  "." table)
    (modify-syntax-entry ?-  ". 12" table); operator; see ada-syntax-propertize for double hyphen as comment
    (modify-syntax-entry ?. "." table)
    (modify-syntax-entry ?/  "." table)
    (modify-syntax-entry ?:  "." table)
    (modify-syntax-entry ?<  "." table)
    (modify-syntax-entry ?=  "." table)
    (modify-syntax-entry ?>  "." table)
    (modify-syntax-entry ?\' "." table); attribute; see ada-syntax-propertize for character literal
    (modify-syntax-entry ?\; "." table)
    (modify-syntax-entry ?\\ "." table); default is escape
    (modify-syntax-entry ?\|  "." table)

    ;; and \f and \n end a comment
    (modify-syntax-entry ?\f  ">   " table)
    (modify-syntax-entry ?\n  ">   " table)

    (modify-syntax-entry ?_ "_" table); symbol constituents, not word. FIXME: check GPS, poll users. doc why need 2

    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    table
    )
  "Syntax table to be used for editing Ada source code.")

(defvar ada-syntax-propertize-hook nil
  "Hook run from `ada-syntax-propertize'.")

(defun ada-syntax-propertize (start end)
  "Assign `syntax-table' properties in accessible part of buffer.
In particular, character constants are said to be strings, #...#
are treated as numbers instead of gnatprep comments."
  ;; (info "(elisp)Syntax Properties")
  (let ((modified (buffer-modified-p))
	(buffer-undo-list t)
	(inhibit-read-only t)
	(inhibit-point-motion-hooks t)
	(inhibit-modification-hooks t))
    (goto-char start)
    (while (re-search-forward
	    (concat
	     "[^a-zA-Z0-9)]\\('\\)[^'\n]\\('\\)"; 1, 2: character constants, not attributes
	     "\\|\\(--\\)"; 3: comment start
	     )
	    end t)
      ;; The help for syntax-propertize-extend-region-functions
      ;; implies that 'start end' will always include whole lines, in
      ;; which case we don't need
      ;; syntax-propertize-extend-region-functions
      (cond
       ((match-beginning 1)
	(put-text-property
	 (match-beginning 1) (match-end 1) 'syntax-table '(7 . ?'))
	(put-text-property
	 (match-beginning 2) (match-end 2) 'syntax-table '(7 . ?')))
       ((match-beginning 3)
	(put-text-property
	 (match-beginning 3) (match-end 3) 'syntax-table '(11 . nil)))
       ))
    (run-hook-with-args 'ada-syntax-propertize-hook start end)
    (unless modified
      (restore-buffer-modified-p nil))))

(defun ada-in-comment-p (&optional parse-result)
  "Return t if inside a comment.
If PARSE-RESULT is non-nil, use it instead of calling `syntax-ppss'."
  (nth 4 (or parse-result (syntax-ppss))))

(defun ada-in-string-p (&optional parse-result)
  "Return t if point is inside a string.
If PARSE-RESULT is non-nil, use it instead of calling `syntax-ppss'."
  (nth 3 (or parse-result (syntax-ppss))))

(defun ada-in-string-or-comment-p (&optional parse-result)
  "Return t if inside a comment or string.
If PARSE-RESULT is non-nil, use it instead of calling `syntax-ppss'."
  (setq parse-result (or parse-result (syntax-ppss)))
  (or (ada-in-string-p parse-result) (ada-in-comment-p parse-result)))

(defun ada-in-paren-p (&optional parse-result)
  "Return t if point is inside a pair of parentheses.
If PARSE-RESULT is non-nil, use it instead of calling `syntax-ppss'."
  (> (nth 0 (or parse-result (syntax-ppss))) 0))

;;; file navigation

(defvar ada-body-suffixes '(".adb")
  "List of possible suffixes for Ada body files.
The extensions should include a `.' if needed.")

(defvar ada-spec-suffixes '(".ads")
  "List of possible suffixes for Ada spec files.
The extensions should include a `.' if needed.")

(defvar ada-other-file-alist
  '(("\\.ads$" (".adb"))
    ("\\.adb$" (".ads")))
  "Alist used by `find-file' to find the name of the other package.
See `ff-other-file-alist'.")

(defconst ada-name-regexp
  "\\(\\(?:\\sw\\|[_.]\\)+\\)")

(defconst ada-parent-name-regexp
  "\\([a-zA-Z0-9_\\.]+\\)\\.[a-zA-Z0-9_]+"
  "Regexp for extracting the parent name from fully-qualified name.")

(defvar ada-make-filename-from-adaname 'ada-make-filename-from-adaname-default
  "Function called with one parameter ADANAME, which is a library
unit name; it should return the filename in which ADANAME is
found.")
;; IMPROVEME: It might be better if this var was a project variable;
;; each project can have a different compiler.

(defun ada-make-filename-from-adaname (adaname)
  "Return the filename in which ADANAME is found."
  (funcall ada-make-filename-from-adaname adaname))

(defun ada-make-filename-from-adaname-default (adaname)
  "Determine the filename in which ADANAME is found.
This matches the GNAT default naming convention, except for
pre-defined units."
  (while (string-match "\\." adaname)
    (setq adaname (replace-match "-" t t adaname)))
  (downcase adaname)
  )

(defun ada-ff-special-extract-separate ()
  (let ((package-name (match-string 1)))
    (save-excursion
      (goto-char (match-end 0))
      (when (eolp) (forward-char 1))
      (skip-syntax-forward " ")
      (looking-at
       (concat "\\(function\\|package body\\|procedure\\|protected body\\|task body\\)\\s +"
	       ada-name-regexp))
      (setq ff-function-name (match-string 0))
      )
    (ff-get-file-name
     ada-search-directories-internal
     (ada-make-filename-from-adaname package-name)
     ada-body-suffixes)))

(defun ada-set-ff-special-constructs ()
  "Add Ada-specific pairs to `ff-special-constructs'."
  (set (make-local-variable 'ff-special-constructs) nil)
  (mapc (lambda (pair) (add-to-list 'ff-special-constructs pair))
	;; Each cdr should set ff-function-name to a string or regexp
	;; for ada-set-point-accordingly, and return the file name
	;; (may include full path, must include suffix) to go to.
	(list
	 ;; Top level child package declaration (not body), or child
	 ;; subprogram declaration or body; go to the parent package.
	 (cons (concat "^\\(?:private[ \t]+\\)?\\(?:package\\|procedure\\|function\\)[ \t]+"
		       ada-parent-name-regexp "[ \t]+\\(?:;\\|is\\|return\\)")
	       (lambda ()
	       	 (setq ff-function-name (match-string 1))
	       	 (ff-get-file-name
	       	   ada-search-directories-internal
	       	   (ada-make-filename-from-adaname ff-function-name)
	       	   ada-spec-suffixes)))

	 ;; A "separate" clause.
	 (cons (concat "^separate[ \t\n]*(" ada-name-regexp ")")
	       'ada-ff-special-extract-separate)

	 ;; A "with" clause. Note that it may refer to a procedure body, as well as a spec
	 (cons (concat "^with[ \t]+" ada-name-regexp)
	       (lambda ()
	       (setq ff-function-name (match-string 1))
	       (ff-get-file-name
		  ada-search-directories-internal
		  (ada-make-filename-from-adaname (match-string 1))
		  (append ada-spec-suffixes ada-body-suffixes))))
	 )))

(defvar ada-which-function nil
  ;; No useful default; the indentation engine should supply a useful function
  ;; This is run from ff-pre-load-hook, so ff-function-name may have
  ;; been set by ff-treat-special; don't reset it.
  "Function called with no parameters; it should return the name
of the package, protected type, subprogram, or task type whose
definition/declaration point is in, or nil.  In addition, if
ff-function-name is non-nil, store in ff-function-name a regexp
that will find the function in the other file.")

(defun ada-which-function ()
  "See `ada-which-function' variable."
  (when ada-which-function
    (funcall ada-which-function)))

(defun ada-set-point-accordingly ()
  "Move to the string specified in `ff-function-name', which may be a regexp,
previously set by a file navigation command."
  (when ff-function-name
    (goto-char (point-min))
    (search-forward-regexp ff-function-name nil t)
    (goto-char (match-beginning 0))
    (setq ff-function-name nil)))

(defun ada-buffer-window (buffer)
  (let ((list (window-list-1 nil nil t))
	window)
    (while (and
	    (setq window (car list))
	    (not (eq (window-buffer window) buffer)))
      (setq list (cdr list)))
    window))

(defun ada-display-buffer-other-frame (buffer-or-name)
  "Display BUFFER-OR-NAME (an existing buffer or the name of an existing buffer) in another frame,
either an existing one, or a new one if there are no existing other frames."
  (let* ((buffer (if (bufferp buffer-or-name) buffer-or-name (get-buffer buffer-or-name)))
	 (window (ada-buffer-window buffer))
	 (frame-1 (and window (window-frame window)))
	 (frame-2 (car (filtered-frame-list (lambda (frame) (not (eq frame (selected-frame)))))))
	 (frame
	  (or (and window
		   frame-1
		   (not (eq frame-1 (selected-frame)))
		   frame-1)
	      frame-2
	      (make-frame))))
    (unless (and window (eq frame frame-1))
      (setq window (get-lru-window frame))) ; FIXME: might be minibuffer-window?
    (display-buffer-record-window 'reuse window buffer)
    (window--display-buffer-1 window)
    (window--display-buffer-2 buffer window)))

(defun ada-display-buffer (buffer-or-name &optional old-other-window)
  "For `display-buffer-function', which see.  Prefix arg on user-level
command determines frame and window:

nil   : no prefix - other window, current frame
'(4)  : C-u       - other window, current frame
'(16) : C-u C-u   - other frame.

Note that `ada-display-buffer' is not called when mapping a
buffer to the currently selected window is desired; other
functions handle that.

If 'other frame' is requested, and there is only one current
frame, a new frame is created. Otherwise, an existing frame is
reused.

In addition, `pop-up-frames' non-nil is interpreted to mean
'other frame', which reuses an existing frame, ignoring
`pop-up-frame-alist'.  If you set `pop-up-frames' non-nil in your
~/.emacs, you are propably expecting the default behavior, which
always pops up a new frame.  To get that, don't set
`display-buffer-function' to `ada-display-buffer' (the standard
Ada mode initialization does this correctly, if you set
`pop-up-frames' to non-nil before ada-mode.el is loaded)."
  (if (or pop-up-frames (equal current-prefix-arg '(16)))
      ;; other frame
      (ada-display-buffer-other-frame buffer-or-name)

    ;; else other window
    (let ((display-buffer-function nil))
      ;; we use standard display-buffer, since it handles this case
      ;; the way current code expects it to.
      (display-buffer buffer-or-name old-other-window)))
  )

(defun ada-find-other-file-noset (other-window-frame)
  "Same as `ada-find-other-file', but preserve point in the other file,
don't move to corresponding declaration."
  (interactive "P")
  (ada-find-other-file other-window-frame t))

(defun ada-find-other-file (other-window-frame &optional no-set-point)
  "Move to the corresponding declaration in another file.

- If region is active, assume it contains a package name;
  position point on that package declaration.

- If point is in the start line of a top level child package
  declaration (but not package body), or a child subprogram spec
  or body, position point on the corresponding parent package
  declaration.

- If point is in the start line of a top level separate body,
  position point on the corresponding separate stub declaration.

- If point is in a context clause line, position point on the
  package declaration that is mentioned.

- If point is in a subprogram body or declaration, position point
  on the corresponding declaration or body.

OTHER-WINDOW-FRAME (default nil, set by interactive prefix)
controls window and frame choice:

nil     : show in current window
C-u     : show in other window
C-u C-u : show in other frame

When called from lisp, OTHER-WINDOW-FRAME is only interpreted as
nil/non-nil; it must set `current-prefix-arg' to have the
meanings shown here.

If NO-SET-POINT is nil (the default), set point in the other file on
the corresponding declaration. If non-nil, preserve existing point in
the other file."

  ;; ff-get-file, ff-find-other file first process
  ;; ff-special-constructs, then run the following hooks:
  ;;
  ;; ff-pre-load-hook      set to ada-which-function
  ;; ff-file-created-hook  set to ada-make-body
  ;; ff-post-load-hook     set to ada-set-point-accordingly,
  ;;                       or to a compiler-specific function that
  ;;                       uses compiler-generated cross reference
  ;;                       information

  (interactive "P")
  (if mark-active
      (progn
        (setq ff-function-name (buffer-substring-no-properties (point) (mark)))
        (ff-get-file
         ada-search-directories-internal
         (ada-make-filename-from-adaname ff-function-name)
         ada-spec-suffixes
         other-window-frame)
        (deactivate-mark))

    (ff-find-other-file other-window-frame)))

(defvar ada-operator-re
  "\\+\\|-\\|/\\|\\*\\*\\|\\*\\|=\\|&\\|abs\\|mod\\|rem\\|and\\|not\\|or\\|xor\\|<=\\|<\\|>=\\|>"
  "Regexp matching Ada operator_symbol.")

(defun ada-identifier-at-point ()
  "Return the identifier around point, move point to start of
identifier.  May be an Ada identifier or operator function name."

  (when (ada-in-comment-p)
    (error "Inside comment"))

  (let (identifier)

    (skip-chars-backward "a-zA-Z0-9_<>=+\\-\\*/&")

    ;; Just in front of, or inside, a string => we could have an operator
    (cond
     ((ada-in-string-p)
      (cond

       ((and (= (char-before) ?\")
	     (progn
	       (forward-char -1)
	       (looking-at (concat "\"\\(" ada-operator-re "\\)\""))))
	(setq identifier (concat "\"" (match-string 1) "\"")))

       (t
	(error "Inside string or character constant"))
       ))

     ((and (= (char-after) ?\")
	   (looking-at (concat "\"\\(" ada-operator-re "\\)\"")))
      (setq identifier (concat "\"" (match-string 1) "\"")))

     ((looking-at "[a-zA-Z0-9_]+")
      (setq identifier (match-string 0)))

     (t
      (error "No identifier around"))
     )))

(defun ada-goto-source (file line column identifier other-window-frame)
  "Find and select FILE, at LINE and COLUMN.
If IDENTIFIER is not at resulting point, search for a declaration for it nearby.

OTHER-WINDOW-FRAME (default nil, set by interactive prefix)
controls window and frame choice:

nil     : show in current window
C-u     : show in other window
C-u C-u : show in other frame
"
  (let ((buffer (get-file-buffer file)))
    (cond
     ((bufferp buffer)
      (cond
       ((null other-window-frame)
	(switch-to-buffer buffer))

       (t (switch-to-buffer-other-window buffer))
       ))

     ((file-exists-p file)
      (cond
       ((null other-window-frame)
	(find-file file))

       (t
	(find-file-other-window file))
       ))

     (t
      (error "'%s' not found" file))))


  ;; move the cursor to the correct position
  (push-mark)
  (goto-char (point-min))
  (forward-line (1- line))
  (move-to-column column)

  ;; (unless (looking-at (ada-name-of identlist))
  ;; FIXME: if this is useful, replace with parser motion; goto next/prev decl
  )

(defvar ada-xref-function nil
  "Function that returns cross reference information.
Called with two arguments, an Ada identifier or operator_symbol, and a
'parent' flag.
point is at the start of the identifier.
Returns a list '(file line column) giving the corresponding location.
If point is at the declaration, the corresponding location is the
body, and vice versa. If the 'parent' flag is non-nil, return the
parent type declaration.")

(defun ada-goto-declaration (other-window-frame &optional parent)
  "Move to the declaration or body of the identifier around point.
If at the declaration, go to the body, and vice versa.

OTHER-WINDOW-FRAME (default nil, set by interactive prefix)
controls window and frame choice:

nil     : show in current window
C-u     : show in other window
C-u C-u : show in other frame"
  (interactive "P")

  (let ((identifier (ada-identifier-at-point))
	 target)
    (when (null ada-xref-function)
      (error "no cross reference information available"))

    (setq target (funcall ada-xref-function identifier parent))

    (ada-goto-source (nth 0 target)
		     (nth 1 target)
		     (nth 2 target)
		     identifier
		     other-window-frame)
    ))

(defun ada-goto-declaration-parent (other-window-frame)
  "Move to the parent type declaration of the type identifier around point.

OTHER-WINDOW-FRAME (default nil, set by interactive prefix)
controls window and frame choice:

nil     : show in current window
C-u     : show in other window
C-u C-u : show in other frame"
  (interactive "P")
  (ada-goto-declaration other-window-frame t))

;;;###autoload
(defun ada-add-extensions (spec body)
  "Define SPEC and BODY as being valid extensions for Ada files.
SPEC and BODY are two regular expressions that must match against
the file name."
  (let* ((reg (concat (regexp-quote body) "$"))
	 (tmp (assoc reg ada-other-file-alist)))
    (if tmp
	(setcdr tmp (list (cons spec (cadr tmp))))
      (add-to-list 'ada-other-file-alist (list reg (list spec)))))

  (let* ((reg (concat (regexp-quote spec) "$"))
	 (tmp (assoc reg ada-other-file-alist)))
    (if tmp
	(setcdr tmp (list (cons body (cadr tmp))))
      (add-to-list 'ada-other-file-alist (list reg (list body)))))

  (add-to-list 'auto-mode-alist
	       (cons (concat (regexp-quote spec) "\\'") 'ada-mode))
  (add-to-list 'auto-mode-alist
	       (cons (concat (regexp-quote body) "\\'") 'ada-mode))

  (add-to-list 'ada-spec-suffixes spec)
  (add-to-list 'ada-body-suffixes body)

  (if (fboundp 'speedbar-add-supported-extension)
      (progn
	(funcall (symbol-function 'speedbar-add-supported-extension)
		 spec)
	(funcall (symbol-function 'speedbar-add-supported-extension)
		 body)))
  )

(defun ada-other-file-name ()
  "Return the name of the other file.
The name returned is the body if `current-buffer' is the spec,
or the spec otherwise."

  (let ((is-spec nil)
	(is-body nil)
	(suffixes ada-spec-suffixes)
	(name (buffer-file-name)))

    ;;  Guess whether we have a spec or a body, and get the basename of the
    ;;  file. Since the extension may not start with '.', we can not use
    ;;  file-name-extension
    (while (and (not is-spec)
		suffixes)
      (if (string-match (concat "\\(.*\\)" (car suffixes) "$") name)
	  (setq is-spec t
		name    (match-string 1 name)))
      (setq suffixes (cdr suffixes)))

    (if (not is-spec)
	(progn
	  (setq suffixes ada-body-suffixes)
	  (while (and (not is-body)
		      suffixes)
	    (if (string-match (concat "\\(.*\\)" (car suffixes) "$") name)
		(setq is-body t
		      name    (match-string 1 name)))
	    (setq suffixes (cdr suffixes)))))

    ;;  If this wasn't in either list, return name itself
    (if (not (or is-spec is-body))
	name

      ;;  Else find the other possible names
      (if is-spec
	  (setq suffixes ada-body-suffixes)
	(setq suffixes ada-spec-suffixes))
      (setq is-spec name)

      (while suffixes

	;;  If we are using project file, search for the other file in all
	;;  the possible src directories.

	;;  FIXME (later):
	;;  better function name than ada-find-src-file-in-dir?
	;;  allow other projects (see c-parse-prj-file, EDE?)
	;;  move to ada-prj?, hook for ada-prj?

	(if (fboundp 'ada-find-src-file-in-dir)
	    (let ((other
		   (ada-find-src-file-in-dir
		    (file-name-nondirectory (concat name (car suffixes))))))
	      (if other
		  (set 'is-spec other)))

	  ;;  Else search in the current directory
	  (if (file-exists-p (concat name (car suffixes)))
	      (setq is-spec (concat name (car suffixes)))))
	(setq suffixes (cdr suffixes)))

      is-spec)))

(defun ada-get-body-name (&optional spec-name)
  ;; FIXME: how is this different from ada-other-file-name? why both?
  "Return the file name for the body of SPEC-NAME.
If SPEC-NAME is nil, return the body for the current package.
Return nil if no body was found."
  (interactive)

  (unless spec-name (setq spec-name (buffer-file-name)))

  ;; Remove the spec extension. We can not simply remove the file
  ;; extension, FIXME (later, when testing): why?  but we need to take
  ;; into account the specific non-GNAT extensions that the user might
  ;; have specified. FIXME (later) : move gnat-specific to ada-gnat,
  ;; or ada-prj for ada-search-directories.

  (let ((suffixes ada-spec-suffixes)
	end)
    (while suffixes
      (setq end (- (length spec-name) (length (car suffixes))))
      (if (string-equal (car suffixes) (substring spec-name end))
	  (setq spec-name (substring spec-name 0 end)))
      (setq suffixes (cdr suffixes))))

  (ff-get-file-name ada-search-directories-internal
		    (ada-make-filename-from-adaname
		     (file-name-nondirectory
		      (file-name-sans-extension spec-name)))
		    ada-body-suffixes))
;;; fill-comment

(defun ada-fill-comment-paragraph-justify ()
  "Fill current comment paragraph and justify each line as well."
  (interactive)
  (ada-fill-comment-paragraph 'full))

(defun ada-fill-comment-paragraph-postfix ()
  "Fill current comment paragraph and justify each line as well.
Adds `ada-fill-comment-postfix' at the end of each line."
  (interactive)
  (ada-fill-comment-paragraph 'full t))

(defvar ada-fill-comment-prefix nil)
(defvar ada-fill-comment-postfix nil)

(defun ada-fill-comment-paragraph (&optional justify postfix)
  "Fill the current comment paragraph.
If JUSTIFY is non-nil, each line is justified as well.
If POSTFIX and JUSTIFY are non-nil, `ada-fill-comment-postfix' is appended
to each line filled and justified.
The paragraph is indented on the first line."
  (interactive "P")
  ;; FIXME (later): canonical `fill-paragraph' has gotten better; try it.
  ;; check if inside comment or just in front a comment
  (if (and (not (ada-in-comment-p))
	   (not (looking-at "[ \t]*--")))
      (error "Not inside comment"))

  (let* (indent from to
	 (opos (point-marker))
	 fill-prefix
	 (fill-column (current-fill-column)))

    ;;  Find end of paragraph
    (back-to-indentation)
    (while (and (not (eobp)) (looking-at ".*--[ \t]*[^ \t\n]"))
      (forward-line 1)

      ;;  If we were at the last line in the buffer, create a dummy empty
      ;;  line at the end of the buffer.
      (if (eobp)
	  (insert "\n")
	(back-to-indentation)))
    (beginning-of-line)
    (setq to (point-marker))
    (goto-char opos)

    ;;  Find beginning of paragraph
    (back-to-indentation)
    (while (and (not (bobp)) (looking-at ".*--[ \t]*[^ \t\n]"))
      (forward-line -1)
      (back-to-indentation))

    ;;  We want one line above the first one, unless we are at the beginning
    ;;  of the buffer
    (unless (bobp)
      (forward-line 1))
    (beginning-of-line)
    (setq from (point-marker))

    ;;  Calculate the indentation we will need for the paragraph
    (back-to-indentation)
    (setq indent (current-column))
    ;;  unindent the first line of the paragraph
    (delete-region from (point))

    ;;  Remove the old postfixes
    (goto-char from)
    (while (re-search-forward "--\n" to t)
      (replace-match "\n"))

    (goto-char (1- to))
    (setq to (point-marker))

    ;;  Indent and justify the paragraph
    (setq fill-prefix ada-fill-comment-prefix)
    (set-left-margin from to indent)
    (if postfix
	(setq fill-column (- fill-column (length ada-fill-comment-postfix))))

    (fill-region-as-paragraph from to justify)

    ;;  Add the postfixes if required
    (if postfix
	(save-restriction
	  (goto-char from)
	  (narrow-to-region from to)
	  (while (not (eobp))
	    (end-of-line)
	    (insert-char ?  (- fill-column (current-column)))
	    (insert ada-fill-comment-postfix)
	    (forward-line))
	  ))

    (goto-char opos)))

;;; support for font-lock.el

(defun ada-font-lock-keywords ()
  "Ada mode keywords for font-lock, customized according to `ada-language-version'."
  ;; keywords added in ada-95:
  ;;    "abstract" "aliased" "protected" "requeue" "tagged" "until"
  ;;
  ;; 2005:
  ;;    "interface" "overriding" "synchronized"
  ;;
  ;; 2012:
  ;;    "some"

  (list

   ;; keywords followed by a name that should be in function-name-face.
   (list
    (apply
     'concat
     (append
      '("\\<\\("
	"accept\\|"
	"entry\\|"
	"function\\|"
	"package[ \t]+body\\|"
	"package\\|"
	"pragma\\|"
	"procedure\\|"
	"task[ \t]+body\\|"
	"task[ \t]+type\\|"
	"task\\|"
	)
      (when (member ada-language-version '(ada95 ada2005 ada2012))
	'("\\|"
	  "protected[ \t]+body\\|"
	  "protected[ \t]+function\\|"
	  "protected[ \t]+procedure\\|"
	  "protected[ \t]+type\\|"
	  "protected"
	  ))
      (list
       "\\)\\>[ \t]*"
       ada-name-regexp "?")))
    '(1 font-lock-keyword-face) '(2 font-lock-function-name-face nil t))

   ;; keywords followed by a name that should be in type-face.
   (list (concat
	  "\\<\\("
	  "access[ \t]+all\\|"
	  "access[ \t]+constant\\|"
	  "access\\|"
	  "constant\\|"
	  "in[ \t]+reverse\\|"; loop iterator
	  "in[ \t]+not[ \t]+null\\|"
	  "in[ \t]+out[ \t]+not[ \t]+null\\|"
	  "in[ \t]+out\\|"
	  "in\\|"
	  ;; "return\\|" can't distinguish between 'function ... return <type>;' and 'return ...;'
	  ;; An indentation engine can, so a rule for this is added there
	  "of\\|"
	  "out\\|"
	  "subtype\\|"
	  "type"
	  "\\)\\>[ \t]*"
	  ada-name-regexp "?")
	 '(1 font-lock-keyword-face nil t) '(2 font-lock-type-face nil t))

   ;; Keywords not treated elsewhere. After above so it doesn't
   ;; override fontication of second or third word in those patterns.
   (list (concat
   	  "\\<"
   	  (regexp-opt
   	   (append
   	    '("abort" "abs" "accept" "all"
   	      "and" "array" "at" "begin" "case" "declare" "delay" "delta"
   	      "digits" "do" "else" "elsif" "entry" "exception" "exit" "for"
   	      "generic" "if" "in" "limited" "loop" "mod" "not"
   	      "null" "or" "others" "private" "raise"
   	      "range" "record" "rem" "renames" "reverse"
   	      "select" "separate" "task" "terminate"
   	      "then" "when" "while" "xor")
   	    (when (member ada-language-version '(ada95 ada2005 ada2012))
   	      '("abstract" "aliased" "requeue" "tagged" "until"))
   	    (when (member ada-language-version '(ada2005 ada2012))
   	      '("interface" "overriding" "synchronized"))
   	    (when (member ada-language-version '(ada2012))
   	      '("some"))
   	    )
   	   t)
   	  "\\>")
   	 '(0 font-lock-keyword-face))

   ;; object and parameter declarations; word after ":" should be in
   ;; type-face if not already fontified or an exception.
   (list (concat
	  ":[ \t]*"
	  ada-name-regexp
	  "[ \t]*\\(=>\\)?")
     '(1 (if (match-beginning 2)
	     'default
	   font-lock-type-face)
	 nil t))

   ;; keywords followed by a name that should be in function-name-face if not already fontified
   (list (concat
	  "\\<\\("
	  "end"
	  "\\)\\>[ \t]*"
	  ada-name-regexp "?")
     '(1 font-lock-keyword-face) '(2 font-lock-function-name-face nil t))

   ;; keywords followed by a name that should be in type-face if not already fontified
   (list (concat
	  "\\<\\("
	  "is"
	  "\\)\\>[ \t]*"
	  ada-name-regexp "?")
     '(1 font-lock-keyword-face) '(2 font-lock-type-face nil t))

   ;; Keywords followed by a name that could be a type or a function (generic instantiation).
   (list (concat
	  "\\<\\("
	  "new"
	  "\\)\\>[ \t]*"
	  ada-name-regexp "?[ \t]*\\((\\)?")
	 '(1 font-lock-keyword-face)
	 '(2 (if (match-beginning 3)
		 font-lock-function-name-face
	       font-lock-type-face)
	     nil t))

   ;; Keywords followed by a comma separated list of names which
   ;; should be in constant-face, unless already fontified. Ada mode 4.01 used this.
   (list (concat
   	  "\\<\\("
   	  "goto\\|"
   	  "use\\|"
   	  ;; don't need "limited" "private" here; they are matched separately
   	  "with"; context clause
   	  "\\)\\>[ \t]*"
   	  "\\(\\(?:\\sw\\|[_., \t]\\)+\\>\\)?"; ada-name-regexp, plus ", \t"
   	  )
   	 '(1 font-lock-keyword-face) '(2 font-lock-constant-face nil t))

   ;; statement labels
   '("<<\\(\\sw+\\)>>" 1 font-lock-constant-face)

   ;; based numberic literals
   (list "\\([0-9]+#[0-9a-fA-F_]+#\\)" '(1 font-lock-constant-face t))

   ;; numeric literals
   (list "\\W\\([-+]?[0-9._]+\\)\\>" '(1 font-lock-constant-face))

   ))

;;;###autoload
(define-derived-mode ada-mode prog-mode "Ada"
  "Ada mode is the major mode for editing Ada code."
  ;; the other ada-*.el files add to ada-mode-hook for their setup
  :group 'ada

  (set-syntax-table ada-mode-syntax-table)
  (set (make-local-variable 'syntax-propertize-function) 'ada-syntax-propertize)
  (set (make-local-variable 'syntax-begin-function) nil)
  ;(set (make-local-variable 'parse-sexp-ignore-comments) t) done in prog-mode
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (set 'case-fold-search t); Ada is case insensitive; the syntax parsing requires this setting
  (set (make-local-variable 'comment-start) "--")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start-skip) "---*[ \t]*")
  (set (make-local-variable 'comment-multi-line) nil)

  (set (make-local-variable 'font-lock-defaults)
       '(ada-font-lock-keywords
	 nil t
	 ((?\_ . "w")))); treat underscore as a word component

  ;; AdaCore standard style (enforced by -gnaty) requires two spaces
  ;; after '--' in comments; this makes it easier to distinguish
  ;; special comments that have something else after '--'
  (set (make-local-variable 'comment-padding) "  ")

  (set (make-local-variable 'ff-other-file-alist)
       'ada-other-file-alist)
  (setq ff-post-load-hook    'ada-set-point-accordingly
	ff-file-created-hook 'ada-make-body)
  (add-hook 'ff-pre-load-hook 'ada-which-function)
  (ada-set-ff-special-constructs)

  (set (make-local-variable 'add-log-current-defun-function)
       'ada-which-function)

  (add-hook 'which-func-functions 'ada-which-function nil t)

  ;;(set (make-local-variable 'ispell-check-comments) 'exclusive) FIXME: ispell var name has changed

  ;;  Support for align
  (add-to-list 'align-dq-string-modes 'ada-mode)
  (add-to-list 'align-open-comment-modes 'ada-mode)
  (set (make-local-variable 'align-region-separate) ada-align-region-separate)

  ;; Exclude comments alone on line from alignment.
  (add-to-list 'align-exclude-rules-list
	       '(ada-solo-comment
		 (regexp  . "^\\(\\s-*\\)--")
		 (modes   . '(ada-mode))))
  (add-to-list 'align-exclude-rules-list
	       '(ada-solo-use
		 (regexp  . "^\\(\\s-*\\)\\<use\\>")
		 (modes   . '(ada-mode))))

  (setq align-mode-rules-list ada-align-modes)

  ;;  Set up the contextual menu
  (if ada-popup-key
      (define-key ada-mode-map ada-popup-key 'ada-popup-menu))

  (define-abbrev-table 'ada-mode-abbrev-table ())
  (setq local-abbrev-table ada-mode-abbrev-table)

  (easy-menu-add ada-mode-menu ada-mode-map)

  ;; (run-mode-hooks 'ada-mode-hook) done after this body by define-minor-mode

  ;; These are run after ada-mode-hook because users or other ada-*
  ;; files might set the relevant variable inside the hook
  ;; FIXME (later): use (add-hook 'hack-local-variables-hook; see emacs 24.2 ada-mode.el

  ;; (if ada-auto-case
  ;;     (ada-activate-keys-for-case))
  ;; FIXME(later): Use post-self-insert-hook instead of changing key bindings.

  )


;;;; Global initializations

;; Setup auto-loading of the other Ada mode files.
;; FIXME (later): add some here?

;; provide some dummy functions so other code can at least run
;; FIXME (later): make these real, somewhere
(defun ada-adjust-case-identifier ()); get this from emacs_stephe/ada-mode-keys.el
(defun ada-adjust-case () (capitalize-word 1))
(defvar ada-case-exception-file nil)

;; load indent engine first; compilers may need to know which is being
;; used (for preprocessor keywords, for example).
(unless (featurep 'ada-indent-engine)
  (require 'ada-smie))

(unless (featurep 'ada-compiler)
  (require 'ada-gnat))

(unless (or pop-up-frames display-buffer-function)
  (setq display-buffer-function 'ada-display-buffer))

(provide 'ada-mode)

;;; end of file
