;;; ada-mode.el --- major-mode for editing Ada sources
;;;
;;; FIXME: this is just the start of a major rewrite; just enough to
;;; test the new smie-based ada-indent.el, and handle gnat compilation
;;; errors (ie, enough for the basic code development cycle).
;;;
;;; Also deleted all Xemacs support, and all pre-Emacs 24.2 support.
;;;
;;; should delete font-lock, since it can now use the smie parser, but
;;; I can't stand reading code without fontlock; later :) Note that
;;; the current font-lock stuff may actually interfere with the
;;; parser. Sigh.
;;;
;;; So far, I've copied ada-mode 4.01, and deleted everything that has
;;; to do with indentation or casing. imenu, outline, which-function,
;;; add-log, format-paramlist, narrow-to-defun also deleted, since
;;; they might be able to take advantage of the smie parser.
;;;
;;; skeleton, templates deleted because it might use semantic
;;;
;;; ada-xref deleted because it will become more orthogonal (it relies
;;; on gnat xref output; it should be easy to use other compiler xref
;;; output).
;;;
;;; Similary ada-prj assumes gnat.
;;;
;;; ada-prj-edit deleted because it won't be supported any more
;;;
;;; compile, build, debug deleted because I don't use them, and I'm
;;; not sure if they are gnat-dependent.

;; Copyright (C) 1994, 1995, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
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
;; features,...).  If you are using another compiler, you might want
;; to set the following variable (FIXME: do not set this in the
;; ada-mode-hook, it won't work) : (setq ada-which-compiler 'generic)

;;; History:
;; The first Ada mode for GNU Emacs was written by V. Broman in
;; 1985. He based his work on the already existing Modula-2 mode.
;; This was distributed as ada.el in versions of Emacs prior to 19.29.
;;
;; Lynn Slater wrote an extensive Ada mode in 1989. It consisted of
;; several files with support for dired commands and other nice
;; things. It is currently available from the PAL
;; (wuarchive.wustl.edu:/languages/ada) as ada-mode-1.06a.tar.Z.
;;
;; The probably very first Ada mode (called electric-ada.el) was
;; written by Steven D. Litvintchouk and Steven M. Rosen for the
;; Gosling Emacs. L. Slater based his development on ada.el and
;; electric-ada.el.
;;
;; A complete rewrite by M. Heritsch and R. Ebert was been done at
;; some point.  Some ideas from the Ada mode mailing list have been
;; added.  Some of the functionality of L. Slater's mode has not (yet)
;; been recoded in this new mode.  Perhaps you prefer sticking to his
;; version.
;;
;; A complete rewrite for Emacs-20 / GNAT-3.11 was been done by Ada Core
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
This hook is automatically executed after `ada-mode' is
fully loaded.
This is a good place to add Ada environment specific bindings.")

(defgroup ada nil
  "Major mode for editing and compiling Ada source in Emacs."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'languages)

(defcustom ada-language-version 'ada2012
  "*Ada language version; one of `ada83', `ada95', `ada2005'.
Only affects the keywords to highlight." ;; FIXME: not actually used for that? FIXME: anything else?
  :type '(choice (const ada83) (const ada95) (const ada2005) (const ada2012)) :group 'ada)

(defcustom ada-popup-key '[down-mouse-3]
  "*Key used for binding the contextual menu.
If nil, no contextual menu is available."
  :type '(restricted-sexp :match-alternatives (stringp vectorp))
  :group 'ada)

;;; ---- end of user configurable variables; see other ada-*.el files for more


(defvar ada-mode-menu (make-sparse-keymap "Ada")
  "Menu for Ada mode.")

(defvar ada-mode-map (make-sparse-keymap)
  "Local keymap used for Ada mode.")

(defvar ada-mode-extra-map (make-sparse-keymap)
  "Keymap used for non-standard keybindings.")

;; default is C-c C-q because it's free in ada-mode-map
(defvar ada-mode-extra-prefix "\C-c\C-q"
  "Prefix key to access `ada-mode-extra-map' functions.")

(defvar ada-mode-abbrev-table nil
  "Local abbrev table for Ada mode.")

(defvar ada-mode-syntax-table nil
  "Syntax table to be used for editing Ada source code.")

;; FIXME: not used for anything?
;; (eval-when-compile
;;   ;; These values are used in eval-when-compile expressions.
;;   (defconst ada-83-string-keywords
;;     '("abort" "abs" "accept" "access" "all" "and" "array" "at" "begin"
;;       "body" "case" "constant" "declare" "delay" "delta" "digits" "do"
;;       "else" "elsif" "end" "entry" "exception" "exit" "for" "function"
;;       "generic" "goto" "if" "in" "is" "limited" "loop" "mod" "new"
;;       "not" "null" "of" "or" "others" "out" "package" "pragma" "private"
;;       "procedure" "raise" "range" "record" "rem" "renames" "return"
;;       "reverse" "select" "separate" "subtype" "task" "terminate" "then"
;;       "type" "use" "when" "while" "with" "xor")
;;     "List of Ada 83 keywords.
;; Used to define `ada-*-keywords'.")

;;   (defconst ada-95-string-keywords
;;     '("abstract" "aliased" "protected" "requeue" "tagged" "until")
;;     "List of keywords new in Ada 95.
;; Used to define `ada-*-keywords'.")

;;   (defconst ada-2005-string-keywords
;;     '("interface" "overriding" "synchronized")
;;     "List of keywords new in Ada 2005.
;; Used to define `ada-*-keywords.'"))

;;   (defconst ada-2012-string-keywords
;;     '("some")
;;     "List of keywords new in Ada 2012.
;; Used to define `ada-*-keywords.'"))

;; (defconst ada-83-keywords
;;   (eval-when-compile
;;     (concat "\\<" (regexp-opt ada-83-string-keywords t) "\\>"))
;;   "Regular expression matching Ada83 keywords.")

;; (defconst ada-95-keywords
;;   (eval-when-compile
;;     (concat "\\<" (regexp-opt
;; 		   (append
;; 		    ada-95-string-keywords
;; 		    ada-83-string-keywords) t) "\\>"))
;;   "Regular expression matching Ada95 keywords.")

;; (defconst ada-2005-keywords
;;   (eval-when-compile
;;     (concat "\\<" (regexp-opt
;; 		   (append
;; 		    ada-2005-string-keywords
;; 		    ada-83-string-keywords
;; 		    ada-95-string-keywords) t) "\\>"))
;;   "Regular expression matching Ada2005 keywords.")

;; (defvar ada-keywords ada-2005-keywords
;;   "Regular expression matching Ada keywords.")
;; not customizable; this is set in ada-mode, controlled by ada-language-version

(defvar ada-body-suffixes '(".adb")
  "List of possible suffixes for Ada body files.
The extensions should include a `.' if needed.")

(defvar ada-spec-suffixes '(".ads")
  "List of possible suffixes for Ada spec files.
The extensions should include a `.' if needed.")

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

;;------------------------------------------------------------
;;  Support for compile.el
;;------------------------------------------------------------

(defun ada-compile-mouse-goto-error ()
  "Mouse interface for `ada-compile-goto-error'."
  (interactive)
  (mouse-set-point last-input-event)
  (ada-compile-goto-error (point))
  )

(defun ada-compile-goto-error (pos)
  "Replace `compile-goto-error' from compile.el.
If POS is on a file and line location, go to this position.  It adds
to compile.el the capacity to go to a reference in an error message.
For instance, on these lines:
  foo.adb:61:11:  [...] in call to size declared at foo.ads:11
  foo.adb:61:11:  [...] in call to local declared at line 20
the 4 file locations can be clicked on and jumped to."
  (interactive "d")
  (goto-char pos)

  (skip-chars-backward "-a-zA-Z0-9_:./\\")
  (cond
   ;;  special case: looking at a filename:line not at the beginning of a line
   ;;  or a simple line reference "at line ..."
   ((and (not (bolp))
	 (or (looking-at ada-compile-goto-error-file-linenr-re)
	     (and
	      (save-excursion
		(beginning-of-line)
		(looking-at ada-compile-goto-error-file-linenr-re))
	      (save-excursion
		(if (looking-at "\\([0-9]+\\)") (backward-word 1))
		(looking-at "line \\([0-9]+\\)"))))
	     )
    (let ((line (if (match-beginning 2) (match-string 2) (match-string 1)))
	  (file (if (match-beginning 2) (match-string 1)
		  (save-excursion (beginning-of-line)
				  (looking-at ada-compile-goto-error-file-linenr-re)
				  (match-string 1))))
	  (error-pos (point-marker))
	  source)

      ;; set source marker
      (save-excursion
	(compilation-find-file (point-marker) (match-string 1) "./")
	(set-buffer file)

	(when (stringp line)
	  (goto-char (point-min))
	  (forward-line (1- (string-to-number line))))

	(setq source (point-marker)))

      (compilation-goto-locus error-pos source nil)

      ))

   ;; otherwise, default behavior
   (t
    (compile-goto-error))
   )
  (recenter))

(defun ada-create-syntax-table ()
  "Create the syntax table; the indentation engine and casing rely on this."
  ;; FIXME: move this to variable init? no need for a function
  (interactive)
  (setq ada-mode-syntax-table (make-syntax-table))
  ;; make-syntax-table sets all alphanumeric to w, etc; so we only
  ;; have to add ada-specific things.

  ;; string brackets. `%' is the obsolete alternative string
  ;; bracket (arm J.2); if we make it syntax class ", it throws
  ;; font-lock and indentation off the track, so we use syntax class
  ;; $.
  (modify-syntax-entry ?%  "$" ada-mode-syntax-table)
  (modify-syntax-entry ?\" "\"" ada-mode-syntax-table)

  ;; punctuation
  (modify-syntax-entry ?:  "." ada-mode-syntax-table)
  (modify-syntax-entry ?\; "." ada-mode-syntax-table)
  (modify-syntax-entry ?&  "." ada-mode-syntax-table)
  (modify-syntax-entry ?\|  "." ada-mode-syntax-table)
  (modify-syntax-entry ?+  "." ada-mode-syntax-table)
  (modify-syntax-entry ?*  "." ada-mode-syntax-table)
  (modify-syntax-entry ?/  "." ada-mode-syntax-table)
  (modify-syntax-entry ?=  "." ada-mode-syntax-table)
  (modify-syntax-entry ?<  "." ada-mode-syntax-table)
  (modify-syntax-entry ?>  "." ada-mode-syntax-table)
  (modify-syntax-entry ?$ "." ada-mode-syntax-table)
  (modify-syntax-entry ?\[ "." ada-mode-syntax-table)
  (modify-syntax-entry ?\] "." ada-mode-syntax-table)
  (modify-syntax-entry ?\{ "." ada-mode-syntax-table)
  (modify-syntax-entry ?\} "." ada-mode-syntax-table)
  (modify-syntax-entry ?. "." ada-mode-syntax-table)
  (modify-syntax-entry ?\\ "." ada-mode-syntax-table)
  (modify-syntax-entry ?\' "." ada-mode-syntax-table)

  ;; a single hyphen is punctuation, but a double hyphen starts a comment
  (modify-syntax-entry ?-  ". 12" ada-mode-syntax-table)

  ;; See the comment above on grammar related function for the special
  ;; setup for '#'. FIXME: that comment moved somewhere else
  (modify-syntax-entry ?#  "$" ada-mode-syntax-table)

  ;; and \f and \n end a comment
  (modify-syntax-entry ?\f  ">   " ada-mode-syntax-table)
  (modify-syntax-entry ?\n  ">   " ada-mode-syntax-table)

  ;; symbol constituents
  (modify-syntax-entry ?_ "_" ada-mode-syntax-table)

  ;; define parentheses to match
  (modify-syntax-entry ?\( "()" ada-mode-syntax-table)
  (modify-syntax-entry ?\) ")(" ada-mode-syntax-table)
  )

(defun ada-set-syntax-table-properties ()
  "Assign `syntax-table' properties in accessible part of buffer.
In particular, character constants are said to be strings, #...#
are treated as numbers instead of gnatprep comments."
  ;; (info "(elisp)Syntax Properties")
  (let ((modified (buffer-modified-p))
	(buffer-undo-list t)
	(inhibit-read-only t)
	(inhibit-point-motion-hooks t)
	(inhibit-modification-hooks t))
    (remove-text-properties (point-min) (point-max) '(syntax-table nil))
    (goto-char (point-min))
    (while (re-search-forward
	    ;; The following regexp was adapted from
	    ;; `ada-font-lock-syntactic-keywords'.
	    "^[ \t]*\\(#\\(?:if\\|else\\|elsif\\|end\\)\\)\\|[^a-zA-Z0-9)]\\('\\)[^'\n]\\('\\)"
	    nil t)
      (if (match-beginning 1)
	  (put-text-property
	       (match-beginning 1) (match-end 1) 'syntax-table '(11 . ?\n))
	(put-text-property
	     (match-beginning 2) (match-end 2) 'syntax-table '(7 . ?'))
	(put-text-property
	     (match-beginning 3) (match-end 3) 'syntax-table '(7 . ?'))))
    (unless modified
      (restore-buffer-modified-p nil))))

(defun ada-after-change-function (beg end old-len)
  "Called when the region between BEG and END was changed in the buffer.
OLD-LEN indicates what the length of the replaced text was.
Update the syntax table properties."
  (save-excursion
    (save-restriction
      (let ((from (progn (goto-char beg) (line-beginning-position)))
	    (to (progn (goto-char end) (line-end-position))))
	(narrow-to-region from to)
	(save-match-data
	  (ada-set-syntax-table-properties))))))

(defun ada-initialize-syntax-table-properties ()
  "Assign `syntax-table' properties in current buffer."
    (save-excursion
      (save-restriction
	(widen)
	(save-match-data
	  (ada-set-syntax-table-properties))))
    (add-hook 'after-change-functions 'ada-after-change-function nil t))

(defun ada-handle-syntax-table-properties ()
  "Handle `syntax-table' properties."
  ;; FIXME: better name and/or doc; only called from ada-mode?
  (if font-lock-mode
      ;; `font-lock-mode' will take care of `syntax-table' properties.
      (remove-hook 'after-change-functions 'ada-after-change-function t)
    ;; Take care of `syntax-table' properties manually.
    (ada-initialize-syntax-table-properties)))

 ;; context menu

(defun ada-call-from-contextual-menu (function)
  "Execute FUNCTION when called from the contextual menu.
It forces Emacs to change the cursor position."
  (interactive)
  (funcall function)
  (setq ada-contextual-menu-last-point
	(list (point) (current-buffer))))

(defun ada-popup-menu (position)
  "Pops up a contextual menu, depending on where the user clicked.
POSITION is the location the mouse was clicked on.
Sets `ada-contextual-menu-last-point' to the current position before
displaying the menu.  When a function from the menu is called, the
point is where the mouse button was clicked."
  (interactive "e")

  ;;  declare this as a local variable, so that the function called
  ;;  in the contextual menu does not hide the region in
  ;;  transient-mark-mode.
  (let ((deactivate-mark nil))
    (setq ada-contextual-menu-last-point
	 (list (point) (current-buffer)))
    (mouse-set-point last-input-event)

    (setq ada-contextual-menu-on-identifier
	  (and (char-after)
	       (or (= (char-syntax (char-after)) ?w)
		   (= (char-after) ?_))
	       (not (ada-in-string-or-comment-p))
	       (save-excursion (skip-syntax-forward "w")
			       (not (ada-after-keyword-p)))
	       ))
    (if (fboundp 'popup-menu)
	(funcall (symbol-function 'popup-menu) ada-contextual-menu)
      (let (choice)
	(setq choice (x-popup-menu position ada-contextual-menu))
	(if choice
	    (funcall (lookup-key ada-contextual-menu (vector (car choice)))))))

    (set-buffer (cadr ada-contextual-menu-last-point))
    (goto-char (car ada-contextual-menu-last-point))
    ))

;;------------------------------------------------------------------
;; Misc functions
;;------------------------------------------------------------------


;;;###autoload
(defun ada-mode ()
  "Ada mode is the major mode for editing Ada code."
  ;; the other ada-*.el files add to ada-mode-hook for their setup

  (interactive)
  (kill-all-local-variables)

  (set (make-local-variable 'require-final-newline) mode-require-final-newline)

  (set-syntax-table ada-mode-syntax-table)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (set 'case-fold-search t); Ada is case insensitive; the syntax parsing requires this setting
  (set (make-local-variable 'comment-start) "--")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start-skip) "---*[ \t]*")

  ;; AdaCore standard style (enforced by -gnaty) requires two spaces
  ;; after '--' in comments; this makes it easier to distinguish
  ;; special comments that have something else after '--'
  (set (make-local-variable 'comment-padding) "  ")

  ;; used by autofill to break a comment line and continue it on another line.
  ;; The reason we need this one is that the default behavior does not work
  ;; correctly with the definition of paragraph-start above when the comment
  ;; is right after a multi-line subprogram declaration (the comments are
  ;; aligned under the latest parameter, not under the declaration start).
  ;; FIXME: deleted paragraph-start; test again
  (set (make-local-variable 'comment-line-break-function)
       (lambda (&optional soft) (let ((fill-prefix nil))
				  (indent-new-comment-line soft))))

  (set (make-local-variable 'comment-multi-line) nil)


  (add-hook 'compilation-mode-hook
	    (lambda()
	      ;; FIXME: This has global impact!  -stef
	      (define-key compilation-minor-mode-map [mouse-2]
		'ada-compile-mouse-goto-error)
	      (define-key compilation-minor-mode-map "\C-c\C-c"
		'ada-compile-goto-error)
	      (define-key compilation-minor-mode-map "\C-m"
		'ada-compile-goto-error)))

  (set (make-local-variable 'font-lock-defaults)
       '(ada-font-lock-keywords
	 nil t
	 ((?\_ . "w") (?# . "."))
	 beginning-of-line
	 ;; FIXME: font-lock-syntactic-keywords is obsolete; use syntax-propertize-function
	 (font-lock-syntactic-keywords . ada-font-lock-syntactic-keywords)))

  (set (make-local-variable 'ff-other-file-alist)
       'ada-other-file-alist)
  (set (make-local-variable 'ff-search-directories)
       'ada-search-directories-internal)
  (setq ff-post-load-hook    'ada-set-point-accordingly
	ff-file-created-hook 'ada-make-body)
  (add-hook 'ff-pre-load-hook 'ada-which-function-are-we-in)

  (make-local-variable 'ff-special-constructs)
  (mapc (lambda (pair) (add-to-list 'ff-special-constructs pair))
	(list
	 ;; Top level child package declaration; go to the parent package.
	 (cons (eval-when-compile
		 (concat "^\\(private[ \t]\\)?[ \t]*package[ \t]+"
			 "\\(body[ \t]+\\)?"
			 "\\(\\(\\sw\\|[_.]\\)+\\)\\.\\(\\sw\\|_\\)+[ \t\n]+is"))
	       (lambda ()
		 (ff-get-file
		  ada-search-directories-internal
		  (ada-make-filename-from-adaname (match-string 3))
		  ada-spec-suffixes)))

	 ;; A "separate" clause.
	 (cons "^separate[ \t\n]*(\\(\\(\\sw\\|[_.]\\)+\\))"
	       (lambda ()
		 (ff-get-file
		  ada-search-directories-internal
		  (ada-make-filename-from-adaname (match-string 1))
		  ada-spec-suffixes)))

	 ;; A "with" clause. Note that it may refer to a procedure body, as well as a spec
	 (cons "^with[ \t]+\\([a-zA-Z0-9_\\.]+\\)"
	       (lambda ()
		 (ff-get-file
		  ada-search-directories-internal
		  (ada-make-filename-from-adaname (match-string 1))
		  (append ada-spec-suffixes ada-body-suffixes))))
	 ))

  (set (make-local-variable 'ispell-check-comments) 'exclusive)

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

  (setq major-mode 'ada-mode
	mode-name "Ada")

  (use-local-map ada-mode-map)

  (easy-menu-add ada-mode-menu ada-mode-map)

  (run-mode-hooks 'ada-mode-hook)

  ;; Run after ada-mode-hook because users might activate
  ;; font-lock-mode in the hook FIXME: why does that matter?
  (ada-initialize-syntax-table-properties)
  (add-hook 'font-lock-mode-hook 'ada-handle-syntax-table-properties nil t)

  ;; Run after ada-mode-hook because users might set
  ;; the relevant variable inside the hook

  ;; (setq ada-keywords
  ;; 	(case ada-language-version
  ;; 	  ('ada83 ada-83-keywords)
  ;; 	  ('ada95 ada-95-keywords)
  ;; 	  ('ada2005 ada-2005-keywords)
  ;; 	  ('ada2012 ada-2012-keywords)
  ;; 	  (t (error "ada-language-version must be one of 'ada83 'ada95 'ada2005 'ada2012"))))

  ;; (if ada-auto-case
  ;;     (ada-activate-keys-for-case))
  )

(defun ada-region-selected ()
  "Should we operate on an active region?"
  (use-region-p)
  ;; Emacs 21.2 doesn't have region-active-p or use-region-p
  ;; FIXME: replace with use-region-p
  )

;; ------------------------------------------------------------
;; --  Define keymap and menus for Ada
;; -------------------------------------------------------------

(defun ada-create-keymap ()
  "Create the keymap associated with the Ada mode."

  (define-key ada-mode-map "\C-c\C-c" 'compile)

  ;; Make body
  (define-key ada-mode-map "\C-c\C-n" 'ada-make-subprogram-body)

  (define-key ada-mode-map "\C-c;"    'comment-region)

  (define-key ada-mode-map "\C-c5\C-d" 'ada-goto-declaration-other-frame)
  (define-key ada-mode-map "\C-c\C-d"  'ada-goto-declaration)
  (define-key ada-mode-map "\C-c\C-s"  'ada-xref-goto-previous-reference)
  (define-key ada-mode-map "\C-c\C-c"  'ada-compile-application)

  ;; All non-standard keys go into ada-mode-extra-map
  ;; FIXME: find standard keys for these?
  (define-key ada-mode-map ada-mode-extra-prefix ada-mode-extra-map)
  (define-key ada-mode-extra-map "o"     'ff-find-other-file)
  (define-key ada-mode-extra-map "f"     'ada-find-file)
  )

(defun ada-create-menu ()
  "Create the Ada menu as shown in the menu bar."
  (let ((m '("Ada"
	     ("Help"
	      ["Ada Mode"               (info "ada-mode") t]
	      ["GNAT User's Guide"      (info "gnat_ugn")
	       (eq ada-which-compiler 'gnat)]
	      ["GNAT Reference Manual"  (info "gnat_rm")
	       (eq ada-which-compiler 'gnat)]
	      ["Gcc Documentation"      (info "gcc")
	       (eq ada-which-compiler 'gnat)]
	      ["Gdb Documentation"      (info "gdb")
	       (eq ada-which-compiler 'gnat)]
	      ["Ada Reference Manual" (info "arm2012") t]
	      )

	     ["Customize"     (customize-group 'ada)]
	     ["------"        nil nil]
	     ["Next compilation error"  next-error             t]
	     ["Other File"              ff-find-other-file     t]
	     ["Other File Other Window" (lambda () (ff-find-other-file t))    t]
	     ("Edit"
	      ["Indent Line"                 indent-for-tab-command  t]
	      ["Indent Lines in Selection"   indent-region           t]
	      ["Indent Lines in File"        (indent-region (point-min) (point-max))  t]
	      ["Comment Selection"           comment-region               t]
	      ["Uncomment Selection"         (lambda () (comment-region t)) t]
	      ["Fill Comment Paragraph"      fill-paragraph               t]
	      ["Fill Comment Paragraph Justify"
	       ada-fill-comment-paragraph-justify                         t]
	      ["Fill Comment Paragraph Postfix"
	       ada-fill-comment-paragraph-postfix                         t]
	      ["---"                         nil                          nil]
	      ["Make body for subprogram"    ada-make-subprogram-body     t]
	      ["Narrow to subprogram"        narrow-to-defun          t]
	      )
	     )))

    (easy-menu-define ada-mode-menu ada-mode-map "Menu keymap for Ada mode" m)
    ))

(defun ada-fill-comment-paragraph-justify ()
  "Fill current comment paragraph and justify each line as well."
  (interactive)
  (ada-fill-comment-paragraph 'full))

(defun ada-fill-comment-paragraph-postfix ()
  "Fill current comment paragraph and justify each line as well.
Adds `ada-fill-comment-postfix' at the end of each line."
  (interactive)
  (ada-fill-comment-paragraph 'full t))

(defun ada-fill-comment-paragraph (&optional justify postfix)
  "Fill the current comment paragraph.
If JUSTIFY is non-nil, each line is justified as well.
If POSTFIX and JUSTIFY are non-nil, `ada-fill-comment-postfix' is appended
to each line filled and justified.
The paragraph is indented on the first line."
  (interactive "P")

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


;; ---------------------------------------------------
;;    support for find-file.el
;;
;; These functions are used by find-file to guess the file names from
;; unit names, and to find the other file (spec or body) from the current
;; file (body or spec).
;;
;; Also used to find in which function we are, so as to put the
;; cursor at the correct position.
;; ---------------------------------------------------

(defvar ada-other-file-alist nil
  "Alist used by `find-file' to find the name of the other package.
See `ff-other-file-alist'.")

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

;; Overridden when we work with GNAT, to use gnatkrunch
(defun ada-make-filename-from-adaname (adaname)
  "Determine the filename in which ADANAME is found.
This matches the GNAT default naming convention, except for
pre-defined units."
  (while (string-match "\\." adaname)
    (setq adaname (replace-match "-" t t adaname)))
  (downcase adaname)
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
	;;  FIXME: better function name, allow other projects (see c-parse-prj-file)

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

(defun ada-set-point-accordingly ()
  "Move to the function declaration that was set by `ff-which-function-are-we-in'."
  (if ff-function-name
      (progn
	(goto-char (point-min))
	;; FIXME: this often goes to the wrong place; at least only goto declaration starts (using syntax)
	(unless (ada-search-ignore-string-comment
		 (concat ff-function-name "\\b") nil)
	  (goto-char (point-min))))))

(defun ada-get-body-name (&optional spec-name)
  "Return the file name for the body of SPEC-NAME.
If SPEC-NAME is nil, return the body for the current package.
Return nil if no body was found."
  (interactive)

  (unless spec-name (setq spec-name (buffer-file-name)))

  ;; Remove the spec extension. We can not simply remove the file extension,
  ;; but we need to take into account the specific non-GNAT extensions that the
  ;; user might have specified.

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

;; ---------------------------------------------------
;;    support for font-lock.el
;; Strings are a real pain in Ada because a single quote character is
;; overloaded as a string quote and type/instance delimiter.  By default, a
;; single quote is given punctuation syntax in `ada-mode-syntax-table'.
;; So, for Font Lock mode purposes, we mark single quotes as having string
;; syntax when the gods that created Ada determine them to be.

(defconst ada-font-lock-syntactic-keywords
  ;; Mark single quotes as having string quote syntax in 'c' instances.
  ;; We used to explicitly avoid ''' as a special case for fear the buffer
  ;; be highlighted as a string, but it seems this fear is unfounded.
  ;;
  ;; This sets the properties of the characters, so that ada-in-string-p
  ;; correctly handles '"' too...
  '(("[^a-zA-Z0-9)]\\('\\)[^\n]\\('\\)" (1 (7 . ?')) (2 (7 . ?')))
    ("^[ \t]*\\(#\\(if\\|else\\|elsif\\|end\\)\\)" (1 (11 . ?\n)))))

(defvar ada-font-lock-keywords
  ;; FIXME: customize according to ada-language-version?
  ;; FIXME: add 'some' for ada2012?
  (eval-when-compile
    (list
     ;;
     ;; handle "type T is access function return S;"
     (list "\\<\\(function[ \t]+return\\)\\>" '(1 font-lock-keyword-face) )

     ;;  preprocessor line
     (list "^[ \t]*\\(#.*\n\\)"  '(1 font-lock-type-face t))

     ;;
     ;; accept, entry, function, package (body), protected (body|type),
     ;; pragma, procedure, task (body) plus name.
     (list (concat
	    "\\<\\("
	    "accept\\|"
	    "entry\\|"
	    "function\\|"
	    "package[ \t]+body\\|"
	    "package\\|"
	    "pragma\\|"
	    "procedure\\|"
	    "protected[ \t]+body\\|"
	    "protected[ \t]+type\\|"
	    "protected\\|"
	    "task[ \t]+body\\|"
	    "task[ \t]+type\\|"
	    "task"
	    "\\)\\>[ \t]*"
	    "\\(\\sw+\\(\\.\\sw*\\)*\\)?")
	   '(1 font-lock-keyword-face) '(2 font-lock-function-name-face nil t))
     ;;
     ;; Optional keywords followed by a type name.
     (list (concat                      ; ":[ \t]*"
	    "\\<\\(access[ \t]+all\\|access[ \t]+constant\\|access\\|constant\\|in[ \t]+reverse\\|\\|in[ \t]+out\\|in\\|out\\)\\>"
	    "[ \t]*"
	    "\\(\\sw+\\(\\.\\sw*\\)*\\)?")
	   '(1 font-lock-keyword-face nil t) '(2 font-lock-type-face nil t))

     ;;
     ;; Main keywords, except those treated specially below.
     (concat "\\<"
	     (regexp-opt
	      '("abort" "abs" "abstract" "accept" "access" "aliased" "all"
		"and" "array" "at" "begin" "case" "declare" "delay" "delta"
		"digits" "do" "else" "elsif" "entry" "exception" "exit" "for"
		"generic" "if" "in" "interface" "is" "limited" "loop" "mod" "not"
		"null" "or" "others" "overriding" "private" "protected" "raise"
		"range" "record" "rem" "renames" "requeue" "return" "reverse"
		"select" "separate" "synchronized" "tagged" "task" "terminate"
		"then" "until" "when" "while" "with" "xor") t)
	     "\\>")
     ;;
     ;; Anything following end and not already fontified is a body name.
     '("\\<\\(end\\)\\>\\([ \t]+\\)?\\(\\(\\sw\\|[_.]\\)+\\)?"
       (1 font-lock-keyword-face) (3 font-lock-function-name-face nil t))
     ;;
     ;; Keywords followed by a type or function name.
     (list (concat "\\<\\("
		   "new\\|of\\|subtype\\|type"
		   "\\)\\>[ \t]*\\(\\sw+\\(\\.\\sw*\\)*\\)?[ \t]*\\((\\)?")
	   '(1 font-lock-keyword-face)
	   '(2 (if (match-beginning 4)
		   font-lock-function-name-face
		 font-lock-type-face) nil t))
     ;;
     ;; Keywords followed by a (comma separated list of) reference.
     ;; Note that font-lock only works on single lines, thus we can not
     ;; correctly highlight a with_clause that spans multiple lines.
     (list (concat "\\<\\(goto\\|raise\\|use\\|with\\)"
		   "[ \t]+\\([a-zA-Z0-9_., \t]+\\)\\W")
	   '(1 font-lock-keyword-face) '(2 font-lock-reference-face nil t))

     ;;
     ;; Goto tags.
     '("<<\\(\\sw+\\)>>" 1 font-lock-reference-face)

     ;; Highlight based-numbers (R. Reagan <robin-reply@reagans.org>)
     (list "\\([0-9]+#[0-9a-fA-F_]+#\\)" '(1 font-lock-constant-face t))

     ;; Ada unnamed numerical constants
     (list "\\W\\([-+]?[0-9._]+\\)\\>" '(1 font-lock-constant-face))

     ))
  "Default expressions to highlight in Ada mode.")

;; --------------------------------------------------------
;; Global initializations
;; --------------------------------------------------------

;; FIXME: move these to the corresponding variable inits?
(ada-create-keymap)
(ada-create-menu)
(ada-create-syntax-table)
(ada-add-extensions ".ads" ".adb")

;; Setup auto-loading of the other Ada mode files.
;; FIXME: add some here?

(provide 'ada-mode)

;;; end of file
