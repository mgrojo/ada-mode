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

;;; ---- end of user configurable variables; see other ada-*.el files for more

;;; keymap and menus

(defvar ada-mode-map
  (let ((map (make-sparse-keymap)))
    ;; C-c <letter> are reserved for users

    (define-key map "\C-c\C-c" 'compile)
    (define-key map "\C-c\C-n" 'ada-make-subprogram-body)
    (define-key map "\C-c\C-o" 'ff-find-other-file)
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
    ["Other File"              ff-find-other-file  t]
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
    ))

(defvar ada-mode-abbrev-table nil
  "Local abbrev table for Ada mode.")

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
    (modify-syntax-entry ?:  "." table)
    (modify-syntax-entry ?\; "." table)
    (modify-syntax-entry ?&  "." table)
    (modify-syntax-entry ?\|  "." table)
    (modify-syntax-entry ?+  "." table)
    (modify-syntax-entry ?*  "." table)
    (modify-syntax-entry ?/  "." table)
    (modify-syntax-entry ?=  "." table)
    (modify-syntax-entry ?<  "." table)
    (modify-syntax-entry ?>  "." table)
    (modify-syntax-entry ?. "." table)
    (modify-syntax-entry ?\' "." table); attribute; see ada-syntax-propertize for character literal
    (modify-syntax-entry ?-  ". 12" table); operator; see ada-syntax-propertize for double hyphen as comment
    (modify-syntax-entry ?#  "$" table); based number

    ;; and \f and \n end a comment
    (modify-syntax-entry ?\f  ">   " table)
    (modify-syntax-entry ?\n  ">   " table)

    (modify-syntax-entry ?_ "_" table); symbol constituents

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
displaying the menu.  When a function from the menu is called,
point is where the mouse button was clicked."
  (interactive "e")

  ;; don't let context menu commands deactivate the mark (which would
  ;; hide the region in transient-mark-mode), even if they normally
  ;; would. FIXME (later, when testing menu): why is this a good idea?
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
    (popup-menu ada-contextual-menu)

    ;; FIXME (later, when testing menu): is this necessary? what do context menus do by default?
    (set-buffer (cadr ada-contextual-menu-last-point))
    (goto-char (car ada-contextual-menu-last-point))
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

(defvar ada-other-file-alist
  '(("\\.ads$" (".adb"))
    ("\\.adb$" (".ads")))
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

(defun ada-set-point-accordingly ()
  "Move to the function declaration that was set by `ff-which-function-are-we-in'."
  (if ff-function-name
      (progn
	(goto-char (point-min))
	;; FIXME (later, when testing this): don't have
	;; ada-search-ignore-string-comment anymore; move this to
	;; ada-indent to be more accurate?
	(unless (ada-search-ignore-string-comment
		 (concat ff-function-name "\\b") nil)
	  (goto-char (point-min))))))

(defun ada-get-body-name (&optional spec-name)
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

;; ---------------------------------------------------
;;    support for font-lock.el

(defconst ada-font-lock-name-regexp
  "\\(\\(?:\\sw\\|[_.]\\)+\\)")

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
       ada-font-lock-name-regexp "?")))
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
	  ada-font-lock-name-regexp "?")
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
	  ada-font-lock-name-regexp
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
	  ada-font-lock-name-regexp "?")
     '(1 font-lock-keyword-face) '(2 font-lock-function-name-face nil t))

   ;; keywords followed by a name that should be in type-face if not already fontified
   (list (concat
	  "\\<\\("
	  "is"
	  "\\)\\>[ \t]*"
	  ada-font-lock-name-regexp "?")
     '(1 font-lock-keyword-face) '(2 font-lock-type-face nil t))

   ;; Keywords followed by a name that could be a type or a function (generic instantiation).
   (list (concat
	  "\\<\\("
	  "new"
	  "\\)\\>[ \t]*"
	  ada-font-lock-name-regexp "?[ \t]*\\((\\)?")
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
   	  "\\(\\(?:\\sw\\|[_., \t]\\)+\\>\\)?"; ada-font-lock-name-regexp, plus ", \t"
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
	 ((?\_ . "w")); treat underscore as a word component
	 beginning-of-line))

  ;; AdaCore standard style (enforced by -gnaty) requires two spaces
  ;; after '--' in comments; this makes it easier to distinguish
  ;; special comments that have something else after '--'
  (set (make-local-variable 'comment-padding) "  ")

  (set (make-local-variable 'ff-other-file-alist)
       'ada-other-file-alist)
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
(defun ada-find-other-file ()
  (interactive)
  (ff-find-other-file))
(defun ada-which-function-are-we-in () "")
(defvar ada-case-exception-file nil)

;; load indent engine first; compilers may need to know which is being
;; used (for preprocessor keywords, for example).
(unless (featurep 'ada-indent-engine)
  (require 'ada-indent)); FIXME: rename to ada-smie

(unless (featurep 'ada-compiler)
  (require 'ada-gnat))

(provide 'ada-mode)

;;; end of file
