;;; ada-mode.el --- major-mode for editing Ada sources
;;
;;; Copyright (C) 1994, 1995, 1997 - 2013  Free Software Foundation, Inc.

;; Author: Stephen Leake <stephen_leake@member.fsf.org>
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
;;
;; Emacs should enter Ada mode automatically when you load an Ada
;; file, based on the file extension.  The default extensions for Ada
;; files are .ads, .adb; use ada-add-extensions to add other
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
;;
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
;; A complete rewrite by Rolf Ebert <ebert@inf.enst.fr> and Markus
;; Heritsch <Markus.Heritsch@studbox.uni-stuttgart.de> was done at
;; some point.  Some ideas from the Ada mode mailing list have been
;; added.  Some of the functionality of L. Slater's mode has not (yet)
;; been recoded in this new mode.
;;
;; A complete rewrite for Emacs-20 / GNAT-3.11 was done by Emmanuel
;; Briot <briot@gnat.com> at Ada Core Technologies.
;;
;; A complete rewrite, to restructure the code more orthogonally, and
;; to use smie for the indentation engine, was done in 2012 by Stephen
;; Leake <stephen_leake@stephe-leake.org>.

;;; Credits:
;;
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
;;     robin-reply@reagans.org
;;    and others for their valuable hints.

(require 'find-file nil t)
(require 'align nil t)
(require 'which-func nil t)
(require 'compile nil t)

(eval-when-compile (require 'cl-macs))

(defun ada-mode-version ()
  "Return Ada mode version."
  (interactive)
  (let ((version-string "5.00"))
    (if (called-interactively-p 'interactive)
	(message version-string)
      version-string)))

;;;;; User variables

(defvar ada-mode-hook nil
  "*List of functions to call when Ada mode is invoked.
This hook is executed after `ada-mode' is fully loaded.  This is
a good place to add Ada environment specific bindings.")

(defgroup ada nil
  "Major mode for editing and compiling Ada source in Emacs."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'languages)

(defcustom ada-auto-case t
  "*Non-nil means automatically change case of preceding word while typing.
Casing of Ada keywords is done according to `ada-case-keyword',
identifiers are Mixed_Case."
  :type 'boolean :group 'ada)
(put 'ada-auto-case 'safe-local-variable 'booleanp)

(defcustom ada-case-exception-file nil
  "*List of special casing exceptions dictionaries for identifiers.
New exceptions may be added interactively via `ada-case-create-exception'.
If an exception is defined in multiple files, the first occurance is used.

The file format is one word per line, that gives the casing to be
used for that word in Ada source code.  If the line starts with
the character *, then the exception will be used for partial
words that either start at the beginning of a word or after a _
character, and end either at the end of the word or at a _
character.  Characters after the first word are ignored, and not
preserved when the list is written back to the file."
  :type '(repeat (file))
  :group 'ada)
(put 'ada-case-exception-file 'safe-local-variable 'listp)

(defcustom ada-case-keyword 'downcase-word
  "*Function to call to adjust the case of an Ada keywords."
  :type '(choice (const downcase-word)
		 (const upcase-word))
  :group 'ada)
(put 'ada-case-keyword 'safe-local-variable 'functionp)

(defcustom ada-case-strict t
  "*If non-nil, force Mixed_Case for identifiers.
Otherwise, allow UPPERCASE for identifiers."
  :type 'boolean
  :group 'ada)
(put 'ada-case-strict 'safe-local-variable 'booleanp)

(defcustom ada-language-version 'ada2012
  "*Ada language version; one of `ada83', `ada95', `ada2005'.
Only affects the keywords to highlight."
  :type '(choice (const ada83) (const ada95) (const ada2005) (const ada2012)) :group 'ada)
(put 'ada-language-version 'safe-local-variable 'symbolp)

(defcustom ada-popup-key '[down-mouse-3]
  ;; FIXME (later, when testing menu): don't need a var for this; user can just bind a key
  "*Key used for binding the contextual menu.
If nil, no contextual menu is available."
  :type '(restricted-sexp :match-alternatives (stringp vectorp))
  :group 'ada)

(defcustom ada-fill-comment-prefix "-- "
  "Comment fill prefix."
  :type 'string
  :group 'ada)

(defcustom ada-fill-comment-postfix " --"
  "Comment fill postfix."
  :type 'string
  :group 'ada)

;;;;; end of user variables

(defconst ada-symbol-end
  ;; we can't just add \> here; that might match _ in a user modified ada-mode-syntax-table
  "\\([ \t]+\\|$\\)"
  "Regexp to add to symbol name in `ada-which-function'.")

(defvar-local ada-compiler nil
  "Symbol indicating which compiler is being used with the current buffer.")

;;;; keymap and menus

(defvar-local ada-mode-map
  (let ((map (make-sparse-keymap)))
    ;; C-c <letter> are reserved for users

    ;; global-map has C-x ` 'next-error
    (define-key map [return]   'ada-indent-newline-indent)
    (define-key map ["\C-c" tab] 'ada-indent-region)
    (define-key map "\C-c`"    'ada-show-secondary-error)
    (define-key map "\C-c\C-a" 'ada-align)
    (define-key map "\C-c\C-b" 'ada-make-subprogram-body)
    (define-key map "\C-c\C-c" 'compile)
    (define-key map "\C-c\C-d" 'ada-goto-declaration)
    (define-key map "\C-c\M-d" 'ada-goto-declaration-parent)
    (define-key map "\C-c\C-n" 'ada-next-statement-keyword)
    (define-key map "\C-c\C-o" 'ada-find-other-file)
    (define-key map "\C-c\M-o" 'ada-find-other-file-noset)
    (define-key map "\C-c\C-p" 'ada-prev-statement-keyword)
    (define-key map "\C-c\C-r" 'ada-show-references)
    (define-key map "\C-c\C-t" 'ada-case-read-all-exceptions)
    (define-key map "\C-c\C-w" 'ada-case-adjust-at-point)
    (define-key map "\C-c\C-y" 'ada-case-create-exception)
    (define-key map "\C-c\C-\M-y" (lambda () (ada-case-create-exception nil nil t)))
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
    ["Next compilation error"     next-error                t]
    ["Show secondary error"       ada-show-secondary-error  t]
    ["------"        nil nil]
    ["Other File"                 ada-find-other-file       t]
    ["Other File don't find decl" ada-find-other-file-noset t]
    ["Goto Declaration/Body"      ada-goto-declaration      t]
    ["Goto parent declaration"    ada-goto-declaration-parent t]
    ["Show references"            ada-show-references       t]
    ["Toggle show parser errors"  wisi-toggle-show-parser-errors t]
    ["------"        nil nil]
    ("Edit"
     ["Indent Line"                 indent-for-tab-command  t]
     ["Indent Lines in Selection or current statement"   ada-indent-region       t]
     ["Indent Lines in File"        (indent-region (point-min) (point-max))  t]
     ["Align"                       ada-align               t]
     ["Comment Selection"           comment-region               t]
     ["Uncomment Selection"         (comment-region t) t]
     ["Fill Comment Paragraph"         ada-fill-comment-paragraph           t]
     ["Fill Comment Paragraph Justify" (ada-fill-comment-paragraph 'full)   t]
     ["Fill Comment Paragraph Postfix" (ada-fill-comment-paragraph 'full t) t]
     ["---" nil nil]
     ["Make body for subprogram"    ada-make-subprogram-body     t]
     )
    ("Case Exceptions"
     ["Create full exception"    'ada-case-create-exception t]
     ["Create partial exception" (lambda () (interactive) (ada-case-create-exception nil nil t)) t]
     )
    ))

(defun ada-indent-newline-indent ()
  "insert a newline, indent the old and new lines."
  (interactive "*")
  ;; point may be in the middle of a word, so insert newline first,
  ;; then go back and indent.
  (newline)
  (forward-char -1)
  (funcall indent-line-function)
  (forward-char 1)
  (funcall indent-line-function))

(defvar-local ada-indent-region nil
  "Function to indent the current region, or the current statement/declaration if region is not active.
Function is called with no arguments.
Supplied by indentation engine parser.")

(defun ada-indent-region ()
  "Return t if point is inside the parameter-list of a subprogram declaration."
  (interactive)
  (when ada-indent-region
    (funcall ada-indent-region)))

;;;; abbrev, align

(defvar ada-mode-abbrev-table nil
  "Local abbrev table for Ada mode.")

(defvar ada-align-rules
  '((ada-declaration-assign
     (regexp  . "[^:]\\(\\s-*\\)\\(:\\)[^:]")
     (valid   . (lambda () (ada-align-valid)))
     (repeat . t)
     (modes   . '(ada-mode)))
    (ada-associate
     (regexp  . "[^=]\\(\\s-*\\)\\(=>\\)")
     (valid   . (lambda () (ada-align-valid)))
     (modes   . '(ada-mode)))
    (ada-comment
     (regexp  . "\\(\\s-*\\)--")
     (modes   . '(ada-mode)))
    (ada-use
     (regexp  . "\\(\\s-*\\)\\<\\(use\\s-\\)")
     (valid   . (lambda () (ada-align-valid)))
     (modes   . '(ada-mode)))
    (ada-at
     (regexp . "\\(\\s-+\\)\\(at\\)\\>")
     (valid   . (lambda () (ada-align-valid)))
     (modes . '(ada-mode))))
  "Rules to use to align different lines.")

(defun ada-align-valid ()
  "See use in `ada-align-rules'."
  (save-excursion
    ;; we don't put "when (match-beginning 2)" here; missing a match
    ;; is a bug in the regexp.
    (goto-char (match-beginning 2))
    (not (ada-in-string-or-comment-p))))

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
     "private\\|"
     "record\\|"
     "return\\|"
     "type\\|"
     "when"
     "\\)\\>\\)"))
  "See the variable `align-region-separate' for more information.")

(defun ada-align ()
  "If region is active, apply 'align'. If not, attempt to align
current construct."
  (interactive)
  (if (use-region-p)
      (progn
        (align (region-beginning) (region-end))
        (deactivate-mark))

    ;; else see if we are in a construct we know how to align
    (cond
     ((ada-in-paramlist-p)
        (ada-format-paramlist))

     (t
      (align-current))
     )))

(defvar-local ada-in-paramlist-p nil
  "Function to return t if point is inside the parameter-list of a subprogram declaration.
Function is called with no arguments.
Supplied by indentation engine parser.")

(defun ada-in-paramlist-p ()
  "Return t if point is inside the parameter-list of a subprogram declaration."
  (when ada-in-paramlist-p
    (funcall ada-in-paramlist-p)))

(defun ada-format-paramlist ()
  "Reformat the parameter list point is in."
  (interactive)
  (ada-goto-open-paren)
  (funcall indent-line-function); so new list is indented properly

  (let* ((inibit-modification-hooks t)
	 (begin (point))
	 (delend (progn (forward-sexp) (point))); just after matching closing paren
	 (end (progn (backward-char) (forward-comment (- (point))) (point))); end of last parameter-declaration
	 (multi-line (> end (save-excursion (goto-char begin) (line-end-position))))
	 (paramlist (ada-scan-paramlist (1+ begin) end)))

    (when paramlist
      ;; delete the original parameter-list
      (delete-region begin delend)

      ;; insert the new parameter-list
      (goto-char begin)
      (if multi-line
	  (ada-insert-paramlist-multi-line paramlist)
	(ada-insert-paramlist-single-line paramlist)))
    ))

(defvar-local ada-scan-paramlist nil
  "Function to scan a region, return a list of subprogram parameter declarations (in inverse declaration order).
Function is called with two args BEGIN END (the region).
Each parameter declaration is represented by a list
'((identifier ...) in-p out-p not-null-p access-p constant-p protected-p type default)."
  ;; mode is 'in | out | in out | [not null] access [constant | protected]'
  ;; IMPROVEME: handle single-line trailing comments, or longer comments, in paramlist?
  )

(defun ada-scan-paramlist (begin end)
  (when ada-scan-paramlist
    (funcall ada-scan-paramlist begin end)))

(defun ada-insert-paramlist-multi-line (paramlist)
  "Insert a multi-line formatted PARAMLIST in the buffer."
  (let ((i (length paramlist))
	param
	j
	len
	(ident-len 0)
	(type-len 0)
	(in-p nil)
	(out-p nil)
	(not-null-p nil)
	(access-p nil)
	ident-col
	colon-col
	out-col
	type-col
	default-col)

    ;; accumulate info across all params
    (while (not (zerop i))
      (setq i (1- i))
      (setq param (nth i paramlist))

      ;; identifier list
      (setq len 0
	    j   0)
      (mapc (lambda (ident)
	      (setq j (1+ j))
	      (setq len (+ len (length ident))))
	    (nth 0 param))
      (setq len (+ len (* 2 (1- j)))); space for commas
      (setq ident-len (max ident-len len))

      ;; we align the defaults after the types that have defaults, not after all types.
      ;; "constant", "protected" are treated as part of 'type'
      (when (nth 8 param)
	(setq type-len
	      (max type-len
		   (+ (length (nth 7 param))
		      (if (nth 5 param) 10 0); "constant "
		      (if (nth 6 param) 10 0); protected
		      ))))

      (setq in-p (or in-p (nth 1 param)))
      (setq out-p (or out-p (nth 2 param)))
      (setq not-null-p (or not-null-p (nth 3 param)))
      (setq access-p (or access-p (nth 4 param)))
      )

    (unless (save-excursion (skip-chars-backward " \t") (bolp))
      ;; paramlist starts on same line as subprogram identifier; clean up whitespace
      (end-of-line)
      (delete-char (- (skip-syntax-backward " ")))
      (insert " "))

    (insert "(")

    ;; compute columns.
    (setq ident-col (current-column))
    (setq colon-col (+ ident-col ident-len 1))
    (setq out-col (+ colon-col (if in-p 5 0))); ": in "
    (setq type-col
	  (+ colon-col
	     (cond
	      (not-null-p 18);    ": not null access "
	      (access-p 9);        ": access"
	      ((and in-p out-p) 9); ": in out "
	      (out-p 6);           ": out "
	      (in-p 5);            ": in "
	      (t 2))));           ": "

    (setq default-col (+ 1 type-col type-len))

    (setq i (length paramlist))
    (while (not (zerop i))
      (setq i (1- i))
      (setq param (nth i paramlist))

      ;; insert identifiers, space and colon
      (mapc (lambda (ident)
	      (insert ident)
	      (insert ", "))
	    (nth 0 param))
      (delete-char -2); last ", "
      (indent-to colon-col)
      (insert ": ")

      (when (nth 1 param)
	(insert "in "))

      (when (nth 2 param)
	(indent-to out-col)
	(insert "out "))

      (when (nth 3 param)
	(insert "not null "))

      (when (nth 4 param)
	(insert "access "))

      (indent-to type-col)
      (when (nth 5 param)
	(insert "constant "))
      (when (nth 6 param)
	(insert "protected "))
      (insert (nth 7 param)); type

      (when (nth 8 param); default
	(indent-to default-col)
	(insert ":= ")
	(insert (nth 8 param)))

      (if (zerop i)
	  (insert ")")
	(insert ";")
	(newline)
	(indent-to ident-col))
      )
    ))

(defun ada-insert-paramlist-single-line (paramlist)
  "Insert a single-line formatted PARAMLIST in the buffer."
  (let ((i (length paramlist))
	param)

    ;; clean up whitespace
    (skip-syntax-forward " ")
    (delete-char (- (skip-syntax-backward " ")))
    (insert " (")

    (setq i (length paramlist))
    (while (not (zerop i))
      (setq i (1- i))
      (setq param (nth i paramlist))

      ;; insert identifiers, space and colon
      (mapc (lambda (ident)
	      (insert ident)
	      (insert ", "))
	    (nth 0 param))
      (delete-char -2); last ", "

      (insert " : ")

      (when (nth 1 param)
	(insert "in "))

      (when (nth 2 param)
	(insert "out "))

      (when (nth 3 param)
	(insert "not null "))

      (when (nth 4 param)
	(insert "access "))

      (when (nth 5 param)
	(insert "constant "))
      (when (nth 6 param)
	(insert "protected "))
      (insert (nth 7 param)); type

      (when (nth 8 param); default
	(insert " := ")
	(insert (nth 8 param)))

      (if (zerop i)
	  (if (= (char-after) ?\;)
	      (insert ")")
	    (insert ") "))
	(insert "; "))
      )
    ))

;;;; context menu

(defvar ada-context-menu-last-point nil)
(defvar ada-context-menu-on-identifier nil)
(defvar ada-context-menu nil)

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

;;;; auto-casing

(defvar ada-case-full-exceptions '()
  "Alist of words (entities) that have special casing, built from
`ada-case-exception-file' full word exceptions. Indexed by
properly cased word; value is t.")

(defvar ada-case-partial-exceptions '()
  "Alist of partial words that have special casing, built from
`ada-case-exception-file' partial word exceptions. Indexed by
properly cased word; value is t.")

(defun ada-case-save-exceptions (full-exceptions partial-exceptions file-name)
  "Save FULL-EXCEPTIONS, PARTIAL-EXCEPTIONS to the file FILE-NAME."
  (with-temp-file (expand-file-name file-name)
    (mapc (lambda (x) (insert (car x) "\n"))
	  (sort (copy-sequence full-exceptions)
		(lambda(a b) (string< (car a) (car b)))))
    (mapc (lambda (x) (insert "*" (car x) "\n"))
	  (sort (copy-sequence partial-exceptions)
		(lambda(a b) (string< (car a) (car b)))))
    ))

(defun ada-case-read-exceptions (file-name)
  "Read the content of the casing exception file FILE-NAME.
Return (cons full-exceptions partial-exceptions)."
  (setq file-name (expand-file-name (substitute-in-file-name file-name)))
  (unless (file-readable-p file-name)
    (error "'%s' is not a readable file." file-name))

  (let (full-exceptions partial-exceptions word)
    (with-temp-buffer
      (insert-file-contents file-name)
      (while (not (eobp))

	(setq word (buffer-substring-no-properties
		    (point) (save-excursion (skip-syntax-forward "w_") (point))))

	(if (char-equal (string-to-char word) ?*)
	    ;; partial word exception
	    (progn
	      (setq word (substring word 1))
	      (unless (assoc-string word partial-exceptions t)
		(add-to-list 'partial-exceptions (cons word t))))

	  ;; full word exception
	  (unless (assoc-string word full-exceptions t)
	    (add-to-list 'full-exceptions (cons word t))))

	(forward-line 1))
      )
    (cons full-exceptions partial-exceptions)))

(defun ada-case-merge-exceptions (result new)
  "Merge NEW exeptions into RESULT."
  (dolist (item new)
    (unless (assoc-string (car item) result t)
      (add-to-list 'result item)))
  result)

(defun ada-case-merge-all-exceptions (exceptions)
  "Merge EXCEPTIONS into `ada-case-full-exceptions', `ada-case-partial-exceptions'."
  (setq ada-case-full-exceptions (ada-case-merge-exceptions ada-case-full-exceptions (car exceptions)))
  (setq ada-case-partial-exceptions (ada-case-merge-exceptions ada-case-partial-exceptions (cdr exceptions))))

(defun ada-case-read-all-exceptions ()
  "Read case exceptions from all files in `ada-case-exception-file',
replacing current values of `ada-case-full-exceptions', `ada-case-partial-exceptions'."
  (interactive)
  (setq ada-case-full-exceptions '()
	ada-case-partial-exceptions '())

  (dolist (file ada-case-exception-file)
    (ada-case-merge-all-exceptions (ada-case-read-exceptions file)))
  )

(defun ada-case-add-exception (word exceptions)
  "Add case exception WORD to EXCEPTIONS, replacing current entry, if any."
  (if (assoc-string word exceptions t)
      (setcar (assoc-string word exceptions t) word)
    (add-to-list 'exceptions (cons word t)))
  exceptions)

(defun ada-case-create-exception (&optional word file-name partial)
  "Define WORD as an exception for the casing system, save it in FILE-NAME.
If PARTIAL is non-nil, create a partial word exception.
WORD defaults to the active region, or the word at point.
When non-interactive, FILE-NAME defaults to the first file in `ada-case-exception-file';
when interactive, user is prompted to choose a file from `ada-case-exception-file'."
  (interactive)
  (setq file-name
	(cond
	 (file-name file-name)
	 ((stringp ada-case-exception-file)
	  ada-case-exception-file)
	 ((and
	   ada-case-exception-file;; nil is a list
	   (listp ada-case-exception-file))
	  (if (called-interactively-p 'any)
	      ;; FIXME (later): not tested yet
	      (completing-read "case exception file: " ada-case-exception-file
			       nil ;; predicate
			       t   ;; require-match
			       nil ;; initial-input
			       nil ;; hist
			       (car ada-case-exception-file) ;; default
			       )
	    (car ada-case-exception-file)))
	 (t
	  (error
	   "No exception file specified. See variable `ada-case-exception-file'"))))

  (unless word
    (if (use-region-p)
	(setq word (buffer-substring-no-properties (region-beginning) (region-end)))
      (save-excursion
	(skip-syntax-backward "w_")
	(setq word
	      (buffer-substring-no-properties
	       (point)
	       (progn (skip-syntax-forward "w_") (point))
	       )))))

  (let* ((exceptions (ada-case-read-exceptions file-name))
	 (full-exceptions (car exceptions))
	 (partial-exceptions (cdr exceptions)))

    (cond
     ((null partial)
      (setq ada-case-full-exceptions (ada-case-add-exception word ada-case-full-exceptions))
      (setq full-exceptions (ada-case-add-exception word full-exceptions)))

     (t
      (setq ada-case-partial-exceptions (ada-case-add-exception word ada-case-partial-exceptions))
      (setq partial-exceptions (ada-case-add-exception word partial-exceptions)))
     )
    (ada-case-save-exceptions full-exceptions partial-exceptions file-name)
    (message "created %s case exception '%s' in file '%s'"
	     (if partial "partial" "full")
	     word
	     file-name)
    ))

(defun ada-in-numeric-literal-p ()
  "Return t if point is after a prefix of a numeric literal."
  (looking-back "\\([0-9]+#[0-9a-fA-F_]+\\)"))

(defun ada-after-keyword-p ()
  "Return non-nil if point is after an element of `ada-keywords'."
  (let ((word (buffer-substring-no-properties
	       (save-excursion (skip-syntax-backward "w_") (point))
	       (point))))
    (member (downcase word) ada-keywords)))

(defun ada-case-adjust-identifier ()
  "Adjust case of the previous word as an identifier.
Uses Mixed_Case, with exceptions defined in
`ada-case-full-exceptions', `ada-case-partial-exceptions'."
  (interactive)
  (save-excursion
    (let ((end   (point-marker))
	  (start (progn (skip-syntax-backward "w_") (point)))
	  match
	  next
	  (done nil))

      (if (setq match (assoc-string (buffer-substring-no-properties start end) ada-case-full-exceptions t))
	  ;; full word exception
	  (progn
	    ;; 'save-excursion' puts a marker at 'end'; if we do
	    ;; 'delete-region' first, it moves that marker to 'start',
	    ;; then 'insert' inserts replacement text after the
	    ;; marker, defeating 'save-excursion'. So we do 'insert' first.
	    (insert (car match))
	    (delete-region (point) end))

	;; else apply Mixed_Case and partial-exceptions
	(if ada-case-strict
	    (downcase-region start end))
	(while (not done)
	  (setq next
		(or
		 (save-excursion (when (search-forward "_" end t) (point-marker)))
		 (copy-marker (1+ end))))

	  (if (setq match (assoc-string (buffer-substring-no-properties start (1- next))
					ada-case-partial-exceptions t))
	      (progn
		;; see comment above at 'full word exception' for why
		;; we do insert first.
		(insert (car match))
		(delete-region (point) (1- next)))

	    ;; else upcase first char
	    (insert-char (upcase (following-char)) 1)
	    (delete-char 1))

	  (goto-char next)
	  (if (< (point) end)
	      (setq start (point))
	    (setq done t))
	)))))

(defun ada-case-adjust (&optional typed-char in-comment)
  "Adjust the case of the word before point.
When invoked interactively, TYPED-CHAR must be
`last-command-event', and it must not have been inserted yet.
If IN-COMMENT is non-nil, adjust case of words in comments."
  (when (not (bobp))
    (when (save-excursion
	    (forward-char -1); back to last character in word
	    (and (not (bobp))
		 (eq (char-syntax (char-after)) ?w); it can be capitalized

		 (not (and (eq typed-char ?')
			   (eq (char-before (point)) ?'))); character literal

		 (or in-comment
		     (not (ada-in-string-or-comment-p)))
		 ;; we sometimes want to capitialize an Ada identifier
		 ;; referenced in a comment, via
		 ;; ada-adjust-case-at-point.

		 (not (ada-in-numeric-literal-p))
		 ))

      (cond
       ;; Some attributes are also keywords, but captialized as
       ;; attributes. So check for attribute first.
       ((save-excursion
	 (skip-syntax-backward "w_")
	 (eq (char-before) ?'))
	(ada-case-adjust-identifier))

       ((and
	 (not (eq typed-char ?_))
	 (ada-after-keyword-p))
	(funcall ada-case-keyword -1))

       (t (ada-case-adjust-identifier))
       ))
    ))

(defun ada-case-adjust-at-point (&optional in-comment)
  "Adjust case of word at point, move to end of word.
With prefix arg, adjust case even if in comment."
  (interactive "P")
  (when
      (and (not (eobp))
	   (memq (char-syntax (char-after)) '(?w ?_)))
    (skip-syntax-forward "w_"))
  (ada-case-adjust nil in-comment))

(defun ada-case-adjust-region (begin end)
  "Adjust case of all words in region BEGIN END."
  (interactive "r")
  (narrow-to-region begin end)
  (save-excursion
    (goto-char begin)
    (while (not (eobp))
      (forward-comment (point-max))
      (skip-syntax-forward "^w_")
      (skip-syntax-forward "w_")
      (unless (eobp)
	(ada-case-adjust))))
  (widen))

(defun ada-case-adjust-buffer ()
  "Adjust case of current buffer."
  (interactive)
  (ada-case-adjust-region (point-min) (point-max)))

(defun ada-case-adjust-interactive (arg)
  "Adjust the case of the previous word, and process the character just typed.
To be bound to keys that should cause auto-casing.
ARG is the prefix the user entered with \\[universal-argument]."
  (interactive "P")

  ;; character typed has not been inserted yet
  (let ((lastk last-command-event))

    (cond
     ((eq lastk ?\n)
      (ada-case-adjust lastk)
      (funcall ada-lfd-binding))

     ((eq lastk ?\r)
      (ada-case-adjust lastk)
      (funcall ada-ret-binding))

     (t
      (ada-case-adjust lastk)
      (self-insert-command (prefix-numeric-value arg)))
     )
  ))

(defvar ada-ret-binding nil)
(defvar ada-lfd-binding nil)

(defun ada-case-activate-keys ()
  "Modify the key bindings for all the keys that should adjust casing."
  (interactive)
  ;; We can't use post-self-insert-hook for \n, \r, because they are
  ;; not self-insert.  So we make ada-mode-map buffer local, and don't
  ;; call this function if ada-auto-case is off. That means
  ;; ada-auto-case cannot be changed after an Ada buffer is created.

  ;; The 'or ...' is there to be sure that the value will not be
  ;; changed again when Ada mode is called more than once, since we
  ;; are rebinding the keys.
  (or ada-ret-binding (setq ada-ret-binding (key-binding "\C-M")))
  (or ada-lfd-binding (setq ada-lfd-binding (key-binding "\C-j")))

  (mapcar (function
	   (lambda(key)
	     (define-key
	       ada-mode-map
	       (char-to-string key)
	       'ada-case-adjust-interactive)))
	  '( ?_ ?% ?& ?* ?( ?) ?- ?= ?+
		?| ?\; ?: ?' ?\" ?< ?, ?. ?> ?/ ?\n 32 ?\r ))
  )

;;;; project files

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

(defvar ada-prj-current-file nil
  "Current Emacs Ada project file.")

(defvar ada-prj-current-project nil
  "Current Emacs Ada mode project; a plist.")

(defun ada-prj-get (prop &optional plist)
  "Return value of PROP in PLIST.
Optional PLIST defaults to `ada-prj-current-project'."
  (plist-get (or plist ada-prj-current-project) prop))

(defun ada-require-project-file ()
  (unless ada-prj-current-file
    (error "no Emacs Ada project file specified")))

(defvar ada-prj-default-function nil
  "Alist indexed by `ada-compiler' of compiler-specific functions
to return default Emacs Ada project properties for the current
buffer.  Called with one argument; the compiler-independent
default properties list.  Function should add to the list and
return it.")

(defun ada-prj-default ()
  "Return the default project properties list with the current buffer as main.
Calls `ada-prj-default-function' to extend the list with
compiler-specific objects."

  (let*
      ((file (buffer-file-name nil))
       (props
	(list
	 ;; variable name alphabetical order
	 'ada-compiler    (default-value 'ada-compiler)
	 'casing          (if (listp (default-value 'ada-case-exception-file))
			      (default-value 'ada-case-exception-file)
			    (list (default-value 'ada-case-exception-file)))
	 'main            (if file
			      (file-name-nondirectory
			       (file-name-sans-extension file))
			    "")
	 'path_sep        path-separator;; prj variable so users can override it for their compiler
	 'proc_env        process-environment
	 'run_cmd         "./${main}"
	 'src_dir         (list ".")
	 )))
    (if ada-prj-default-function
	(funcall ada-prj-default-function props)
      props)
    ))

(defvar ada-prj-parser-alist
  (list
   (cons "adp" 'ada-prj-parse-file-1))
  "Alist of parsers for project files.
Default provides the minimal Ada mode parser; compiler support
code may add other parsers.  Parser is called with two arguments;
the project file name and the current project property
list. Parser must modify or add to the property list and return it.")

;; This autoloaded because it is often used in Makefiles, and thus
;; will be the first ada-mode function executed.
;;;###autoload
(defun ada-parse-prj-file (prj-file)
  "Read Emacs Ada or compiler-specific project file PRJ-FILE, set project properties in `ada-prj-alist'."
  ;; Not called ada-prj-parse-file for Ada mode 4.01 compatibility
  (let ((project (ada-prj-default))
	(parser (cdr (assoc (file-name-extension prj-file) ada-prj-parser-alist))))

    (setq prj-file (expand-file-name prj-file))

    (if parser
	;; parser may reference the "current project", so bind that now.
	(let ((ada-prj-current-project project)
	      (ada-prj-current-file prj-file))
	  (setq project (funcall parser prj-file project)))
      (error "no project file parser defined for '%s'" prj-file))

    ;; Store the project properties
    (if (assoc prj-file ada-prj-alist)
	(setcdr (assoc prj-file ada-prj-alist) project)
      (add-to-list 'ada-prj-alist (cons prj-file project)))

    ;; (ada-xref-update-project-menu) FIXME (later): implement

    ;; return t for interactive use
    t))

(defvar ada-prj-parse-file-ext nil
"Alist indexed by `ada-compiler' of compiler-specific functions to process one Ada project property.
Called with three arguments; the property name, property value,
and project properties list. Function should add to or modify the
properties list and return it, or return nil if the name is not
recognized.")

(defvar ada-prj-parse-file-final nil
  "Alist indexed by `ada-compiler' of compiler-specific function to finish processing Ada project properties.
Called with one argument; the project properties list. Function
should add to or modify the list and return it.")

(defun ada-prj-parse-file-1 (prj-file project)
  "Parse the Ada mode project file PRJ-FILE, set project properties in PROJECT.
Return new value of PROJECT."
  (let (;; fields that are lists or that otherwise require special processing
	casing comp_cmd make_cmd obj_dir run_cmd src_dir
	tmp-prj
	(parse-file-ext (cdr (assoc ada-compiler ada-prj-parse-file-ext)))
	(parse-file-final (cdr (assoc ada-compiler ada-prj-parse-file-final))))

    (with-current-buffer (find-file-noselect prj-file)
      (goto-char (point-min))

      ;; process each line
      (while (not (eobp))

	;; ignore lines that don't have the format "name=value", put
	;; 'name', 'value' in match-string.
	(when (looking-at "^\\([^=\n]+\\)=\\(.*\\)")
	  (cond
	   ;; variable name alphabetical order
	   ((string= (match-string 1) "casing")
	    (add-to-list 'casing
			 (expand-file-name
			  (substitute-in-file-name (match-string 2)))))

	   ;; IMPROVEME: handle 'compilation-error-regexp-alist; set to
	   ;; nil, let user add others in project file. Assumes other
	   ;; Makefiles/projects will do the same. Or use per-project
	   ;; compilation buffer.

	   ((string= (match-string 1) "comp_cmd")
	    (add-to-list 'comp_cmd (match-string 2)))

	   ((string= (match-string 1) "make_cmd")
	    (add-to-list 'make_cmd (match-string 2)))

	   ((string= (match-string 1) "obj_dir")
	    (add-to-list 'obj_dir
			 (file-name-as-directory
			  (expand-file-name (match-string 2)))))

	   ((string= (match-string 1) "src_dir")
	    (add-to-list 'src_dir
			 (file-name-as-directory
			  (expand-file-name (match-string 2)))))

	   (t
	    (if (and parse-file-ext
		     (setq tmp-prj (funcall parse-file-ext (match-string 1) (match-string 2) project)))
		(setq project tmp-prj)

	      ;; any other field in the file is set as a project property or project environment variable
	      (if (= ?$ (elt (match-string 1) 0))
		  ;; process env var
		  (let ((env-current (plist-get project 'proc_env))
			(env-add (concat (substring (match-string 1) 1)
					 "="
					 (expand-file-name (substitute-in-file-name (match-string 2))))))
		    (add-to-list 'env-current env-add)
		    (setq project
			  (plist-put project
				     'proc_env
				     env-current)))
		;; project var
		;; FIXME: make it a list if multiple occurances
		(setq project (plist-put project
					 (intern (match-string 1))
					 (match-string 2))))
	      ))
	   ))

	(forward-line 1))

      );; done reading file

    ;; process accumulated lists
    (if casing (set 'project (plist-put project 'casing (reverse casing))))
    (if comp_cmd (set 'project (plist-put project 'comp_cmd (reverse comp_cmd))))
    (if make_cmd (set 'project (plist-put project 'make_cmd (reverse make_cmd))))
    (if obj_dir (set 'project (plist-put project 'obj_dir (reverse obj_dir))))
    (if src_dir (set 'project (plist-put project 'src_dir (reverse src_dir))))

    (when parse-file-final
      ;; parse-file-final may reference the "current project", so
      ;; re-bind that now, to include the properties set above.
      (let ((ada-prj-current-project project)
	    (ada-prj-current-file prj-file))
	(set 'project (funcall parse-file-final project))))

    project
    ))

(defun ada-select-prj-file (prj-file)
  "Select PRJ-FILE as the current project file."
  (interactive)
  (setq prj-file (expand-file-name prj-file))

  (setq ada-prj-current-project (cdr (assoc prj-file ada-prj-alist)))

  (when (null ada-prj-current-project)
    (setq ada-prj-current-file nil)
    (error "Project file '%s' was not previously parsed." prj-file))

  (setq ada-prj-current-file prj-file)

  (setq ada-compiler (ada-prj-get 'ada-compiler))

  (when (ada-prj-get 'casing)
    (setq ada-case-exception-file (ada-prj-get 'casing))
    (ada-case-read-all-exceptions))

  (setq compilation-search-path (ada-prj-get 'src_dir))

  ;; return 't', for decent display in message buffer when called interactively
  t)

;;;; syntax properties

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
    (modify-syntax-entry ?#  "w" table); based number - word syntax, since we don't need the number
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
    (modify-syntax-entry ?\\ "." table); default is escape; not correct for Ada strings
    (modify-syntax-entry ?\|  "." table)

    ;; and \f and \n end a comment
    (modify-syntax-entry ?\f  ">   " table)
    (modify-syntax-entry ?\n  ">   " table)

    (modify-syntax-entry ?_ "_" table); symbol constituents, not word.

    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    table
    )
  "Syntax table to be used for editing Ada source code.")

(defvar ada-syntax-propertize-hook nil
  "Hook run from `ada-syntax-propertize'.")

(defun ada-syntax-propertize (start end)
  "Assign `syntax-table' properties in accessible part of buffer.
In particular, character constants are set to have string syntax."
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
	     "\\|[^a-zA-Z0-9)]\\('''\\)"; 3: character constant '''
	     "\\|\\(--\\)"; 4: comment start
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
	 (match-beginning 3) (1+ (match-beginning 3)) 'syntax-table '(7 . ?'))
	(put-text-property
	 (1- (match-end 3)) (match-end 3) 'syntax-table '(7 . ?')))
       ((match-beginning 4)
	(put-text-property
	 (match-beginning 4) (match-end 4) 'syntax-table '(11 . nil)))
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

(defun ada-goto-open-paren (&optional offset parse-result)
  "Move point to innermost opening paren surrounding current point, plus OFFSET.
Throw error if not in paren.  If PARSE-RESULT is non-nil, use it
instead of calling `syntax-ppss'."
  (goto-char (+ (or offset 0) (nth 1 (or parse-result (syntax-ppss))))))

;;;; navigation within and between files

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

(defvar ada-file-name-from-ada-name nil
  "Function called with one parameter ADA-NAME, which is a library
unit name; it should return the filename in which ADA-NAME is
found.")

(defun ada-file-name-from-ada-name (ada-name)
  "Return the filename in which ADA-NAME is found."
  (funcall ada-file-name-from-ada-name ada-name))

(defvar ada-ada-name-from-file-name nil
  "Function called with one parameter FILE-NAME, which is a library
unit name; it should return the Ada name that should be found in FILE-NAME.")

(defun ada-ada-name-from-file-name (file-name)
  "Return the ada-name that should be found in FILE-NAME."
  (funcall ada-ada-name-from-file-name file-name))

(defun ada-ff-special-extract-parent ()
  (setq ff-function-name (match-string 1))
  (file-name-nondirectory
   (or
    (ff-get-file-name
     compilation-search-path
     (ada-file-name-from-ada-name ff-function-name)
     ada-spec-suffixes)
    (error "parent '%s' not found; set project file?" ff-function-name))))

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
    (file-name-nondirectory
     (or
      (ff-get-file-name
       compilation-search-path
       (ada-file-name-from-ada-name package-name)
       ada-body-suffixes)
      (error "package '%s' not found; set project file?" package-name)))))

(defun ada-ff-special-with ()
  (let ((package-name (match-string 1)))
    (setq ff-function-name (concat "^package\\s-+" package-name "\\([^_]\\|$\\)"))
    (file-name-nondirectory
     (or
      (ff-get-file-name
       compilation-search-path
       (ada-file-name-from-ada-name package-name)
       (append ada-spec-suffixes ada-body-suffixes))
      (error "package '%s' not found; set project file?" package-name)))
    ))

(defun ada-set-ff-special-constructs ()
  "Add Ada-specific pairs to `ff-special-constructs'."
  (set (make-local-variable 'ff-special-constructs) nil)
  (mapc (lambda (pair) (add-to-list 'ff-special-constructs pair))
	;; Each car is a regexp; if it matches at point, the cdr is invoked.
	;; Each cdr should set ff-function-name to a string or regexp
	;; for ada-set-point-accordingly, and return the file name
	;; (sans directory, must include suffix) to go to.
	(list
	 ;; Top level child package declaration (not body), or child
	 ;; subprogram declaration or body; go to the parent package.
	 (cons (concat "^\\(?:private[ \t]+\\)?\\(?:package\\|procedure\\|function\\)[ \t]+"
		       ada-parent-name-regexp "\\(?:;\\|[ \t]+\\|$\\)")
	       'ada-ff-special-extract-parent)

	 ;; A "separate" clause.
	 (cons (concat "^separate[ \t\n]*(" ada-name-regexp ")")
	       'ada-ff-special-extract-separate)

	 ;; A "with" clause. Note that it may refer to a procedure body, as well as a spec
	 (cons (concat "^\\(?:limited[ \t]+\\)?\\(?:private[ \t]+\\)?with[ \t]+" ada-name-regexp)
	       'ada-ff-special-with)
	 )))

(defvar-local ada-which-function nil
  ;; No useful default; the indentation engine should supply a useful function
  ;; This is run from ff-pre-load-hook, so ff-function-name may have
  ;; been set by ff-treat-special; don't reset it.
  "Function called with no parameters; it should return the name
of the package, protected type, subprogram, or task type whose
definition/declaration point is in or just after, or nil.  In
addition, if ff-function-name is non-nil, store in
ff-function-name a regexp that will find the function in the
other file.")

(defun ada-which-function ()
  "See `ada-which-function' variable."
  (when ada-which-function
    (funcall ada-which-function)))

(defun ada-add-log-current-function ()
  "For `add-log-current-defun-function'; uses `ada-which-function'."
  ;; add-log-current-defun is typically called with point at the start
  ;; of an ediff change section, which is before the start of the
  ;; declaration of a new item. So go to the end of the current line
  ;; first, then call `ada-which-function'
  (save-excursion
    (end-of-line 1)
    (ada-which-function)))

(defun ada-set-point-accordingly ()
  "Move to the string specified in `ff-function-name', which may be a regexp,
previously set by a file navigation command."
  (when ff-function-name
    (let ((done nil)
	  (found nil))
      (goto-char (point-min))
      ;; We are looking for an Ada declaration, so don't stop for strings or comments
      ;;
      ;; This will still be confused by multiple references; we need
      ;; to use compiler cross reference info for more precision.
      (while (not done)
	(when (search-forward-regexp ff-function-name nil t)
	  (setq found (match-beginning 0)))
	(if (ada-in-string-or-comment-p)
	    (setq found nil)
	  (setq done t)))
      (when found
	(goto-char found)
	;; different parsers find different points on the line; normalize here
	(back-to-indentation))
      (setq ff-function-name nil))))

(defun ada-find-other-file-noset (other-window-frame)
  "Same as `ada-find-other-file', but preserve point in the other file,
don't move to corresponding declaration."
  (interactive "P")
  (ada-find-other-file other-window-frame t))

(defun ada-find-other-file (other-window-frame &optional no-set-point)
  "Move to the corresponding declaration in another file.

- If region is active, assume it contains a package name;
  position point on that package declaration.

- If point is in the start line of a non-nested child package or
  subprogram declaration, position point on the corresponding
  parent package specification.

- If point is in the start line of a separate body,
  position point on the corresponding separate stub declaration.

- If point is in a context clause line, position point on the
  first package declaration that is mentioned.

- If point is in a subprogram body or specification, position point
  on the corresponding specification or body.

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
  ;; ff-file-created-hook  set to ada-ff-create-body
  ;; ff-post-load-hook     set to ada-set-point-accordingly,
  ;;                       or to a compiler-specific function that
  ;;                       uses compiler-generated cross reference
  ;;                       information

  (interactive "P")
  (when (null (car compilation-search-path))
    (error "no file search path defined; set project file?"))

  (if mark-active
      (progn
	(setq ff-function-name (buffer-substring-no-properties (point) (mark)))
	(ff-get-file
	 compilation-search-path
	 (ada-file-name-from-ada-name ff-function-name)
	 ada-spec-suffixes
	 other-window-frame)
	(deactivate-mark))

    ;; else use name at point
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
	(setq identifier (concat "\"" (match-string-no-properties 1) "\"")))

       (t
	(error "Inside string or character constant"))
       ))

     ((and (= (char-after) ?\")
	   (looking-at (concat "\"\\(" ada-operator-re "\\)\"")))
      (setq identifier (concat "\"" (match-string-no-properties 1) "\"")))

     ((looking-at "[a-zA-Z0-9_]+")
      (setq identifier (match-string-no-properties 0)))

     (t
      (error "No identifier around"))
     )))

(defun ada-goto-source (file line column other-window-frame)
  "Find and select FILE, at LINE and COLUMN.
FILE may be absolute, or on `compilation-search-path'.

OTHER-WINDOW-FRAME (default nil, set by interactive prefix)
controls window and frame choice:

nil     : show in current window
C-u     : show in other window
C-u C-u : show in other frame
"
  (setq file (ff-get-file-name compilation-search-path file))
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
  (push-mark nil t)
  (goto-char (point-min))
  (forward-line (1- line))
  (forward-char column)
  )

(defvar ada-xref-other-function nil
  "alist indexed by `ada-compiler' of functions that return cross reference information.
Function is called with five arguments:
- an Ada identifier or operator_symbol
- filename of containing the identifier
- line number containing the identifier
- column of the start of the identifier
- 'parent' flag.
Returns a list '(file line column) giving the corresponding location.
'file' may be absolute, or on `compilation-search-path'.  If point is
at the specification, the corresponding location is the body, and vice
versa. If the 'parent' flag is non-nil, return the parent type
declaration.")

(defun ada-goto-declaration (other-window-frame &optional parent)
  "Move to the declaration or body of the identifier around point.
If at the declaration, go to the body, and vice versa.

OTHER-WINDOW-FRAME (default nil, set by interactive prefix)
controls window and frame choice:

nil     : show in current window
C-u     : show in other window
C-u C-u : show in other frame"
  (interactive "P")

  (let ((xref-function (cdr (assoc ada-compiler ada-xref-other-function)))
	target)
    (when (null xref-function)
      (error "no cross reference information available"))

    (setq target
	  (funcall xref-function
		   (ada-identifier-at-point)
		   (file-name-nondirectory (buffer-file-name))
		   (line-number-at-pos)
		   (cl-case (char-after)
		     (?\" (+ 2 (current-column))) ;; work around bug in gnat find
		     (t (1+ (current-column))))
		   parent))

    (ada-goto-source (nth 0 target)
		     (nth 1 target)
		     (nth 2 target)
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

(defvar ada-xref-all-function nil
  "alist indexed by `ada-compiler' of functions that return cross reference information.
Called with four arguments:
- an Ada identifier or operator_symbol
- filename of containing the identifier
- line number containing the identifier
- column of the start of the identifier
Displays a buffer in compilation-mode giving locations where the
identifier is declared or referenced.")

(defun ada-show-references ()
  "Show all references of identifier at point."
  (interactive)

  (let ((xref-function (cdr (assoc ada-compiler ada-xref-all-function))))
    (when (null xref-function)
      (error "no cross reference information available"))

    (funcall xref-function
	     (ada-identifier-at-point)
	     (file-name-nondirectory (buffer-file-name))
	     (line-number-at-pos)
	     (cl-case (char-after)
	       (?\" (+ 2 (current-column))) ;; work around bug in gnat find
	       (t (1+ (current-column)))))
    ))

;; This is autoloaded because it may be used in ~/.emacs
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

(defun ada-show-secondary-error (other-window-frame)
  "Show the next secondary file reference in the compilation buffer.
A secondary file reference is defined by text having text
property `ada-secondary-error'.  These can be set by
compiler-specific compilation filters.

OTHER-WINDOW-FRAME (default nil, set by interactive prefix)
controls window and frame choice:

nil     : show in current window
C-u     : show in other window
C-u C-u : show in other frame"
  (interactive "P")

  ;; preserving the current window works only if the frame
  ;; doesn't change, at least on Windows.
  (let ((start-buffer (current-buffer))
	(start-window (selected-window))
	(start-frame (selected-frame))
	pos item file)
    (set-buffer compilation-last-buffer)
    (setq pos (next-single-property-change (point) 'ada-secondary-error))
    (when pos
      (setq item (get-text-property pos 'ada-secondary-error))
      ;; file-relative-name handles absolute Windows paths from
      ;; g++. Do this in compilation buffer to get correct
      ;; default-directory.
      (setq file (file-relative-name (nth 0 item)))

      ;; Set point in compilation buffer past this secondary error, so
      ;; user can easily go to the next one. For some reason, this
      ;; doesn't change the visible point!?
      (forward-line 1))

    (set-buffer start-buffer);; for windowing history
    (when item
      (ada-goto-source
       file
       (nth 1 item); line
       (nth 2 item); column
       other-window-frame)
      (select-window start-window)
      (select-frame start-frame)
      )
    ))

(defvar-local ada-goto-declaration-start nil
  ;; No useful default; the indentation engine should supply a useful function
  ;; This is run from ff-pre-load-hook, so ff-function-name may have
  ;; been set by ff-treat-special; don't reset it.
  "Function to move point to start of the generic, package,
protected, subprogram, or task declaration point is currently in
or just after.  Called with no parameters.")

(defun ada-goto-declaration-start ()
  "Call `ada-goto-declaration-start'."
  (when ada-goto-declaration-start
    (funcall ada-goto-declaration-start)))

(defvar-local ada-goto-declarative-region-start nil
  ;; No useful default; the indentation engine should supply a useful function
  "Function to move point to start of the declarative region of
the subprogram, package, task, or declare block point
is currently in.  Called with no parameters.")

(defun ada-goto-declarative-region-start ()
  "Call `ada-goto-declarative-region-start'."
  (when ada-goto-declarative-region-start
    (funcall ada-goto-declarative-region-start)))

(defvar-local ada-next-statement-keyword nil
  ;; No useful default; the indentation engine should supply a useful function
  "Function called with no parameters; it should move forward to
the next keyword in the statement following the one point is
in (ie from 'if' to 'then').  If not in a keyword, move forward
to the next keyword in the current statement. If at the last keyword,
move forward to the first keyword in the next statement or next
keyword in the containing statement.")

(defvar-local ada-goto-end nil
  ;; No useful default; the indentation engine should supply a useful function
  "Function to move point to end of the declaration or statement point is in or before.
Called with no parameters.")

(defun ada-goto-end ()
  "Call `ada-goto-end'."
  (when ada-goto-end
    (funcall ada-goto-end)))

(defun ada-next-statement-keyword ()
  "See `ada-next-statement-keyword' variable."
  (interactive)
  (when ada-next-statement-keyword
    (funcall ada-next-statement-keyword)))

(defvar-local ada-prev-statement-keyword nil
  ;; No useful default; the indentation engine should supply a useful function
  "Function called with no parameters; it should move to the previous
keyword in the statement following the one point is in (ie from
'then' to 'if').  If at the first keyword, move to the previous
keyword in the previous statement or containing statement.")

(defun ada-prev-statement-keyword ()
  "See `ada-prev-statement-keyword' variable."
  (interactive)
  (when ada-prev-statement-keyword
    (funcall ada-prev-statement-keyword)))

;;;; code creation

(defvar-local ada-make-subprogram-body nil
  "Function to convert subprogram specification after point into a subprogram body stub.
Called with no args, point at declaration start. Leave point in
subprogram body, for user to add code.")

(defun ada-make-subprogram-body ()
  "If point is in or after a subprogram specification, convert it
into a subprogram body stub, by calling `ada-make-subprogram-body'."
  (interactive)
  (ada-goto-declaration-start)
  (if ada-make-subprogram-body
      (funcall ada-make-subprogram-body)
    (error "`ada-make-subprogram-body' not set")))

(defvar ada-make-package-body nil
  "Function to create a package body from a package spec.
Called with one argument; the absolute path to the body
file. Current buffer is the package spec.  Should create the
package body file, containing skeleton code that will compile.")

(defun ada-make-package-body (body-file-name)
  (if ada-make-package-body
      (funcall ada-make-package-body body-file-name)
    (error "`ada-make-package-body' not set")))

(defun ada-ff-create-body ()
  ;; ff-find-other-file calls us with point in an empty buffer for the
  ;; body file; ada-make-package-body expects to be in the spec. So go
  ;; back.
  (let ((body-file-name (buffer-file-name)))
    (ff-find-the-other-file)
    (funcall ada-make-package-body body-file-name)
    ;; FIXME (later): if 'ada-make-package-body' fails, delete the body buffer
    ;; so it doesn't get written to disk, and we can try again.

    ;; back to the body, read in from the disk.
    (ff-find-the-other-file)
    (revert-buffer t t)
    ))

;;;; fill-comment

(defun ada-fill-comment-paragraph (&optional justify postfix)
  "Fill the current comment paragraph.
If JUSTIFY is non-nil, each line is justified as well.
If POSTFIX and JUSTIFY are non-nil, `ada-fill-comment-postfix' is appended
to each line filled and justified.
The paragraph is indented on the first line."
  (interactive "P")
  (if (and (not (ada-in-comment-p))
	   (not (looking-at "[ \t]*--")))
      (error "Not inside comment"))

  (let* (indent from to
	 (opos (point-marker))
	 ;; we bind `fill-prefix' here rather than in ada-mode because
	 ;; setting it in ada-mode causes indent-region to use it for
	 ;; all indentation.
	 (fill-prefix ada-fill-comment-prefix)
	 (fill-column (current-fill-column)))

    ;;  Find end of comment paragraph
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
    (while (re-search-forward (concat "\\(" ada-fill-comment-postfix "\\)" "\n") to t)
      (delete-region (match-beginning 1) (match-end 1)))

    (goto-char (1- to))
    (setq to (point-marker))

    ;;  Indent and justify the paragraph
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

;;;; support for font-lock.el

;; casing keywords defined here to keep the two lists together
(defconst ada-83-keywords
  '("abort" "abs" "accept" "access" "all" "and" "array" "at" "begin"
    "body" "case" "constant" "declare" "delay" "delta" "digits" "do"
    "else" "elsif" "end" "entry" "exception" "exit" "for" "function"
    "generic" "goto" "if" "in" "is" "limited" "loop" "mod" "new"
    "not" "null" "of" "or" "others" "out" "package" "pragma" "private"
    "procedure" "raise" "range" "record" "rem" "renames" "return"
    "reverse" "select" "separate" "subtype" "task" "terminate" "then"
    "type" "use" "when" "while" "with" "xor")
  "List of Ada 83 keywords.")

(defconst ada-95-keywords
  '("abstract" "aliased" "protected" "requeue" "tagged" "until")
  "List of keywords new in Ada 95.")

(defconst ada-2005-keywords
  '("interface" "overriding" "synchronized")
  "List of keywords new in Ada 2005.")

(defconst ada-2012-keywords
  '("some")
  "List of keywords new in Ada 2012.")

(defvar ada-keywords nil
  "List of Ada keywords for current `ada-language'.")

(defun ada-font-lock-keywords ()
  "Ada mode keywords for font-lock, customized according to `ada-language-version'."
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
	  "of[ \t]+reverse\\|"
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

   ;; keywords followed by a name that should be in type-face if not already fontified (for subtypes)
   ;; after "new" to handle "is new"
   (list (concat
	  "\\<\\("
	  "is"
	  "\\)\\>[ \t]*"
	  ada-name-regexp "?")
     '(1 font-lock-keyword-face) '(2 font-lock-type-face nil t))

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

;;;; ada-mode

;; autoload required by automatic mode setting
;;;###autoload
(defun ada-mode ()
  "The major mode for editing Ada code."
  ;; the other ada-*.el files add to ada-mode-hook for their setup

  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'ada-mode)
  (setq mode-name "Ada")
  (use-local-map ada-mode-map)
  (set-syntax-table ada-mode-syntax-table)
  (define-abbrev-table 'ada-mode-abbrev-table ())
  (setq local-abbrev-table ada-mode-abbrev-table)

  (set (make-local-variable 'syntax-propertize-function) 'ada-syntax-propertize)
  (set (make-local-variable 'syntax-begin-function) nil)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (set 'case-fold-search t); Ada is case insensitive; the syntax parsing requires this setting
  (set (make-local-variable 'comment-start) "--")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start-skip) "---*[ \t]*")
  (set (make-local-variable 'comment-multi-line) nil)

  ;; we _don't_ set `fill-prefix' here because that causes
  ;; indent-region to use it for all indentation. See
  ;; ada-fill-comment-paragraph.

  ;; AdaCore standard style (enforced by -gnaty) requires two spaces
  ;; after '--' in comments; this makes it easier to distinguish
  ;; special comments that have something else after '--'
  (set (make-local-variable 'comment-padding) "  ")

  (set (make-local-variable 'require-final-newline) t)

  (set (make-local-variable 'font-lock-defaults)
       '(ada-font-lock-keywords
	 nil t
	 ((?\_ . "w")))); treat underscore as a word component

  (set (make-local-variable 'ff-other-file-alist)
       'ada-other-file-alist)
  (setq ff-post-load-hook    'ada-set-point-accordingly
	ff-file-created-hook 'ada-ff-create-body)
  (add-hook 'ff-pre-load-hook 'ada-which-function)
  (setq ff-search-directories 'compilation-search-path)
  (ada-set-ff-special-constructs)

  (set (make-local-variable 'add-log-current-defun-function)
       'ada-add-log-current-function)

  (add-hook 'which-func-functions 'ada-which-function nil t)

  ;;  Support for align
  (add-to-list 'align-dq-string-modes 'ada-mode)
  (add-to-list 'align-open-comment-modes 'ada-mode)
  (set (make-local-variable 'align-region-separate) ada-align-region-separate)
  (set (make-local-variable 'align-indent-before-aligning) t)

  ;; Exclude comments alone on line from alignment.
  (add-to-list 'align-exclude-rules-list
	       '(ada-solo-comment
		 (regexp  . "^\\(\\s-*\\)--")
		 (modes   . '(ada-mode))))
  (add-to-list 'align-exclude-rules-list
	       '(ada-solo-use
		 (regexp  . "^\\(\\s-*\\)\\<use\\>")
		 (modes   . '(ada-mode))))

  (setq align-mode-rules-list ada-align-rules)

  ;;  Set up the contextual menu
  (if ada-popup-key
      (define-key ada-mode-map ada-popup-key 'ada-popup-menu))

  (easy-menu-add ada-mode-menu ada-mode-map)

  (run-mode-hooks 'ada-mode-hook)

  ;; If global-font-lock is not enabled, ada-gnat-syntax-propertize is
  ;; not run when the text is first loaded into the buffer. Recover
  ;; from that.
  (syntax-ppss-flush-cache (point-min))
  (syntax-propertize (point-max))

  (add-hook 'hack-local-variables-hook 'ada-mode-post-local-vars)
  )

(defun ada-mode-post-local-vars ()
  ;; These are run after ada-mode-hook and file local variables
  ;; because users or other ada-* files might set the relevant
  ;; variable inside the hook or file local variables (file local
  ;; variables are processed after the mode is set, and thus after
  ;; ada-mode is run).

  ;; This means to fully set ada-mode interactively, user must
  ;; do M-x ada-mode M-; (hack-local-variables)

  (when ada-auto-case (ada-case-activate-keys))

  (cl-case ada-language-version
   (ada83
    (setq ada-keywords ada-83-keywords))

   (ada95
    (setq ada-keywords
	  (append ada-83-keywords
		  ada-95-keywords)))

   (ada2005
    (setq ada-keywords
	  (append ada-83-keywords
		  ada-95-keywords
		  ada-2005-keywords)))
   (ada2012
    (setq ada-keywords
	  (append ada-83-keywords
		  ada-95-keywords
		  ada-2005-keywords
		  ada-2012-keywords))))
  )

(put 'ada-mode 'custom-mode-group 'ada)

(provide 'ada-mode)

;;;;; Global initializations

;; load indent engine first; compilers may need to know which is being
;; used (for preprocessor keywords, for example).
(unless (featurep 'ada-indent-engine)
  (require 'ada-wisi))

(unless (featurep 'ada-compiler)
  (require 'ada-gnat))

;;; end of file
