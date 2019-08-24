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

(require 'ada-wisi)
(require 'compile)
(require 'find-file)
(require 'wisi-prj)

;;;; misc

(defgroup ada nil
  "Major mode for editing Ada source code in Emacs."
  :group 'languages)

(defcustom ada-which-func-parse-size 30000
  "Minimum size of the region surrounding point that is parsed for `which-function-mode'."
  :group 'ada
  :type 'integer
  :safe #'integerp)

(defcustom ada-process-parse-exec "ada_mode_wisi_lr1_parse.exe"
  ;; We use .exe even on Linux to simplify the Makefile
  "Name of executable to use for external process Ada parser.
There are two standard choices; ada_mode_wisi_lalr_parse.exe and
ada_mode_wisi_lr1_parse.exe. The LR1 version (the default) is
slower to load on first use, but gives better error recovery."
  :type 'string
  :group 'ada-indentation)

(defcustom ada-process-parse-exec-opts nil
  "List of process start options for `ada-process-parse-exec'."
  :type 'string
  :group 'ada-indentation)

(defconst ada-wisi-language-protocol-version "2"
  "Defines language-specific parser parameters.
Must match wisi-ada.ads Language_Protocol_Version.")

(defconst ada-operator-re
  "\\+\\|-\\|/\\|\\*\\*\\|\\*\\|=\\|&\\|\\_<\\(abs\\|mod\\|rem\\|and\\|not\\|or\\|xor\\)\\_>\\|<=\\|<\\|>=\\|>"
  "Regexp matching Ada operator_symbol.")

(defconst ada-name-regexp
  "\\(\\(?:\\sw\\|[_.]\\)+\\)")

;; FIXME: dispatch on compiler slot of ada-prj
(defvar ada-compiler nil
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

(defun ada-in-based-numeric-literal-p ()
  "Return t if point is after a prefix of a based numeric literal."
  (looking-back "\\([0-9]+#[0-9a-fA-F_]+\\)" (line-beginning-position)))

(defun ada-in-paren-p (&optional parse-result)
  "Return t if point is inside a pair of parentheses.
If PARSE-RESULT is non-nil, use it instead of calling `syntax-ppss'."
  (> (nth 0 (or parse-result (syntax-ppss))) 0))

(defun ada-pos-in-paren-p (pos)
  "Return t if POS is inside a pair of parentheses."
  (save-excursion
    (> (nth 0 (syntax-ppss pos)) 0)))

(defun ada-same-paren-depth-p (pos1 pos2)
  "Return t if POS1 is at same parentheses depth as POS2."
  (= (nth 0 (syntax-ppss pos1)) (nth 0 (syntax-ppss pos2))))

(defun ada-goto-open-paren (&optional offset parse-result)
  "Move point to innermost opening paren surrounding current point, plus OFFSET.
Throw error if not in paren.  If PARSE-RESULT is non-nil, use it
instead of calling `syntax-ppss'."
  (goto-char (+ (or offset 0) (nth 1 (or parse-result (syntax-ppss))))))

;; FIXME: assume wisi
(defvar ada-goto-declarative-region-start nil
  ;; Supplied by indentation engine
  "Function to move point to start of the declarative region of
the subprogram, package, task, or declare block point
is currently in.  Called with no parameters.")

(defun ada-goto-declarative-region-start ()
  "Call `ada-goto-declarative-region-start'."
  (interactive)
  (when ada-goto-declarative-region-start
    (funcall ada-goto-declarative-region-start)))

;; FIXME: use find-tag-marker-ring, ring-insert, pop-tag-mark (see xref.el)
(defvar ada-goto-pos-ring '()
  "List of positions selected by navigation functions. Used
to go back to these positions.")

(defconst ada-goto-pos-ring-max 16
  "Number of positions kept in the list `ada-goto-pos-ring'.")

(defun ada-goto-push-pos ()
  "Push current filename, position on `ada-goto-pos-ring'. See `ada-goto-previous-pos'."
  (setq ada-goto-pos-ring (cons (list (point) (buffer-file-name)) ada-goto-pos-ring))
  (if (> (length ada-goto-pos-ring) ada-goto-pos-ring-max)
      (setcdr (nthcdr (1- ada-goto-pos-ring-max) ada-goto-pos-ring) nil)))

(defun ada-goto-previous-pos ()
  "Go to the first position in `ada-goto-pos-ring', pop `ada-goto-pos-ring'."
  (interactive)
  (when ada-goto-pos-ring
    (let ((pos (pop ada-goto-pos-ring)))
      (find-file (cadr pos))
      (goto-char (car pos)))))

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

;;;; auto-case

(defcustom ada-language-version 'ada2012
  "Ada language version; one of `ada83', `ada95', `ada2005', `ada2012'.
Only affects the keywords to highlight, not which version the
parser accepts; the parser always accepts a superset of ada2012."
  :type '(choice (const ada83)
		 (const ada95)
		 (const ada2005)
		 (const ada2012))
  :safe  #'symbolp)
(make-variable-buffer-local 'ada-language-version)

(defun ada--set-auto-case (symbol value)
  (setq wisi-auto-case value)
  (set symbol value))

(defcustom ada-auto-case t
  "Buffer-local value that may override project variable `auto_case'.
Global value is default for project variable `auto_case'.
t means automatically change case of preceding word while typing.
not-upper-case means only change case if typed word is not all upper-case.
Casing of Ada keywords is done according to `ada-case-keyword',
identifiers according to `ada-case-identifier'."
  :type  '(choice (const nil)
		  (const t)
		  (const not-upper-case))
  :safe  (lambda (val) (memq val '(nil t not-upper-case)))
  :set #'ada--set-auto-case)
(make-variable-buffer-local 'ada-auto-case)

(defvar ada-case-exception-file nil)
(make-obsolete-variable
 'ada-case-exception-file
 "set in project files only"
 "Emacs 27.1, Ada mode 6.3.0")

(defun ada--set-case-keyword (symbol value)
  (setq wisi-case-keyword value)
  (set symbol value))

(defcustom ada-case-keyword 'lower-case
  "Buffer-local value that may override project variable `case_keyword'.
Global value is default for project variable `case_keyword'.
"
  :type '(choice (const lower-case)
		 (const upper-case))
  :safe #'symbolp
  :set #'ada--set-case-keyword)
(make-variable-buffer-local 'ada-case-keyword)

(defcustom ada-case-strict t
  "Buffer-local value that may override project variable `case_strict'.
Global value is default for project variable `case_strict'.
If non-nil, force Mixed_Case for identifiers.
Otherwise, allow UPPERCASE for identifiers."
  :type 'boolean
  :safe  #'booleanp)
(make-variable-buffer-local 'ada-case-strict)

(defun ada--set-case-identifier (symbol value)
  (setq wisi-case-identifier
	(cl-ecase value
	  (mixed-case (if ada-case-strict 'mixed-case-strict 'mixed-case-relaxed))
	  (lower-case 'lower-case)
	  (upper-case 'upper-case)))
  (set symbol value))

(defcustom ada-case-identifier 'mixed-case
  "Buffer-local value that may override project variable `case_keyword'.
Global value is default for project variable `case_keyword'.
Indicates how to adjust the case of Ada keywords."
  :type '(choice (const mixed-case)
		 (const lower-case)
		 (const upper-case))
  ;; see comment on :safe at ada-case-keyword
  :safe (lambda (val) (memq val '(mixed-case lower-case upper-case)))
  :set #'ada--set-case-identifier
  :set-after '(ada-case-strict))
(make-variable-buffer-local 'ada-case-identifier)

(defvar ada-keywords nil
  "List of Ada keywords for current `ada-language-version'.")

(defun ada-after-keyword-p ()
  "Return non-nil if point is after an element of `ada-keywords'."
  (let ((word (buffer-substring-no-properties
	       (save-excursion (skip-syntax-backward "w_") (point))
	       (point))))
    (member (downcase word) ada-keywords)))

;; FIXME: when move this to wisi, cannot be single global variable;
;; multiple modes will use wisi. Move case-adjust to
;; post-command-hook? post-self-insert-hook?
(defvar ada-ret-binding 'ada-indent-newline-indent)
(defvar ada-lfd-binding 'newline-and-indent)

(defun ada-case-keyword (beg end)
  (cl-ecase ada-case-keyword
    (lower-case (downcase-region beg end))
    (upper-case (upcase-region beg end))
    ))

(defun ada-case-identifier (start end force-case-strict)
  (cl-ecase ada-case-identifier
    (mixed-case (ada-mixed-case start end force-case-strict))
    (lower-case (downcase-region start end))
    (upper-case (upcase-region start end))
    ))

(defun ada-mixed-case (start end force-case-strict)
  "Adjust case of region START END to Mixed_Case."
  (let ((done nil)
	next)
    (if (or force-case-strict ada-case-strict)
	(downcase-region start end))
    (goto-char start)
    (while (not done)
      (setq next
	    (or
	     (save-excursion (when (search-forward "_" end t) (point-marker)))
	     (copy-marker (1+ end))))

      ;; upcase first char
      (upcase-region (point) (1+ (point)))

      (goto-char next)
      (if (< (point) end)
	  (setq start (point))
	(setq done t))
      )))

(defun ada-case-adjust-identifier (&optional force-case)
  "Adjust case of the previous word as an identifier.
Uses `ada-case-identifier', with exceptions defined in
`ada-case-full-exceptions', `ada-case-partial-exceptions'."
  (interactive)
  (save-excursion
    (let ((end   (point-marker))
	  (start (progn (skip-syntax-backward "w_") (point)))
	  match
	  next
	  (done nil))

      (if (setq match (assoc-string (buffer-substring-no-properties start end)
				    (ada-prj-case-full-exceptions ada-prj-current-project)
				    t))
	  ;; full word exception
	  (progn
	    ;; 'save-excursion' puts a marker at 'end'; if we do
	    ;; 'delete-region' first, it moves that marker to 'start',
	    ;; then 'insert' inserts replacement text after the
	    ;; marker, defeating 'save-excursion'. So we do 'insert' first.
	    (insert (car match))
	    (delete-region (point) end))

	;; else apply ada-case-identifier
	(ada-case-identifier start end force-case)

	;; apply partial-exceptions
	(goto-char start)
	(while (not done)
	  (setq next
		(or
		 (save-excursion (when (search-forward "_" end t) (point-marker)))
		 (copy-marker (1+ end))))

	  (when (setq match (assoc-string (buffer-substring-no-properties start (1- next))
					  (ada-prj-case-partial-exceptions ada-prj-current-project)
					  t))
	    ;; see comment above at 'full word exception' for why
	    ;; we do insert first.
	    (insert (car match))
	    (delete-region (point) (1- next)))

	  (goto-char next)
	  (if (< (point) end)
	      (setq start (point))
	    (setq done t))
          )))))

(defun ada-case-adjust-keyword ()
  "Adjust the case of the previous word as a keyword.
`word' here is allowed to be underscore-separated (GPR external_as_list)."
  (save-excursion
    (let ((end   (point-marker))
	  (start (progn (skip-syntax-backward "w_") (point))))
      (ada-case-keyword start end)
    )))

(defun ada-case-adjust (&optional typed-char in-comment)
  "Adjust the case of the word before point.
When invoked interactively, TYPED-CHAR must be
`last-command-event', and it must not have been inserted yet.
If IN-COMMENT is non-nil, adjust case of words in comments and strings as code,
and treat `ada-case-strict' as t in code."
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
		 ;; ada-case-adjust-at-point.

		 (not (ada-in-based-numeric-literal-p))
		 ;; don't adjust case on hex digits
		 ))

      ;; The indentation engine may trigger a reparse on
      ;; non-whitespace changes, but we know we don't need to reparse
      ;; for this change (assuming the user has not abused case
      ;; exceptions!).
      (let ((inhibit-modification-hooks t))
	(cond
	 ;; Some attributes are also keywords, but captialized as
	 ;; attributes. So check for attribute first.
	 ((and
	   (not in-comment)
	   (save-excursion
	     (skip-syntax-backward "w_")
	     (eq (char-before) ?')))
	  (ada-case-adjust-identifier in-comment))

	 ((and
	   (not in-comment)
	   (not (eq typed-char ?_))
	   (ada-after-keyword-p))
	  (ada-case-adjust-keyword))

	 (t (ada-case-adjust-identifier in-comment))
	 ))
      )))

(defun ada-case-adjust-at-point (&optional in-comment)
  "If ’ada-auto-case’ is non-nil, adjust case of word at point, move to end of word.
With prefix arg, adjust case as code even if in comment or string;
otherwise, capitalize words in comments and strings.
If ’ada-auto-case’ is nil, capitalize current word."
  (interactive "P")
  (cond
   ((or (null ada-auto-case)
	(and (not in-comment)
	     (ada-in-string-or-comment-p)))
    (skip-syntax-backward "w_")
    (capitalize-word 1))

   (t
    (when
	(and (not (eobp))
	     ;; we use '(syntax-after (point))' here, not '(char-syntax
	     ;; (char-after))', because the latter does not respect
	     ;; ada-syntax-propertize.
	     (memq (syntax-class (syntax-after (point))) '(2 3)))
      (skip-syntax-forward "w_"))
    (ada-case-adjust nil in-comment))
   ))

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
      (ada-case-adjust)))
  (widen))

(defun ada-case-adjust-buffer ()
  "Adjust case of current buffer."
  (interactive)
  (ada-case-adjust-region (point-min) (point-max)))

(defun ada-case-adjust-interactive (arg)
  "If `ada-auto-case' is non-nil, adjust the case of the previous word, and process the character just typed.
To be bound to keys that should cause auto-casing.
ARG is the prefix the user entered with \\[universal-argument]."
  (interactive "P")

  ;; character typed has not been inserted yet
  (let ((lastk last-command-event)
	(do-adjust nil))
    (cond
     ((null ada-auto-case))
     ((eq ada-auto-case 'not-upper-case)
      (save-excursion
	(let* ((begin (progn (skip-syntax-backward "w_") (point)))
	       (end  (progn (skip-syntax-forward "w_") (point)))
	       (word (buffer-substring-no-properties begin end)))
	  (setq do-adjust (not (string-equal word (upcase word)))))))
     (t
      (setq do-adjust t)))

    (cond
     ((eq lastk ?\n)
        (when do-adjust
	  (ada-case-adjust lastk))
	(funcall ada-lfd-binding))

     ((memq lastk '(?\r return))
      (when do-adjust
	(ada-case-adjust lastk))
      (funcall ada-ret-binding))

     (t
      (when do-adjust
	(ada-case-adjust lastk))
      (self-insert-command (prefix-numeric-value arg)))
     )))

(defun ada-case-activate-keys (map)
  "Modify the key bindings for all the keys that should adjust casing."
  ;; we could just put these in the keymap below, but this is easier.
  (mapc (function
	 (lambda(key)
	   (define-key
	     map
	     (char-to-string key)
	     'ada-case-adjust-interactive)))
	'( ?_ ?% ?& ?* ?\( ?\) ?- ?= ?+
	      ?| ?\; ?: ?' ?\" ?< ?, ?. ?> ?/ ?\n 32 ?\r ))
  )

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
     (valid   . (lambda () (ada-align-valid)))
     (modes   . '(ada-mode)))
    (ada-use
     (regexp  . "\\(\\s-*\\)\\<\\(use\\s-\\)")
     (valid   . (lambda () (ada-align-valid)))
     (modes   . '(ada-mode)))
    (ada-at
     (regexp . "\\(\\s-+\\)\\(at\\)\\_>")
     (valid   . (lambda () (ada-align-valid)))
     (modes . '(ada-mode))))
  "Rules to use to align different lines.")

(defun ada-align-valid ()
  "See use in `ada-align-rules'."
  (save-excursion
    ;; we don't put "when (match-beginning n)" here; missing a match
    ;; is a bug in the regexp.
    (goto-char (or (match-beginning 2) (match-beginning 1)))
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
     "\\)\\_>\\)"))
  "See the variable `align-region-separate' for more information.")

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
	 (ada-in-paren-p parse-result)
	 (ada-wisi-in-case-expression))
	;; align '=>'
	(let ((begin (nth 1 parse-result))
	      (end   (scan-lists (point) 1 1)))
	  (align begin end 'entire)))

       (t
	(align-current))
       ))))

(defvar ada-in-paramlist-p nil
  ;; Supplied by indentation engine parser
  "Function to return t if point is inside the parameter-list of a subprogram declaration.
Function is called with one optional argument; syntax-ppss result.")

(defun ada-in-paramlist-p (&optional parse-result)
  "Return t if point is inside the parameter-list of a subprogram declaration."
  (when ada-in-paramlist-p
    (funcall ada-in-paramlist-p parse-result)))

(defun ada-format-paramlist ()
  "Reformat the parameter list point is in."
  (interactive)
  (condition-case nil
      (ada-goto-open-paren)
    (error
     (user-error "Not in parameter list")))
  (funcall indent-line-function); so new list is indented properly
  (when (not (looking-back "^[ \t]*" (line-beginning-position)))
    (delete-horizontal-space)
    (insert " "))
  (ada-refactor ada-refactor-format-paramlist))

;;;; xref
(defvar ada-xref-tool (if (locate-file "gpr_query" exec-path '("" ".exe")) 'gpr-query 'gnat)
  "Default Ada cross reference tool; can be overridden in project files.")

(defcustom ada-xref-full-path nil
  "If t, cross-references show the full path to source files; if
nil, only the file name."
  :group 'ada
  :type 'boolean
  :safe #'booleanp)

;; FIXME: dispatch on xref
(defvar ada-file-name-from-ada-name nil
  ;; determined by ada-xref-tool, set by *-select-prj
  "Function called with one parameter ADA-NAME, which is a library
unit name; it should return the filename in which ADA-NAME is
found.")

(defun ada-file-name-from-ada-name (ada-name)
  "Return the filename in which ADA-NAME is found."
  (ada-require-project-file)
  (funcall ada-file-name-from-ada-name ada-name))

;; FIXME: dispatch on xref
(defvar ada-ada-name-from-file-name nil
  ;; supplied by compiler
  "Function called with one parameter FILE-NAME, which is a library
unit name; it should return the Ada name that should be found in FILE-NAME.")

(defun ada-ada-name-from-file-name (file-name)
  "Return the ada-name that should be found in FILE-NAME."
  (ada-require-project-file)
  (funcall ada-ada-name-from-file-name file-name))

;; FIXME: dispatch on xref
(defvar ada-xref-other-function nil
  ;; determined by xref_tool, set by *-select-prj-xref
  "Function that returns cross reference information.
Function is called with four arguments:
- an Ada identifier or operator_symbol
- filename containing the identifier (full path)
- line number containing the identifier
- Emacs column of the start of the identifier
Point is on the start of the identifier.
Returns a list (FILE LINE COLUMN) giving the corresponding location.
FILE may be absolute, or on `compilation-search-path'.  If point is
at the specification, the corresponding location is the body, and vice
versa.")

(defun ada-goto-declaration ()
  "Move to the declaration or body of the identifier around point.
If at the declaration, go to the body, and vice versa."
  (interactive)
  (ada-check-current-project (buffer-file-name))

  (when (null ada-xref-other-function)
    (error "no cross reference information available"))

  (let ((target
	 (funcall ada-xref-other-function
		  (ada-identifier-at-point)
		  (buffer-file-name)
		  (line-number-at-pos)
		  (current-column)
		  )))

    (ada-goto-source (nth 0 target)
		     (nth 1 target)
		     (nth 2 target))
    ))

;; FIXME: dispatch on xref
(defvar ada-xref-parent-function nil
  ;; determined by xref_tool, set by *-select-prj-xref
  "Function that returns cross reference information.
Function is called with four arguments:
- an Ada identifier or operator_symbol
- filename containing the identifier
- line number containing the identifier
- Emacs column of the start of the identifier
Displays a buffer in compilation-mode giving locations of the parent type declarations.")

(defun ada-show-declaration-parents ()
  "Display the locations of the parent type declarations of the type identifier around point."
  (interactive)
  (ada-check-current-project (buffer-file-name))

  (when (null ada-xref-parent-function)
    (error "no cross reference information available"))

  (funcall ada-xref-parent-function
	   (ada-identifier-at-point)
	   (file-name-nondirectory (buffer-file-name))
	   (line-number-at-pos)
	   (current-column))
  )

;; FIXME: dispatch on xref
(defvar ada-xref-all-function nil
  ;; determined by xref_tool, set by *-select-prj-xref
  "Function that displays cross reference information.
Called with four arguments:
- an Ada identifier or operator_symbol
- filename containing the identifier
- line number containing the identifier
- Emacs column of the start of the identifier
- local-only; if t, show references in current file only
- append; if t, keep previous output in result buffer
Displays a buffer in compilation-mode giving locations where the
identifier is declared or referenced.")

(defun ada-show-references (&optional append)
  "Show all references of identifier at point.
With prefix, keep previous references in output buffer."
  (interactive "P")
  (ada-check-current-project (buffer-file-name))

  (when (null ada-xref-all-function)
    (error "no cross reference information available"))

  (funcall ada-xref-all-function
	   (ada-identifier-at-point)
	   (file-name-nondirectory (buffer-file-name))
	   (line-number-at-pos)
	   (current-column)
	   nil ;; local-only
	   append)
  )

(defun ada-show-local-references (&optional append)
  "Show all references of identifier at point.
With prefix, keep previous references in output buffer."
  (interactive "P")
  (ada-check-current-project (buffer-file-name))

  (when (null ada-xref-all-function)
    (error "no cross reference information available"))

  (funcall ada-xref-all-function
	   (ada-identifier-at-point)
	   (file-name-nondirectory (buffer-file-name))
	   (line-number-at-pos)
	   (current-column)
	   t ;; local-only
	   append)
  )
;; FIXME: dispatch on xref
(defvar ada-xref-overriding-function nil
  ;; determined by ada-xref-tool, set by *-select-prj
  "Function that displays cross reference information for overriding subprograms.
Called with four arguments:
- an Ada identifier or operator_symbol
- filename containing the identifier
- line number containing the identifier
- Emacs column of the start of the identifier
Displays a buffer in compilation-mode giving locations of the overriding declarations.")

(defun ada-show-overriding ()
  "Show all overridings of identifier at point."
  (interactive)
  (ada-check-current-project (buffer-file-name))

  (when (null ada-xref-overriding-function)
    (error "no cross reference information available"))

  (funcall ada-xref-overriding-function
	   (ada-identifier-at-point)
	   (file-name-nondirectory (buffer-file-name))
	   (line-number-at-pos)
	   (current-column))
  )

;; FIXME: dispatch on xref
(defvar ada-xref-overridden-function nil
  ;; determined by ada-xref-tool, set by *-select-prj
  "Function that displays cross reference information for overridden subprogram.
Called with four arguments:
- an Ada identifier or operator_symbol
- filename containing the identifier
- line number containing the identifier
- Emacs column of the start of the identifier
Returns a list (FILE LINE COLUMN) giving the corresponding location.
FILE may be absolute, or on `compilation-search-path'.")

(defun ada-show-overridden ()
  "Show the overridden declaration of identifier at point."
  (interactive)
  (ada-check-current-project (buffer-file-name))

  (when (null ada-xref-overridden-function)
    (error "'show overridden' not supported, or no cross reference information available"))

  (let ((target
	 (funcall ada-xref-overridden-function
		  (ada-identifier-at-point)
		  (file-name-nondirectory (buffer-file-name))
		  (line-number-at-pos)
		  (current-column))))

    (ada-goto-source (nth 0 target)
		     (nth 1 target)
		     (nth 2 target))
  ))

;; FIXME: dispatch on xref
(defvar ada-show-xref-tool-buffer nil
  ;; Supplied by xref tool
  "Function to show process buffer used by xref tool."
  )

(defun ada-show-xref-tool-buffer ()
  (interactive)
  (when ada-show-xref-tool-buffer
    (funcall ada-show-xref-tool-buffer)))

;; FIXME: assume wisi
(defvar ada-make-subprogram-body nil
  ;; Supplied by indentation engine
  "Function to convert subprogram specification after point into a subprogram body stub.
Called with no args, point at declaration start. Leave point in
subprogram body, for user to add code.")

(defun ada-make-subprogram-body ()
  "If point is in or after a subprogram specification, convert it
into a subprogram body stub, by calling `ada-make-subprogram-body'."
  (interactive)
  (wisi-goto-statement-start)
  (if ada-make-subprogram-body
      (funcall ada-make-subprogram-body)
    (error "`ada-make-subprogram-body' not set")))

;; FIXME: dispatch on xref, default ada-skel
(defvar ada-make-package-body nil
  ;; Supplied by xref tool
  "Function to create a package body from a package spec.
Called with two arguments; the project and the absolute path to the body
file. Current buffer is the package spec.  Should create the
package body file, containing skeleton code that will compile.")

(defun ada-make-package-body (body-file-name)
  ;; no error if not set; let ada-skel do its thing.
  (when ada-make-package-body
    (funcall ada-make-package-body ada-prj-current-project
	     (expand-file-name body-file-name))))

(defun ada-ff-create-body ()
  ;; no error if not set; let ada-skel do its thing.
  (when ada-make-package-body
    ;; ff-find-other-file calls us with point in an empty buffer for
    ;; the body file; ada-make-package-body expects to be in the
    ;; spec. So go back to the spec, and delete the body buffer so it
    ;; does not get written to disk.
    (let ((body-buffer (current-buffer))
	  (body-file-name (buffer-file-name)))

      (set-buffer-modified-p nil);; may have a skeleton; allow silent delete

      (ff-find-the-other-file);; back to spec

      (kill-buffer body-buffer)

      (ada-make-package-body body-file-name)

      ;; back to the new body file, read in from the disk.
      (ff-find-the-other-file)
      (revert-buffer t t))
    ))

(defun ada-goto-source (file line column)
  "Find and select FILE, at LINE and COLUMN.
FILE may be absolute, or on `compilation-search-path'.
LINE, COLUMN are Emacs origin."
  (let ((file-1
	 (if (file-name-absolute-p file) file
	   (ff-get-file-name compilation-search-path file))))
    (if file-1
	(setq file file-1)
      (error "File %s not found; installed library, or set project?" file))
    )

  (ada-goto-push-pos)

  (let ((buffer (get-file-buffer file)))
    (cond
     ((bufferp buffer)
      ;; use pop-to-buffer, so package other-frame-window works.
      (pop-to-buffer buffer (list #'display-buffer-same-window)))

     ((file-exists-p file)
      (find-file file))

     (t
      (error "'%s' not found" file))))

  ;; move the cursor to the correct position
  (push-mark nil t)
  (goto-char (point-min))
  (forward-line (1- line))
  (forward-char column)
  )

;; FIXME: dispatch on xref
(defvar ada-xref-refresh-function nil
  ;; determined by xref_tool, set by *-select-prj-xref
  "Function that refreshes cross reference information cache.")

(defun ada-xref-refresh (delete-files)
  "Refresh cross reference information cache, if any.
With non-nil prefix arg, delete cross reference files, which may
be needed when a compiler is upgraded, or some configuration is
changed."
  (interactive "P")

  (when (null ada-xref-refresh-function)
    (error "no cross reference information available"))

  (funcall ada-xref-refresh-function delete-files)
  )

(defun ada-identifier-at-point ()
  "Return the identifier around point, move point to start of
identifier.  May be an Ada identifier or operator."

  (when (ada-in-comment-p)
    (error "Inside comment"))

  ;; Handle adjacent operator/identifer like:
  ;; test/ada_mode-slices.adb
  ;;   D1, D2 : Day := +Sun;

  ;; Move to the beginning of the identifier or operator
  (if (looking-at "[a-zA-Z0-9_]")
      ;; In an identifier
      (skip-chars-backward "a-zA-Z0-9_")
    ;; In an operator
    (skip-chars-backward "+\\-\\*/&<>="))

  ;; Just in front of, or inside, a string => we could have an
  ;; operator function declaration.
  (cond
   ((ada-in-string-p)
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

(defcustom ada-prj-file-ext-extra nil
  "List of secondary project file extensions."
  :type 'list)
(make-obsolete-variable
 'ada-prj-file-ext-extra
 'wisi-prj-file-extensions
 "Emacs 27.1, Ada mode 6.3.0")

(defcustom ada-prj-parse-hook nil
  "Hook run at start of `ada-parse-prj-file'.
Useful for setting `ada-xref-tool' and similar vars."
  :type 'function
  :group 'ada)

(defvar ada-prj-file-extensions '("adp" "prj")
  "List of Emacs Ada mode project file extensions.")
(make-obsolete-variable
 'ada-prj-file-extensions
 'wisi-prj-file-extensions
 "Emacs 27.1, Ada mode 6.3.0")

(defvar ada-prj-alist nil
  "Alist holding currently parsed Emacs Ada project files. Indexed by absolute project file name.")

(defvar ada-prj-current-file nil
  "Current Emacs Ada project file.")

(defvar ada-prj-current-project nil
  "Current Emacs Ada mode project; an `ada-prj' object")

(defun ada-require-project-file ()
  (unless ada-prj-current-file
    (error "no Emacs Ada project file specified")))

(defvar ada-prj-default-list nil
  ;; project file parse
  "List of functions to add default project variables. Called
with one argument; the default project properties
list. `default-directory' is set to the directory containing the
project file. Function should add to the properties list and
return it.")

(defvar ada-prj-default-compiler-alist nil
  ;; project file parse
  "Compiler-specific function to set default project variables.
Indexed by ada-compiler.  Called with one argument; the default
project properties list. Function should add to the properties
list and return it.")

(defvar ada-prj-default-xref-alist nil
  ;; project file parse
  "Xref-tool-specific function to set default project variables.
Indexed by ada-xref-tool.  Called with one argument; the default
project properties list. Function should add to the properties
list and return it.")

(defun ada-prj-default (&optional src-dir)
  "Return the default `ada-prj' object.
If SRC-DIR is non-nil, use it as the default for src_dir.
Include properties set via `ada-prj-default-compiler-alist',
`ada-prj-default-xref-alist'."

  (let ((project
	 (make-ada-prj
	  :plist (list
		  ;; variable name alphabetical order
		  'ada_compiler    ada-compiler
		  'auto_case       ada-auto-case
		  'case_keyword    ada-case-keyword
		  'case_identifier ada-case-identifier
		  'case_strict     ada-case-strict
		  'casing          nil
		  'path_sep        path-separator;; prj variable so users can override it for their compiler
		  'proc_env        (cl-copy-list process-environment)
		  'src_dir         (if src-dir (list src-dir) nil)
		  'xref_tool       ada-xref-tool
		  )))
	  func)

    (cl-dolist (func ada-prj-default-list)
      (funcall func project))

    (setq func (cdr (assq ada-compiler ada-prj-default-compiler-alist)))
    (when func (funcall func project))
    (setq func (cdr (assq ada-xref-tool ada-prj-default-xref-alist)))
    (when func (funcall func project))

    project))

(defvar ada-prj-parser-alist
  (mapcar
   (lambda (ext) (cons ext #'ada-prj-parse-file-1))
   ada-prj-file-extensions)
  ;; project file parse
  "Alist of parsers for project files, indexed by file extension.
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
  (setq prj-file (expand-file-name prj-file))

  (unless (file-readable-p prj-file)
    (error "Project file '%s' is not readable" prj-file))

  (run-hooks `ada-prj-parse-hook)

  (let* ((default-directory (file-name-directory prj-file))
	 (project (ada-prj-default))
	 (parser (cdr (assoc (file-name-extension prj-file) ada-prj-parser-alist))))

    (if parser
	;; parser may reference the "current project", so bind that now.
	(let ((ada-prj-current-project project)
	      (ada-prj-current-file prj-file))
	  (funcall parser prj-file project))
      (error "no project file parser defined for '%s'" prj-file))

    ;; Store the project properties
    (if (assoc prj-file ada-prj-alist)
	(setcdr (assoc prj-file ada-prj-alist) project)
      (add-to-list 'ada-prj-alist (cons prj-file project)))

    ;; return t for interactive use
    t))

(defun ada-prj-reparse-select-current ()
  "Reparse the current project file, re-select it.
Useful when the project file has been edited."
  (interactive)
  (ada-parse-prj-file ada-prj-current-file)
  (ada-select-prj-file ada-prj-current-file))

(defun ada-reset-comp-prj ()
  "Reset compilation and project vars affected by a change in compiler version.
Useful when experimenting with an upgraded compiler."
  (interactive)
  (when (buffer-live-p "*compilation*")
    (with-current-buffer "*compilation*"
      (setq compilation-environment nil)))
  (setq ada-prj-alist nil)
  (setq ada-prj-current-project nil)
  )

;; FIXME: dispatch on ada-prj-compiler
(defvar ada-prj-parse-one-compiler nil
  ;; project file parse
  "Compiler-specific function to process one Ada project property.
Indexed by project variable ada_compiler.
Called with three arguments; the property name, property value,
and project properties list. Function should add to or modify the
properties list and return it, or return nil if the name is not
recognized.")

;; FIXME: dispatch on ada-prj-xref
(defvar ada-prj-parse-one-xref nil
  ;; project file parse
  "Xref-tool-specific function to process one Ada project property.
Indexed by project variable xref_tool.
Called with three arguments; the property name, property value,
and project properties list. Function should add to or modify the
properties list and return it, or return nil if the name is not
recognized.")

;; FIXME: dispatch on ada-prj-compiler
(defvar ada-prj-parse-final-compiler nil
  ;; project file parse
  "Alist of compiler-specific functions to finish processing Ada project properties.
Indexed by project variable ada_compiler.
Called with one argument; the project properties list. Function
should add to or modify the list and return it.")

;; FIXME: dispatch on ada-prj-xref
(defvar ada-prj-parse-final-xref nil
  ;; project file parse
  "Alist of xref-tool-specific functions to finish processing Ada project properties.
Indexed by project variable xref_tool.
Called with one argument; the project properties list. Function
should add to or modify the list and return it.")

(cl-defstruct (ada-prj (:include wisi-prj))
  plist ;; old-style ada-mode project property list, while we are converting.
  )

(defun ada-prj-parse-file-1 (prj-file project)
  "Parse the Ada mode project file PRJ-FILE, set project properties in PROJECT.
PROJECT is an `ada-prj' object."
  (let (src_dir obj_dir
		(parse-one-compiler (cdr (assoc ada-compiler ada-prj-parse-one-compiler)))
		(parse-final-compiler (cdr (assoc ada-compiler ada-prj-parse-final-compiler)))
		(parse-one-xref (cdr (assoc ada-xref-tool ada-prj-parse-one-xref)))
		(parse-final-xref (cdr (assoc ada-xref-tool ada-prj-parse-final-xref))))

    (with-current-buffer (find-file-noselect prj-file)
      (goto-char (point-min))

      ;; process each line
      (while (not (eobp))

	;; ignore lines that don't have the format "name=value", put
	;; 'name', 'value' in match-string.
	(when (looking-at "^\\([^=\n]+\\)=\\(.*\\)")
	  (cond
	   ;; variable name alphabetical order

	   ((string= (match-string 1) "ada_compiler")
	    (let ((comp (intern (match-string 2))))
	      (setf (ada-prj-plist project) (plist-put (ada-prj-plist project) 'ada_compiler comp))
	      (setq parse-one-compiler (cdr (assq comp ada-prj-parse-one-compiler)))
	      (setq parse-final-compiler (cdr (assq comp ada-prj-parse-final-compiler)))))

	   ((string= (match-string 1) "auto_case")
	    (setf (ada-prj-plist project) (plist-put (ada-prj-plist project) 'auto_case (intern (match-string 2)))))

	   ((string= (match-string 1) "case_keyword")
	    (setf (ada-prj-plist project) (plist-put (ada-prj-plist project) 'case_keyword (intern (match-string 2)))))

	   ((string= (match-string 1) "case_identifier")
	    (setf (ada-prj-plist project)
		  (plist-put (ada-prj-plist project) 'case_identifier (intern (match-string 2)))))

	   ((string= (match-string 1) "case_strict")
	    (setf (ada-prj-plist project) (plist-put (ada-prj-plist project) 'case_strict (intern (match-string 2)))))

	   ((string= (match-string 1) "casing")
            (cl-pushnew (expand-file-name
                         (substitute-in-file-name (match-string 2)))
                        (ada-prj-case-exception-files project)
			:test #'equal))

	   ((string= (match-string 1) "el_file")
	    (let ((file (expand-file-name (substitute-in-file-name (match-string 2)))))
	      (setf (ada-prj-plist project) (plist-put (ada-prj-plist project) 'el_file file))
	      ;; eval now as well as in select, since it might affect parsing
	      (load-file file)))

	   ((string= (match-string 1) "src_dir")
            (cl-pushnew (file-name-as-directory
                         (expand-file-name (match-string 2)))
                        src_dir :test #'equal))

	   ((string= (match-string 1) "obj_dir")
	    (cl-pushnew (file-name-as-directory
			 (expand-file-name (match-string 2)))
			obj_dir :test #'equal))

	   ((string= (match-string 1) "xref_tool")
	    (let ((xref (intern (match-string 2))))
	      (setf (ada-prj-plist project) (plist-put (ada-prj-plist project) 'xref_tool xref))
	      (setq parse-one-xref (cdr (assq xref ada-prj-parse-one-xref)))
	      (setq parse-final-xref (cdr (assq xref ada-prj-parse-final-xref)))))

	   ((and parse-one-compiler
		 (funcall parse-one-compiler (match-string 1) (match-string 2) project)))

	   ((and parse-one-xref
		 (funcall parse-one-xref (match-string 1) (match-string 2) project)))

	   (t
	    ;; Any other field in the file is set as an environment
	    ;; variable or a project file variable.
	    (if (= ?$ (elt (match-string 1) 0))
		;; process env var. We don't do expand-file-name
		;; here because the application may be expecting a
		;; simple string.
		(let ((process-environment (cl-copy-list (plist-get (ada-prj-plist project) 'proc_env))))
		  (setenv (substring (match-string 1) 1)
			  (substitute-in-file-name (match-string 2)))
		  (setf (ada-prj-plist project)
			(plist-put (ada-prj-plist project) 'proc_env (cl-copy-list process-environment))))

	      ;; Assume it is a user-defined project variable like "comp_opt"
	      (setf (ada-prj-plist project) (plist-put (ada-prj-plist project)
						       (intern (match-string 1)) (match-string 2)))
	      ))
	   ))

	(forward-line 1))

      );; done reading file

    ;; process accumulated lists
    (if src_dir (setf (ada-prj-plist project) (plist-put (ada-prj-plist project) 'src_dir (reverse src_dir))))
    (if obj_dir (setf (ada-prj-plist project) (plist-put (ada-prj-plist project) 'obj_dir (reverse obj_dir))))

    (when parse-final-compiler
      ;; parse-final-compiler may reference the "current project", so
      ;; bind that now, to include the properties set above. FIXME: just use PROJECT!
      (let ((ada-prj-current-project project)
	    (ada-prj-current-file prj-file))
	(funcall parse-final-compiler project)))

    (when parse-final-xref
      (let ((ada-prj-current-project project)
	    (ada-prj-current-file prj-file))
	(funcall parse-final-xref project)))
    ))

;; FIXME: dispatch on ada-prj-compiler
(defvar ada-select-prj-compiler nil
  "Alist of functions to call for compiler specific project file selection.
Indexed by project variable ada_compiler.")

;; FIXME: dispatch on ada-prj-compiler
(defvar ada-deselect-prj-compiler nil
  "Alist of functions to call for compiler specific project file deselection.
Indexed by project variable ada_compiler.")

;; FIXME: dispatch on ada-prj-xref
(defvar ada-select-prj-xref-tool nil
  "Alist of functions to call for xref-tool specific project file selection.
Indexed by project variable xref_tool.")

;; FIXME: dispatch on ada-prj-xref
(defvar ada-deselect-prj-xref-tool nil
  "Alist of functions to call for xref-tool specific project file deselection.
Indexed by project variable xref_tool.")

;; This is autoloaded because it is often used in Makefiles, and thus
;; will be the first ada-mode function executed.
;;;###autoload
(defun ada-select-prj-file (prj-file &optional no-force)
  "Select PRJ-FILE as the current project file, parsing it if necessary.
Deselects the current project first."
  (interactive)
  (setq prj-file (expand-file-name prj-file))

  (when (or (not no-force)
	    (not (string-equal prj-file ada-prj-current-project)))
    (setq ada-prj-current-project (cdr (assoc prj-file ada-prj-alist)))

    (when (null ada-prj-current-project)
      (setq ada-prj-current-file nil)
      (ada-parse-prj-file prj-file)
      (setq ada-prj-current-project (cdr (assoc prj-file ada-prj-alist)))
      (when (null ada-prj-current-project)
	(error "Project file '%s' parse failed." prj-file)))

    (setq ada-prj-current-file prj-file)

    (let ((func (cdr (assq (plist-get (ada-prj-plist ada-prj-current-project) 'ada_compiler)
			   ada-deselect-prj-compiler))))
      (when func (funcall func ada-prj-current-project)))

    (let ((func (cdr (assq (plist-get (ada-prj-plist ada-prj-current-project) 'xref_tool)
			   ada-deselect-prj-xref-tool))))
      (when func (funcall func ada-prj-current-project)))

    ;; Project file should fully specify what compilers are used,
    ;; including what compilation filters they need. There may be more
    ;; than just an Ada compiler.
    (setq compilation-error-regexp-alist nil)
    (setq compilation-filter-hook nil)

    (when (plist-get (ada-prj-plist ada-prj-current-project) 'el_file)
      (load-file (plist-get (ada-prj-plist ada-prj-current-project) 'el_file)))

    ;; FIXME: move to wisi-prj-select
    (wisi--case-read-all-exceptions ada-prj-current-project)

    (setq compilation-search-path (plist-get (ada-prj-plist ada-prj-current-project) 'src_dir))

    (let ((func (cdr (assq (plist-get (ada-prj-plist ada-prj-current-project) 'ada_compiler) ada-select-prj-compiler))))
      (when func (funcall func ada-prj-current-project)))

    (let ((func (cdr (assq (plist-get (ada-prj-plist ada-prj-current-project) 'xref_tool) ada-select-prj-xref-tool))))
      (when func (funcall func ada-prj-current-project)))

    ;; return 't', for decent display in message buffer when called interactively
    t))

(defun ada-deselect-prj (prj-file)
  "Deselect the project file PRJ-FILE, if current."
  ;; For use as ’project-deselect’ (experimental). Duplicates part of
  ;; ’ada-select-prj-file’; should delete that, use this.
  (when (string-equal prj-file ada-prj-current-file)
    (let ((func (cdr (assq (plist-get (ada-prj-plist ada-prj-current-project) 'ada_compiler)
			   ada-deselect-prj-compiler))))
      (when func (funcall func)))

    (let ((func (cdr (assq (plist-get (ada-prj-plist ada-prj-current-project) 'xref_tool)
			   ada-deselect-prj-xref-tool))))
      (when func (funcall func)))

    (setq ada-prj-current-project nil)
    ))

(defun ada-refresh-prj-file ()
  "Reparse, reselect current project file.
Useful when project has been edited."
  (interactive)
  (let* ((prj-file ada-prj-current-file)
   	 (parsed (assoc prj-file ada-prj-alist)))
    (setq ada-prj-current-file nil)
    (setq ada-prj-current-project nil)
    (when parsed
      (ada-deselect-prj prj-file)
      (setq ada-prj-alist (delq parsed ada-prj-alist)))
    (ada-select-prj-file prj-file nil)))

(defun ada-create-select-default-prj (&optional directory)
  "Create a default project with src_dir set to DIRECTORY (default current directory), select it."
  (let* ((dir (or directory default-directory))
	 (prj-file (expand-file-name "default_.adp" dir))
	 (project (ada-prj-default dir)))

    (if (assoc prj-file ada-prj-alist)
	(setcdr (assoc prj-file ada-prj-alist) project)
      (add-to-list 'ada-prj-alist (cons prj-file project)))

    (ada-select-prj-file prj-file)
    ))

(defun ada-prj-select ()
  "Select the current project file from the list of currently available project files."
  (interactive)
  (ada-select-prj-file (completing-read "project: " ada-prj-alist nil t))
  )

(defun ada-prj-delete ()
  "Delete a project file from the list of currently available project files."
  (interactive)
  (let* ((prj-file (completing-read "project: " ada-prj-alist nil t))
	 (prj-entry (assoc prj-file ada-prj-alist)))
    (setq ada-prj-alist (delete prj-entry ada-prj-alist))
    ))

(defun ada-prj-show ()
  "Show current Emacs Ada mode project file."
  (interactive)
  (message "current Emacs Ada mode project file: %s" ada-prj-current-file))

;; FIXME: dispatch on ada-prj-compiler
(defvar ada-prj-show-prj-path nil
  ;; Supplied by compiler
  "Function to show project file search path used by compiler (and possibly xref tool)."
  )

(defun ada-prj-show-prj-path ()
  (interactive)
  (when ada-prj-show-prj-path
    (funcall ada-prj-show-prj-path)))

(defun ada-prj-show-src-path ()
  "Show the project source file search path."
  (interactive)
  (if compilation-search-path
      (progn
	(pop-to-buffer (get-buffer-create "*Ada project source file search path*"))
	(erase-buffer)
	(dolist (file compilation-search-path)
	  (insert (format "%s\n" file))))
    (message "no project source file search path set")
    ))

(defun ada-check-current-project (file-name)
  "Throw error if FILE-NAME (must be absolute) is not found in
the current project source directories, or if no project has been
set."
  (when (null (car compilation-search-path))
    (error "no file search path defined; set project file?"))

  ;; file-truename handles symbolic links
  (let* ((visited-file (file-truename file-name))
         (found-file (locate-file (file-name-nondirectory visited-file)
				  compilation-search-path)))
    (unless found-file
      (error "current file not part of current project; wrong project?"))

    (setq found-file (file-truename found-file))

    ;; (nth 10 (file-attributes ...)) is the inode; required when hard
    ;; links are present.
    (let* ((visited-file-inode (nth 10 (file-attributes visited-file)))
           (found-file-inode (nth 10 (file-attributes found-file))))
      (unless (equal visited-file-inode found-file-inode)
        (error "%s (opened) and %s (found in project) are two different files"
               file-name found-file)))))

(defun ada-project-menu-compute ()
  "Return an easy-menu menu for `ada-project-menu-install'.
Menu displays currently parsed Ada mode projects."
  (let (menu)
    (dolist (item ada-prj-alist)
      (push
       (vector
	(if (equal (car item) ada-prj-current-file)
	    ;; current project
	    (concat (car item) "  *")
	  (car item))
	`(lambda () (interactive) (ada-select-prj-file ,(car item)))
	t)
       menu)
      )
    (nreverse menu)))

(provide 'ada-core)
;; ada-core.el ends here
