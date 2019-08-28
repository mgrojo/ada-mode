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

;;;; ada-compiler generic interface

(cl-defgeneric ada-compiler-parse-one (compiler name value)
  "Set NAME, VALUE in COMPILER, if recognized by COMPILER.")

(cl-defgeneric ada-compiler-parse-final (compiler project prj-file-name)
  ;; project holds src_dir, set from compiler project file.
  "Do any compiler-specific processing on COMPILER and PROJECT
after the project file PRJ-FILE-NAME is parsed.")

(cl-defgeneric ada-compiler-select-prj (compiler project)
  "PROJECT has been selected; do any compiler-specific actions required.")

(cl-defgeneric ada-compiler-deselect-prj (compiler project)
  "PROJECT has been de-selected; undo any compiler-specific select actions.")

(cl-defgeneric ada-compiler-file-name-from-ada-name (compiler ada-name)
  "Return the filename that would contain the library level ADA-NAME.")

(defun ada-file-name-from-ada-name (ada-name)
  "Return the filename in which ADA-NAME is found."
  (ada-compiler-file-name-from-ada-name (ada-prj-compiler (ada-prj-require-prj)) ada-name))

(cl-defgeneric ada-compiler-ada-name-from-file-name (file-name)
  "Return the Ada library unit name that should be found in FILE-NAME.")

(defun ada-ada-name-from-file-name (file-name)
  "Return the ada-name that should be found in FILE-NAME."
  (ada-compiler-ada-name-from-file-name (ada-prj-compiler (ada-prj-require-prj)) file-name))

(cl-defgeneric ada-compiler-make-package-body (body-file-name)
  "Create a package body skeleton from a package spec.
BODY-FILE-NAME is the file name of the body file. Current buffer
is the package spec.")

(defun ada-make-package-body (body-file-name)
  (ada-compiler-make-package-body (ada-prj-compiler (ada-prj-require-prj))
				  (expand-file-name body-file-name)))

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
    (let ((prj (ada-prj-require-prj))
	  (end   (point-marker))
	  (start (progn (skip-syntax-backward "w_") (point)))
	  match
	  next
	  (done nil))

      (if (setq match
		(assoc-string (buffer-substring-no-properties start end)
			      (ada-prj-case-full-exceptions prj)
			      t ;; case-fold
			      ))
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
					  (ada-prj-case-partial-exceptions prj)
					  t))
	    ;; see comment above at 'full word exception' for why
	    ;; we do insert first.
	    (insert (car match))
	    (delete-region (point) (1- next)))

	  (goto-char next)
	  (if (< (point) end)
	      (setq start (point))
	    (setq done t))
          ))
      )))

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

(defun ada-in-case-expression ()
  "Return non-nil if point is in a case expression."
  (save-excursion
    ;; Used by ada-align; we know we are in a paren.
    (ada-goto-open-paren 1)
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
	 (ada-in-paren-p parse-result)
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

(cl-defgeneric ada-xref-parse-one (xref name value)
  "Set NAME, VALUE in XREF, if recognized by XREF.")

(cl-defgeneric ada-xref-parse-final (xref project prj-file-name)
  "Do any xref-specific processing on XREF and PROJECT
after the project file PRJ-FILE-NAME is parsed.")

(cl-defgeneric ada-xref-select-prj (xref project)
  "PROJECT has been selected; do any xref-specific actions required.")

(cl-defgeneric ada-xref-deselect-prj (xref project)
  "PROJECT has been de-selected; undo any xref-specific select actions.")

(cl-defgeneric ada-xref-refresh-cache (xref no-full)
  "Refresh cached information in XREF. If no-full is non-nil,
slow refresh operations may be skipped.")

(cl-defgeneric ada-xref-other (xref &key identifier filename line column)
  "Function that returns cross reference information.
IDENTIFIER - an Ada identifier or operator_symbol
FILENAME - absolute filename containing the identifier
LINE - line number containing the identifier (may be nil)
COLUMN - Emacs column of the start of the identifier (may be nil)
Point is on the start of the identifier.
Returns a list (FILE LINE COLUMN) giving the corresponding location.
If point is at the specification, the corresponding location is the
body, and vice versa.")

(defun ada-goto-declaration ()
  "Move to the declaration or body of the identifier around point.
If at the declaration, go to the body, and vice versa."
  (interactive)
  (ada-check-current-project (buffer-file-name))

  (let ((target (ada-xref-other
		 (ada-prj-xref (ada-prj-require-prj))
		 :identifier (ada-identifier-at-point)
		 :filename (buffer-file-name)
		 :line (line-number-at-pos)
		 :column (current-column)
		 )))

    (ada-goto-source (nth 0 target)
		     (nth 1 target)
		     (nth 2 target))
    ))

(cl-defgeneric ada-xref-parents (compiler &key identifier filename line column)
  "Displays parent type declarations.
IDENTIFIER - an Ada identifier or operator_symbol
FILENAME - absolute filename containing the identifier
LINE - line number containing the identifier
COLUMN - Emacs column of the start of the identifier

Displays a buffer in compilation-mode giving locations of the parent type declarations.")

(defun ada-show-declaration-parents ()
  "Display the locations of the parent type declarations of the type identifier around point."
  (interactive)
  (ada-check-current-project (buffer-file-name))
  (ada-xref-parents
   (ada-prj-xref (ada-prj-require-prj))
   :identifier (ada-identifier-at-point)
   :filename (file-name-nondirectory (buffer-file-name))
   :line (line-number-at-pos)
   :column (current-column))
  )

(cl-defgeneric ada-xref-all (compiler &key identifier filename line column local-only append)
  "Displays cross reference information.
IDENTIFIER - an Ada identifier or operator_symbol (a string).
FILENAME - absolute filename containing the identifier
LINE - line number containing the identifier
COLUMN - Emacs column of the start of the identifier
LOCAL-ONLY - if t, show references in FILE only
APPEND - if t, keep previous output in result buffer
Displays a buffer in compilation-mode giving locations where the
identifier is declared or referenced.")

(defun ada-show-references (&optional append)
  "Show all references of identifier at point.
With prefix, keep previous references in output buffer."
  (interactive "P")
  (ada-check-current-project (buffer-file-name))
  (ada-xref-all
   (ada-prj-xref (ada-prj-require-prj))
   :identifier (ada-identifier-at-point)
   :filename (file-name-nondirectory (buffer-file-name))
   :line (line-number-at-pos)
   :column (current-column)
   :local-only nil
   :append append)
  )

(defun ada-show-local-references (&optional append)
  "Show all references of identifier at point.
With prefix, keep previous references in output buffer."
  (interactive "P")
  (ada-check-current-project (buffer-file-name))

  (ada-xref-all
   (ada-prj-xref (ada-prj-require-prj))
   :identifier (ada-identifier-at-point)
   :filename (file-name-nondirectory (buffer-file-name))
   :line (line-number-at-pos)
   :column (current-column)
   :local-only t
   :append append)
  )

(cl-defgeneric ada-xref-overriding (compiler &key identifier filename line column)
  "Displays a buffer in compilation-mode giving locations of the overriding declarations.
IDENTIFIER - an Ada identifier or operator_symbol
FILENAME - filename containing the identifier
LINE - line number containing the identifier
COLUMN - Emacs column of the start of the identifier ")

(defun ada-show-overriding ()
  "Show all overridings of identifier at point."
  (interactive)
  (ada-check-current-project (buffer-file-name))
  (ada-xref-overriding
   (ada-prj-xref (ada-prj-require-prj))
   :identifier (ada-identifier-at-point)
   :filename (file-name-nondirectory (buffer-file-name))
   :line (line-number-at-pos)
   :column (current-column))
  )

(cl-defgeneric ada-xref-overridden (compiler &key identifier filename line column)
  "Returns a list (FILE LINE COLUMN) giving the location of the overridden declaration.
IDENTIFIER - an Ada identifier or operator_symbol
FILENAME - absolute filename containing the identifier
LINE - line number containing the identifier
COLUMN - Emacs column of the start of the identifier")

(defun ada-show-overridden ()
  "Show the overridden declaration of identifier at point."
  (interactive)
  (ada-check-current-project (buffer-file-name))
  (let ((target
	 (ada-xref-overridden
	  (ada-prj-xref (ada-prj-require-prj))
	  :identifier (ada-identifier-at-point)
	  :filename (file-name-nondirectory (buffer-file-name))
	  :line (line-number-at-pos)
	  :column (current-column))))

    (ada-goto-source (nth 0 target)
		     (nth 1 target)
		     (nth 2 target))
  ))

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

;; FIXME: delete after finish converting ada-build.el
(defvar ada-prj-default-list nil
  ;; project file parse
  "List of functions to add default project variables. Called
with one argument; the project. `default-directory' is set to the directory containing the
project file. Function should update the project.")

(defun ada-prj-default (&optional src-dir)
  "Return the default `ada-prj' object.
If SRC-DIR is non-nil, use it as the default for src_dir."

  (let ((project
	 (make-ada-prj
	  :compiler-label  ada-compiler
	  :xref-label      ada-xref-tool
	  :plist (list
		  ;; variable name alphabetical order
		  'auto_case       ada-auto-case
		  'case_keyword    ada-case-keyword
		  'case_identifier ada-case-identifier
		  'case_strict     ada-case-strict
		  'casing          nil
		  'src_dir         (if src-dir (list src-dir) nil)
		  ))))

    (cl-dolist (func ada-prj-default-list) ;; FIXME delete
      (funcall func project))

    project))

;; This autoloaded because it is often used in Makefiles, and thus
;; will be the first ada-mode function executed.
;;;###autoload
(defun ada-parse-prj-file (prj-file)
    (wisi-prj-parse-file prj-file))
(make-obsolete
 'ada-parse-prj-file
 'wisi-prj-parse-file
 "ada-mode 7.0")

(defun ada-prj-make-compiler (label)
  (funcall (intern (format "make-%s-compiler" (symbol-name label)))))

(defun ada-prj-make-xref (label)
  (funcall (intern (format "make-%s-xref" (symbol-name label)))))

(cl-defstruct
    (ada-prj
     (:include wisi-prj)
     (:copier nil)
     (:constructor nil)
     (:constructor make-ada-prj
		   (&key
		    compiler-label
		    xref-label
		    plist
		    file-pred
		    &aux
		    (compiler (ada-prj-make-compiler compiler-label))
		    (xref (ada-prj-make-xref xref-label))
		    )))
  compiler ;; compiler object
  xref     ;; xref object
  plist    ;; old-style ada-mode project property list, while we are converting. FIXME: delete

  file-pred
  ;; Function taking an absolute file name, returns non-nil
  ;; if the file should be included in `project-files'.
  )

(defun ada-prj-require-prj ()
  "Return current `ada-prj' object.
Throw an error if current project is not an ada-prj."
  (let ((prj (project-current)))
    (if (ada-prj-p prj)
	prj
      (error "selected project is not an ada project."))))

(defun ada-prj-parse-file (prj-file project)
  "Parse the Ada mode project file PRJ-FILE, set project properties in PROJECT.
PROJECT is an `ada-prj' object."
  ;; FIXME: move parsing for wisi-prj slots to wisi-parse-prj-file-1 or something
  (let (src_dir obj_dir)

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
	      (setf (ada-prj-compiler project) (ada-prj-make-compiler comp))))

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
	      (setf (ada-prj-xref project) (ada-prj-make-xref xref))))

	   ((let ((name (match-string 1))
		  (value (match-string 2))
		  result)
	      ;; Both compiler and xref need to see some settings; eg gpr_file, env vars.
	      (when (ada-compiler-parse-one (ada-prj-compiler project) name value)
		(setq result t))
	      (when (ada-xref-parse-one (ada-prj-xref project) name value)
		(setq result t))
	      result))

	   (t
	    ;; Any other field in the file is set as a project file variable.
	    ;; eg "comp_opt"
	    (setf (ada-prj-plist project) (plist-put (ada-prj-plist project)
						     (intern (match-string 1)) (match-string 2))))
	   ))

	(forward-line 1))

      );; done reading file

    ;; process accumulated lists
    (if src_dir (setf (ada-prj-plist project) (plist-put (ada-prj-plist project) 'src_dir (reverse src_dir))))
    (if obj_dir (setf (ada-prj-plist project) (plist-put (ada-prj-plist project) 'obj_dir (reverse obj_dir))))

    (wisi-prj-parse-final project)
    (ada-compiler-parse-final (ada-prj-compiler project) project prj-file)
    (ada-xref-parse-final     (ada-prj-xref     project) project prj-file)
    ))

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
(defun ada-select-prj-file (prj-file)
  ;; not ada-prj-select-file for backward compatibility
  "Select PRJ-FILE as the current project file, parsing it if necessary.
Deselects the current project first."
  (wisi-prj-select-file prj-file))
(make-obsolete 'ada-select-prj-file 'wisi-prj-select-file "ada-mode 7.0")

(cl-defmethod wisi-prj-select ((project ada-prj))
  (when (plist-get (ada-prj-plist project) 'el_file)
    (load-file (plist-get (ada-prj-plist project) 'el_file)))

  (setq compilation-search-path (plist-get (ada-prj-plist project) 'src_dir))

  (ada-compiler-select-prj (ada-prj-compiler project) project)
  (ada-xref-select-prj     (ada-prj-xref project)     project)

  (let ((func (cdr (assq (plist-get (ada-prj-plist project) 'xref_tool) ada-select-prj-xref-tool))))
    (when func (funcall func project)))
  )

(cl-defmethod wisi-prj-deselect ((project ada-prj))
  (ada-xref-deselect-prj (ada-prj-xref project) project)
  (ada-compiler-deselect-prj (ada-prj-compiler project) project)

  (let ((func (cdr (assq (plist-get (ada-prj-plist project) 'xref_tool)
			 ada-deselect-prj-xref-tool))))
    (when func (funcall func)))
  )

(cl-defmethod wisi-prj-refresh-cache :after ((project ada-prj) not-full)
  (ada-xref-refresh-cache (ada-prj-xref project) not-full))

(cl-defmethod project-roots ((_project ada-prj))
  ;; Not meaningful
  nil)

(cl-defmethod project-files ((project ada-prj) &optional dirs)
  (let (result)
    (dolist (dir (or dirs
		     (plist-get (ada-prj-plist project) 'src_dir)))
      (mapc
       (lambda (absfile)
	 (when (and (not (string-equal "." (substring absfile -1)))
		    (not (string-equal ".." (substring absfile -2)))
		    (not (file-directory-p absfile))
                    (or (null (ada-prj-file-pred project))
			(funcall (ada-prj-file-pred project) absfile)))
	   (push absfile result)))
       (directory-files dir t)))
    result))

(when (not (fboundp 'project--read-file-cpd-relative)) ;; emacs < 27
  (cl-defmethod project-file-completion-table ((project ada-prj) &optional dirs)
    (apply-partially #'uniq-file-completion-table (uniq-file-uniquify (project-files project dirs)))))

(defun ada-create-select-default-prj (&optional directory)
  "Create a default project with src_dir set to DIRECTORY (default current directory), select it."
  ;; FIXME: if move src_dir to wisi-prj, move this there.
  (let* ((dir (or directory default-directory))
	 (prj-file (expand-file-name "default_.adp" dir)) ;; we assume this does not exist
	 (project (ada-prj-default dir)))

    ;; Do this here so wisi-prj-select-file will not try to parse the
    ;; project file.
    (if (assoc prj-file wisi-prj-alist)
	(setcdr (assoc prj-file wisi-prj-alist) project)
      (add-to-list 'wisi-prj-alist (cons prj-file project)))

    (wisi-prj-select-file prj-file)
    ))

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

;; FIXME: move to project menu to wisi, provide delete
(defun ada-project-menu-compute ()
  "Return an easy-menu menu for `ada-project-menu-install'.
Menu displays currently parsed Ada mode projects."
  (let (menu)
    (dolist (item wisi-prj-alist)
      (push
       (vector
	(if (equal (car item) wisi-prj--current-file)
	    ;; current project
	    (concat (car item) "  *")
	  (car item))
	`(lambda () (interactive) (wisi-prj-select ,(cdr item)))
	t)
       menu)
      )
    (nreverse menu)))

;;;; initialization
(mapc
 (lambda (ext)
   (push (cons ext #'ada-prj-parse-file) wisi-prj-parser-alist)
   (push (cons ext #'ada-prj-default) wisi-prj-default-alist))
 '("adp" "prj"))

(add-to-list 'wisi-prj-file-extensions '("adp" "prj"))


(provide 'ada-core)
;; ada-core.el ends here
