;;; ada-skel.el --- an extension to Ada mode for inserting statement skeletons

;; Copyright (C) 1987, 1993, 1994, 1996, 1997, 1998, 1999, 2000, 2001, 2002,
;;   2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2013
;;   Free Software Foundation, Inc.

;; Authors: Daniel Pfeiffer
;;	Markus Heritsch
;;	Rolf Ebert <ebert@waporo.muc.de>
;; Maintainer: Stephen Leake <stephen_leake@stephe-leake.org>
;; Keywords: languages, ada

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

;;; Commentary:
;;
;; The primary user command is `ada-skel-expand', which inserts the
;; skeleton associated with the previous word (possibly skipping a
;; name).

;;; History:

;; Created May 1987.
;; Original version from V. Bowman as in ada.el of Emacs-18
;; (borrowed heavily from Mick Jordan's Modula-2 package for GNU,
;; as modified by Peter Robinson, Michael Schmidt, and Tom Perrine.)
;;
;; Sep 1993. Daniel Pfeiffer <pfeiffer@cict.fr> (DP)
;; Introduced statement.el for smaller code and user configurability.
;;
;; Nov 1993. Rolf Ebert <ebert@enpc.fr> (RE) Moved the
;; skeleton generation into this separate file. The code still is
;; essentially written by DP
;;
;; Adapted Jun 1994. Markus Heritsch
;; <Markus.Heritsch@studbox.uni-stuttgart.de> (MH)
;; added menu bar support for templates
;;
;; 1994/12/02  Christian Egli <cegli@hcsd.hac.com>
;; General cleanup and bug fixes.
;;
;; 1995/12/20  John Hutchison <hutchiso@epi.syr.ge.com>
;; made it work with skeleton.el from Emacs-19.30. Several
;; enhancements and bug fixes.
;;
;; Sep 2013 Stephen Leake renamed to ada-skel (to match skeleton.el),
;; added ada-skel-alist (to get some of the functionality of the sadly
;; missed Else package). Also updated for Ada 2012.

(require 'skeleton nil t)

;;;;; skeletons (alphabetical)

;; We don't define skeletons that prompt for most of the content; it
;; is easier just to type in the buffer.
;;
;; These skeletons are not intended to teach a novice the language,
;; just to make it easier to write code that the ada-wisi parser
;; likes, and handle repeated names nicely.

;; no ada-skel-body because package, task, protected bodies are
;; different; need more stuff like ada-make-subprogram-body.

(define-skeleton ada-skel-case
  "Insert case statement."
  ()
  "case " str " is\n"
  "when " _ "=>\n"
  "end case;")

(define-skeleton ada-skel-declare
  "Insert a block statement with an optional name (from `str')."
  ()
  str & ":\n"
  "declare\n"
  _
  "begin\n"
  "exception\n"
  "end " str | -1 ?\;)

(define-skeleton ada-skel-for
  "Insert a for loop statement with an optional name (from `str')."
  ()
  str & ":\n"
  "for " _ " loop\n"
  "end loop " str | -1 ";")

(define-skeleton ada-skel-if
  "Insert an if statement."
  ()
  "if " _ " then\n"
  "elsif  then\n"
  "else\n"
  "end if;")

(define-skeleton ada-skel-loop
  "Insert a loop statement with an optional name (from `str')."
  ()
  str & ":\n"
  "loop\n"
  "exit " str | -1 " when " _ ";\n"
  "end loop " str | -1 ";")

(define-skeleton ada-skel-package
  "Insert a package specification with name from `str'.
See `ada-find-other-file' to create library level package body from spec."
  ()
  "package " str " is\n"
  _
  "private\n"
  "end " str ";")

(define-skeleton ada-skel-record
  "Insert a record type declaration with a type name from `str'."
  ()
  "type " str " is record\n"
  _
  "end record;")

(define-skeleton ada-skel-select
  "Insert a select statement."
  ()
  "select\n"
  _
  "else\n"
  "end select;")

(define-skeleton ada-skel-task
  "Insert a task specification with name from `str'."
  ()
  "task " str " is\n"
  _
  "end " str ";")

(define-skeleton ada-skel-while
  "Insert a while loop statement with an optional name (from `str')."
  ()
  str & ":\n"
  "while " _ " loop\n"
  "end loop " str | -1 ";")

;;;;; token alist

(defconst ada-skel-token-alist
  '(("case" . ada-skel-case)
    ("declare" . ada-skel-declare)
    ("for" . ada-skel-for)
    ("if" . ada-skel-if)
    ("loop" . ada-skel-loop)
    ("package" . ada-skel-package)
    ("record" . ada-skel-record)
    ("select" . ada-skel-select)
    ("task" . ada-skel-task)
    ("while" . ada-skel-while))
  "alist of skeletons, indexed by a string. See `ada-skel-expand'.
The string is normally the first Ada keyword in the skeleton, but can be anything.")

;;;###autoload
(defun ada-skel-expand (&optional name)
  "Expand the word before point to a skeleton, as defined by `ada-skel-token-alist'.
If the word before point is not in `ada-skel-token-alist', assume
it is a name, and use the word before that as the token."
  (interactive "*")
  (let* ((end (prog1 (point) (skip-syntax-backward "w_")))
	 (token (buffer-substring-no-properties (point) end))
	 (skel (cdr (assoc-string token ada-skel-token-alist))))
    (if skel
	(progn
	  ;; delete token and name. point is currently before token.
	  (delete-region
	   (point)
	   (progn
	     (skip-syntax-forward "w_")
	     (when name
	       (skip-syntax-forward " ")
	       (skip-syntax-forward "w_"))
	     (point)))
	  (funcall skel name))

      ;; word at point is not a token; assume it is a name
      (if name
	  ;; already tried that once, don't recurse
	  (error "undefined skeleton token: %s" name)
	(skip-syntax-backward "w_") ;; name
	(skip-syntax-backward " ")
	(ada-skel-expand token)))
    ))

(defun ada-skel-setup ()
  "Set buffer-local values for ada-skel."
  (add-hook 'skeleton-end-hook 'ada-indent-statement nil t))

(provide 'ada-skeletons)
(provide 'ada-skel)

(setq ada-expand 'ada-skel-expand)

(add-hook 'ada-mode-hook 'ada-skel-setup)

;;; ada-skel.el ends here
