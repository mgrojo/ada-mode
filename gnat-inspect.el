;;; gnat-inspect.el --- minor-mode for navigating sources using the
;;; AdaCore cross reference tool gnatinspect.
;;;
;;; gnatinspect supports Ada and any gcc language that supports the
;;; -fdump-xref switch (which includes C, C++).
;;
;;; Copyright (C) 2013  Free Software Foundation, Inc.

;; Author: Stephen Leake <stephen_leake@member.fsf.org>
;; Maintainer: Stephen Leake <stephen_leake@member.fsf.org>
;; Keywords: languages Ada C C++ cross-reference

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
;; M-x gnat-inspect

(require 'compile)

(defgroup gnat-inspect nil
  "Minor mode for navigating sources using GNAT cross reference tool `gnatinspect'."
  :group 'languages)

(defun gnat-inspect-dist (found-line line found-col col)
  "Return non-nil if found-line, -col is closer to line, col than min-distance."
  (+ (abs (- found-line line))
     (* (abs (- found-col col)) 250)))

(defun gnat-inspect-other (identifier file line col)
  "For `ada-xref-other-function', using gnatinspect."
  (unless (ada-prj-get 'gpr_file)
    (error "no gnat project file defined."))

  (when (eq ?\" (aref identifier 0))
    ;; gnatinspect wants the quotes stripped
    (setq col (+ 1 col))
    (setq identifier (substring identifier 1 (1- (length identifier))))
    )

  (with-current-buffer (gnat-run-buffer)
    (let ((project-file (file-name-nondirectory
			 (or (ada-prj-get 'gnat_inspect_gpr_file)
			     (ada-prj-get 'gpr_file))))
	  (arg (format "%s:%s:%d:%d" identifier (file-name-nondirectory file) line col))
	  (regexp "\\(.*\\):\\(.*\\):\\([0-9]+\\):\\([0-9]+\\) (\\(.*\\))")
	  (decl-loc nil)
	  (body-loc nil)
	  (search-type nil)
	  (min-distance (1- (expt 2 29)))
	  (result nil))

      ;; 'gnatinspect refs' returns a list containing the declaration,
      ;; the body, and all the references, in no particular order.
      ;;
      ;; We search the list, looking for the input location,
      ;; declaration and body, then return the declaration or body as
      ;; appropriate.
      ;;
      ;; the format of each line is name:file:line:column (type) scope=name:file:line:column
      ;;                            1    2    3    4       5
      ;;
      ;; 'type' can be:
      ;;   body
      ;;   declaration
      ;;   full declaration  (for a private type)
      ;;   implicit reference
      ;;   reference
      ;;   static call
      ;;
      ;; Module_Type:/home/Projects/GDS/work_stephe_2/common/1553/gds-hardware-bus_1553-wrapper.ads:171:9 (full declaration) scope=Wrapper:/home/Projects/GDS/work_stephe_2/common/1553/gds-hardware-bus_1553-wrapper.ads:49:31
      ;;
      ;; itc_assert:/home/Projects/GDS/work_stephe_2/common/itc/opsim/itc_dscovr_gdsi/Gds1553/src/Gds1553.cpp:830:9 (reference) scope=Gds1553WriteSubaddress:/home/Projects/GDS/work_stephe_2/common/itc/opsim/itc_dscovr_gdsi/Gds1553/inc/Gds1553.hpp:173:24
      (message "running gnatinspect ...")

      ;; gnatinspect from gnatcoll-1.6w-20130902 can't handle aggregate projects; M910-032
      (gnat-run "gnatinspect" (list "-P" project-file "-c" (concat "refs " arg))
		"can't handle aggregate projects? - use gnat_inspect_gpr_file")
      (message "running gnatinspect ... done.")
      (message "parsing result ...")

      (goto-char (point-min))

      (while (not (eobp))
	(cond
	 ((looking-at regexp)
	  ;; process line
	  (let* ((found-file (file-name-nondirectory (match-string 2)))
		 (found-line (string-to-number (match-string 3)))
		 (found-col  (string-to-number (match-string 4)))
		 (found-type (match-string 5))
		 (dist       (gnat-inspect-dist found-line line found-col col))
		 )

	    (when (string-equal found-type "declaration")
	      (setq decl-loc (list found-file found-line (1- found-col))))

	    (when (or
		   (string-equal found-type "body")
		   (string-equal found-type "full declaration"))
	      (setq body-loc (list found-file found-line (1- found-col))))

	    (when
		;; In general, we don't know where in the gnatinspect
		;; output the search item occurs, so we search for it.
		;;
		;; We use the same distance algorithm as gnatinspect
		;; to allow a fuzzy match on edited code.
		(and (equal found-file file)
		     (< dist min-distance))
	      (setq min-distance dist)
	      (setq search-type found-type))
	    ))

	 (t ;; ignore line
	  ;;
	  ;; This skips GPR_PROJECT_PATH and echoed command at start of buffer.
	  ;;
	  ;; It also skips warning lines. For example,
	  ;; gnatcoll-1.6w-20130902 can't handle the Auto_Text_IO
	  ;; language, because it doesn't use the gprconfig
	  ;; configuration project. That gives lines like:
	  ;;
	  ;; common_text_io.gpr:15:07: language unknown for "gds-hardware-bus_1553-time_tone.ads"
	  ;;
	  ;; There are probably other warnings that might be reported as well.
	  )
	 )
	(forward-line 1)
	)

      (cond
       ((null search-type)
	(pop-to-buffer (current-buffer))
	(error "gnatinspect did not return other item"))

       ((and
	 (string-equal search-type "declaration")
	 body-loc)
	(setq result body-loc))

       (decl-loc
	(setq result decl-loc))
       )

      (when (null result)
	(pop-to-buffer (current-buffer))
	(error "gnatinspect did not return other item"))

      (message "parsing result ... done")
      result)))

(defconst gnat-inspect-ident-file-regexp
  ".*:\\(.*\\):\\([0123456789]+\\):\\([0123456789]+\\)"
  "Regexp matching <idendifier>:<file>:<line>:<column>")

(defconst gnat-inspect-ident-file-regexp-alist
  ;; Write_Message:C:\Projects\GDS\work_dscovr_release\common\1553\gds-mil_std_1553-utf.ads:252:25
  (list (concat "^" gnat-inspect-ident-file-regexp) 1 2 3)
  "For compilation-error-regexp-alist, matching `gnatinspect overriding_recursive' output")

(defun gnat-inspect-scope-secondary-error ()
  "Return ada-secondary-error 'face' value for compilation-error-regexp-alist submatch"
  (list
   'face
   compilation-info-face
   'ada-secondary-error
   (list
    file
    (string-to-number (match-string-no-properties 5)); line
    (string-to-number (match-string-no-properties 6)) ;; column
    )))

(defconst gnat-inspect-ident-file-scope-regexp-alist
  ;; RX_Enable:C:\common\1553\gds-hardware-bus_1553-raw_read_write.adb:163:13 (write reference) scope=New_Packet_TX:C:\common\1553\gds-hardware-bus_1553-raw_read_write.adb:97:14

  (list (concat
	 gnat-inspect-ident-file-regexp
	 " (.*) "
	 "scope="
;;	 gnat-inspect-ident-file-regexp
	 )
	1 2 3 ;; file line column
	;; 1 ;; hyperlink
	;;(list 4 'gnat-inspect-scope-secondary-error)
	)
  "For compilation-error-regexp-alist, matching `gnatinspect refs' output")

;; debugging:
;; (setq compilation-error-regexp-alist-alist (cons 'gnat-inspect-ident-file-scope gnat-inspect-ident-file-scope-regexp-alist))

(defun gnat-inspect-compilation (identifier file line col cmd comp-err)
  "Run gnatinspect identifier:file:line:col cmd, in compilation-mode.
Uses a separate compilation buffer to run gnatinspect, with
`compilation-error-regexp-alist' set to COMP-ERR."
  ;; WORKAROUND: gnatinspect from gnatcoll-1.6w can't handle aggregate
  ;; projects, so we use an alternate project file for gnatinspect
  ;; queries, specified in the Emacs ada-mode project file by
  ;; "gnat_inspect_gpr_file".

  (with-current-buffer (gnat-run-buffer); for default-directory
    (let* ((project-file (file-name-nondirectory
			  (or (ada-prj-get 'gnat_inspect_gpr_file)
			      (ada-prj-get 'gpr_file))))
	   (arg (format "%s:%s:%d:%d" identifier file line col))
	   (cmd-1
		(concat "gnatinspect --project=" project-file
			" --nightlydb=gnatinspect.db"
			" -c \"" cmd " " arg "\""))
	   )
      (unless project-file
	(error "no gnatinspect project file defined."))

      (let ((process-environment (ada-prj-get 'proc_env)) ;; for GPR_PROJECT_PATH
	    (compilation-error "reference")
	    (compilation-mode-hook
	     (lambda ()
	       (set (make-local-variable 'compilation-filter) nil)
	       (set (make-local-variable 'compilation-error-regexp-alist) (list comp-err))
	       )))

	(compilation-start cmd-1
			   'compilation-mode
			   (lambda (mode-name) (concat mode-name "-gnatinspect-" cmd)))
	)
      )))

(defun gnat-inspect-all (identifier file line col)
  "For `ada-xref-all-function', using gnatinspect."
  ;; This will in general return a list of references, so we use
  ;; `compilation-start' to run gnatinspect, so the user can navigate
  ;; to each result in turn via `next-error'.
  (gnat-inspect-compilation identifier file line col "refs" 'gnat-inspect-ident-file-scope))

(defun gnat-inspect-parents (identifier file line col)
  "For `ada-xref-parent-function', using gnatinspect."
  ;; This will in general return a list of references, so we use
  ;; `compilation-start' to run gnatinspect, so the user can navigate
  ;; to each result in turn via `next-error'.
  (gnat-inspect-compilation identifier file line col "parent_types" 'gnat-inspect-ident-file))

(defun gnat-inspect-overriding (identifier file line col)
  "For `ada-xref-overriding-function', using gnatinspect."
  ;; This will in general return a list of references, so we use
  ;; `compilation-start' to run gnatinspect, so the user can navigate
  ;; to each result in turn via `next-error'.
  (gnat-inspect-compilation identifier file line col "overridden_recursive" 'gnat-inspect-ident-file))

(defun gnat-inspect-goto-declaration (other-window)
  "Move to the declaration or body of the identifier around point.
If at the declaration, go to the body, and vice versa. If at a
reference, goto the declaration.

If OTHER-WINDOW (set by interactive prefix) is non-nil, show the
buffer in another window."
  (interactive "P")

  (let ((target
	 (gnat-inspect-other
	  (thing-at-point 'symbol)
	  (buffer-file-name)
	  (line-number-at-pos)
	  (save-excursion
	    (goto-char (car (bounds-of-thing-at-point 'symbol)))
	    (1+ (current-column)))
	  )))

    (ada-goto-source (nth 0 target)
		     (nth 1 target)
		     (nth 2 target)
		     other-window)
    ))

(defvar gnat-inspect-map
  (let ((map (make-sparse-keymap)))
    ;; C-c <letter> are reserved for users

    (define-key map "\C-c\C-d" 'gnat-inspect-goto-declaration)
    ;; FIXME: (define-key map "\C-c\M-d" 'gnat-inspect-parents)
    ;; FIXME: overriding
    (define-key map "\C-c\C-r" 'gnat-inspect-all)
    map
  )  "Local keymap used for GNAT inspect minor mode.")

;; FIXME: define menu

(define-minor-mode gnat-inspect
  "Minor mode for navigating sources using GNAT cross reference tool.
Enable mode if ARG is positive"
  :initial-value t
  :lighter       " gnat-inspect"   ;; mode line

  ;; just enable the menu and keymap
  )

;;;;; support for Ada mode

(defun ada-gnat-inspect-select-prj ()
  (setq ada-file-name-from-ada-name 'ada-gnat-file-name-from-ada-name)
  (setq ada-ada-name-from-file-name 'ada-gnat-ada-name-from-file-name)
  (setq ada-make-package-body       'ada-gnat-make-package-body)

  (add-hook 'ada-syntax-propertize-hook 'gnatprep-syntax-propertize)

  ;; must be after indentation engine setup, because that resets the
  ;; indent function list.
  (add-hook 'ada-mode-hook 'ada-gnat-inspect-setup t)

  (setq ada-xref-all-function        'gnat-inspect-all)
  (setq ada-xref-other-function      'gnat-inspect-other)
  (setq ada-xref-parent-function     'gnat-inspect-parents)
  (setq ada-xref-all-function        'gnat-inspect-all)
  (setq ada-xref-overriding-function 'gnat-inspect-overriding)

  (add-to-list 'completion-ignored-extensions ".ali") ;; gnat library files, used for cross reference
  )

(defun ada-gnat-inspect-deselect-prj ()
  (setq ada-file-name-from-ada-name nil)
  (setq ada-ada-name-from-file-name nil)
  (setq ada-make-package-body       nil)

  (setq ada-syntax-propertize-hook (delq 'gnatprep-syntax-propertize ada-syntax-propertize-hook))
  (setq ada-mode-hook (delq 'ada-gnat-inspect-setup ada-mode-hook))

  (setq ada-xref-other-function      nil)
  (setq ada-xref-parent-function     nil)
  (setq ada-xref-all-function        nil)
  (setq ada-xref-overriding-function nil)

  (setq completion-ignored-extensions (delete ".ali" completion-ignored-extensions))
  )

(defun ada-gnat-inspect-setup ()
  (when (boundp 'wisi-indent-calculate-functions)
    (add-to-list 'wisi-indent-calculate-functions 'gnatprep-indent))
  )

(defun ada-gnat-inspect ()
  "Set Ada mode global vars to use gnatinspect."
  (require 'ada-mode)

  (add-to-list 'ada-prj-parser-alist       '("gpr" . gnat-parse-gpr))
  (add-to-list 'ada-select-prj-xref-tool   '(gnat_inspect  . ada-gnat-inspect-select-prj))
  (add-to-list 'ada-deselect-prj-xref-tool '(gnat_inspect  . ada-gnat-inspect-deselect-prj))

  ;; no parse-*-xref

  (font-lock-add-keywords 'ada-mode
   ;; gnatprep preprocessor line
   (list (list "^[ \t]*\\(#.*\n\\)"  '(1 font-lock-type-face t))))

  (add-hook 'ada-gnat-fix-error-hook 'ada-gnat-fix-error)
  )

(provide 'gnat-inspect)

(add-to-list 'compilation-error-regexp-alist-alist
	     (cons 'gnat-inspect-ident-file       gnat-inspect-ident-file-regexp-alist))
(add-to-list 'compilation-error-regexp-alist-alist
	     (cons 'gnat-inspect-ident-file-scope gnat-inspect-ident-file-scope-regexp-alist))

(ada-gnat-inspect)

(unless (and (boundp 'ada-xref-tool)
	     (default-value 'ada-xref-tool))
  (setq ada-xref-tool 'gnat_inspect))

;;; end of file
