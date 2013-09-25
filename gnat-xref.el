;;; gnat-xref.el --- minor-mode for navigating sources using the
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
;; M-x gnat-xref

(defgroup gnat-xref nil
  "Minor mode for navigating sources using GNAT cross reference tool `gnatinspect'."
  :group 'languages)

(defun gnat-xref-other (identifier file line col)
  ;; For `ada-xref-other-function', using gnatinspect.
  "Move to the declaration or body of IDENTIFIER, which is at the location FILE LINE COL.
FILE must be absolute. If the location is the declaration, go to
the body, and vice versa. If the location is a reference, goto
the declaration."

  (unless (ada-prj-get 'gpr_file)
    (error "no gnat project file defined."))

  (with-current-buffer (ada-gnat-run-buffer)
    (let ((project-file (file-name-nondirectory
			 (or (ada-prj-get 'gnatinspect_gpr_file)
			     (ada-prj-get 'gpr_file))))
	  (arg (format "%s:%s:%d:%d" identifier (file-name-nondirectory file) line col))
	  status
	  (regexp "\\(.*\\):\\(.*\\):\\([0-9]+\\):\\([0-9]+\\) (\\(.*\\))")
	  (decl-loc nil)
	  (body-loc nil)
	  (want 'unknown) ;; 'decl 'body
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
      (setq status (ada-gnat-run "gnatinspect" (list "-P" project-file "-c" (concat "refs " arg))))
      (message "running gnatinspect ... done.")
      (message "parsing result ...")

      (cond
       ((= status 0); success
	(goto-char (point-min))

	(while (not result)
	  (cond
	   ((eobp)
	    (pop-to-buffer (current-buffer))
	    (error "gnatinspect did not return other item"))

	   ((looking-at regexp)
	    ;; process line
	    (let ((found-file (match-string 2))
		  (found-line (string-to-number (match-string 3)))
		  (found-col  (string-to-number (match-string 4)))
		  (found-type (match-string 5)))

	      (when (string-equal found-type "declaration")
		(setq decl-loc (list found-file found-line (1- found-col))))

	      (when (or
		     (string-equal found-type "body")
		     (string-equal found-type "full declaration"))
		(setq body-loc (list found-file found-line (1- found-col))))

	      (when
		  (and (eq want 'unknown)
		       (equal found-file file)
		       (= found-line line)
		       (= found-col col))
		;; search item
		(cond
		 ((string-equal found-type "declaration")
		  (setq want 'body))

		 (t
		  (setq want 'decl))

		 ))
	      (cl-ecase want
		(unknown)

		(decl (when decl-loc (setq result decl-loc)))

		(body (when body-loc (setq result body-loc)))
		)
	      ))

	   (t ;; ignore line
	    ;;
	    ;; This skips ADA_PROJECT_PATH and echoed command at start of buffer.
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
	  ))

       (t ; failure
	(pop-to-buffer (current-buffer))
	;; gnatinspect from gnatcoll-1.6w-20130902 can't handle aggregate projects; M910-032
	(error "gnatinspect failed; can't handle aggregate projects? - use gnatinspect_gpr_file"))
       )
      (message "parsing result ... done")
      result)))

(defconst gnat-xref-overriding-regexp-alist
  ;; Write_Message:C:\Projects\GDS\work_dscovr_release\common\1553\gds-mil_std_1553-utf.ads:252:25
  (list "^.*:\\(.*\\):\\([0123456789]+\\):\\([0123456789]+\\)" 1 2 3)
  "Regexp for compilation-error-regexp-alist, matching `gnatinspect overriding_recursive' output")

(defun gnat-xref-overriding (identifier file line col)
  "For `ada-xref-overriding-function', using gnatinspect."
  ;; This will in general return a list of references, so we use
  ;; `compilation-start' to run gnatinspect, so the user can navigate
  ;; to each result in turn via `next-error'.
  ;;
  ;; WORKAROUND: gnatinspect from gnatcoll-1.6w can't handle aggregate
  ;; projects, so we use an alternate project file for gnatinspect
  ;; queries, specified in the Emacs ada-mode project file by
  ;; "gnatinspect_gpr_file".

  (with-current-buffer (ada-gnat-run-buffer)
    (let* ((project-file (file-name-nondirectory
			  (or (ada-prj-get 'gnatinspect_gpr_file)
			      (ada-prj-get 'gpr_file))))
	   (arg (format "%s:%s:%d:%d" identifier file line col))
	   (cmd (concat "gnatinspect --project=" project-file
			" --nightlydb=gnatinspect.db"
			" -c \"overridden_recursive " arg "\"")))

      (unless project-file
	(error "no gnatinspect project file defined."))

      (with-current-buffer (ada-gnat-run-buffer); for default-directory
	(let ((compilation-environment (ada-prj-get 'proc_env)) ;; for ADA_PROJECT_PATH
	      (compilation-error "reference")
	      (compilation-mode-hook
	       (lambda () (set (make-local-variable 'compilation-error-regexp-alist) (list 'gnatinspect-overriding)))))
	  (compilation-start cmd
			     'compilation-mode
			     (lambda (mode-name) (concat mode-name "-gnatinspect-overriding")))
	  ))
      )))

(defun gnat-xref-goto-declaration (other-window-frame)
  "Move to the declaration or body of the identifier around point.
If at the declaration, go to the body, and vice versa. If at a
reference, goto the declaration.

OTHER-WINDOW-FRAME (default nil, set by interactive prefix)
controls window and frame choice:

nil     : show in current window
C-u     : show in other window
C-u C-u : show in other frame
"
  (interactive "P")

  (let ((target
	 (gnat-xref-other
	  (thing-at-point 'symbol)
	  (buffer-file-name)
	  (line-number-at-pos)
	  (save-excursion
	    (goto-char (car (bounds-of-thing-at-point 'symbol)))
	    (1+ (current-column))))))

    (ada-goto-source (nth 0 target)
		     (nth 1 target)
		     (nth 2 target)
		     other-window-frame)
    ))

(defun gnat-xref-goto-declaration-parent ()
  "Move to the parent type declaration of the type identifier around point."
  (interactive)
  (error "not implemented"))

(defvar-local gnat-xref-map
  (let ((map (make-sparse-keymap)))
    ;; C-c <letter> are reserved for users

    (define-key map "\C-c\C-d" 'gnat-xref-goto-declaration)
    (define-key map "\C-c\M-d" 'gnat-xref-goto-declaration-parent)
    (define-key map "\C-c\C-r" 'gnat-xref-all-references)
    map
  )  "Local keymap used for GNAT xref minor mode.")

(define-minor-mode gnat-xref
  "Minor mode for navigating sources using GNAT cross reference tool.
Enable mode if ARG is positive"
  :initial-value t
  :lighter       " gnat-xref"   ;; mode line

  (if gnat-xref
      ;; enable use of gnatinspect
      (progn
	(when (boundp 'ada-xref-tool)
	  ;; for ada-mode
	  (setq         ada-xref-tool                 'gnatinspect)
	  (add-to-list 'ada-xref-overriding-function  (cons 'gnatinspect 'gnat-xref-overriding))
	  ))

    ;; disable use of gnatinspect
    (when (boundp 'ada-xref-tool)
      ;; for ada-mode
      (setq ada-xref-tool nil)
      ;; no need to delete from lists
      )
    ))

(provide 'gnat-xref)

(add-to-list 'compilation-error-regexp-alist-alist
	     (cons 'gnatinspect-overriding gnat-xref-overriding-regexp-alist))

;;; end of file
