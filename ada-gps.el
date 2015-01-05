;; Emacs ada-mode indentation engine, using GPS code in a subprocess.
;;
;; GNAT is provided by AdaCore; see http://libre.adacore.com/
;;
;;; Copyright (C) 2014  Free Software Foundation, Inc.
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

(require 'ada-mode)

(defcustom ada-gps-size-threshold 100000
  "Max size (in characters) for using ada-wisi indentation engine."
  :type 'integer
  :group 'ada-indentation
  :safe 'integerp)

(defvar ada-gps-debug 0)

;;;;; sessions

;; ada_mode_gps_indent runs a loop, waiting for indentation requests.
;;
;; We only need one process; there is no persistent state.

(cl-defstruct (ada-gps--session)
  (process nil) ;; running gpr_query
  (buffer nil))  ;; receives output of gpr_query

(defvar ada-gps-session
  (make-ada-gps--session)
  "The single instance of ada-gps--session")

(defconst ada-gps-buffer-name " *ada_gps*")

;; ada-gps-exec declared in ada-mode for auto-detection of indent engine

(defun ada-gps--start-process ()
  "Start the session process running ada_gps."
  (unless (buffer-live-p (ada-gps--session-buffer ada-gps-session))
    ;; user may have killed buffer
    (setf (ada-gps--session-buffer ada-gps-session) (get-buffer-create ada-gps-buffer-name)))

  (let ((exec-file (locate-file ada-gps-exec exec-path '("" ".exe"))))
    (unless exec-file
      (error "%s not found on `exec-path'" ada-gps-exec))

    (with-current-buffer (ada-gps--session-buffer ada-gps-session)
      (erase-buffer); delete any previous messages, prompt
      (setf (ada-gps--session-process ada-gps-session)
	    (start-process ada-gps-buffer-name (current-buffer) exec-file))
      (set-process-query-on-exit-flag (ada-gps--session-process ada-gps-session) nil)
      (ada-gps-session-wait)

      ;; check for warnings about invalid directories etc
      (goto-char (point-min))
      (when (search-forward "warning:" nil t)
	(error "ada_gps warnings"))
      )))

(defun ada-gps-require-session ()
  "Create ada-gps session if not active."
  (unless (and (ada-gps--session-process ada-gps-session)
	       (process-live-p (ada-gps--session-process ada-gps-session)))
   (ada-gps--start-process)))

(defconst ada-gps-prompt "^GPS_Indent> $"
  "Regexp matching ada_mode_gps_indent prompt; indicates previous command is complete.")

(defun ada-gps-session-wait ()
  "Wait for the current command to complete."
  (unless (process-live-p (ada-gps--session-process ada-gps-session))
    (ada-gps-show-buffer)
    (error "ada-gps process died"))

  (with-current-buffer (ada-gps--session-buffer ada-gps-session)
    (let ((process (ada-gps--session-process ada-gps-session))
	  (search-start (point-min))
	  (wait-count 0)
	  (found nil))
      (while (and (process-live-p process)
		  (progn
		    ;; process output is inserted before point, so move back over it to search it
		    (goto-char search-start)
		    (not (setq found (re-search-forward ada-gps-prompt (point-max) t)))))
	(setq search-start (point));; don't search same text again
	(setq wait-count (1+ wait-count))
	(accept-process-output process 0.1))
      (if found
	  (when (> ada-gps-debug 0)
	    (message "ada-gps-session-wait: %d" wait-count)
	    (message "'%s'" (buffer-substring-no-properties (point-min) (point-max))))

	(ada-gps-show-buffer)
	(error "ada_gps process died"))
      )))

(defun ada-gps-session-send (cmd wait prefix)
  "Send CMD to ada_gps session.
If WAIT is non-nil, wait for command to complete.
If PREFIX is non-nil, prefix with count of bytes in cmd."
  (ada-gps-require-session)
  ;; we don't wait for previous command to complete, because the
  ;; previous call to ada-gps-session-cmd might have been a partial
  ;; command string (in particular, for 'compute_indent').
  (let* ((byte-count-img (when prefix (format "%02d" (string-bytes cmd))))
	 (msg (concat byte-count-img cmd)))
    (when (and (> ada-gps-debug 0)
	       (< (length msg) 100))
      (message msg))
    (with-current-buffer (ada-gps--session-buffer ada-gps-session)
      (erase-buffer)
      (process-send-string (ada-gps--session-process ada-gps-session) msg)
      (when wait
	(ada-gps-session-wait))
      )))

(defun ada-gps-kill-session ()
  (interactive)
  (when (process-live-p (ada-gps--session-process ada-gps-session))
    (process-send-string (ada-gps--session-process ada-gps-session) "04exit")
    ))

(defun ada-gps-show-buffer ()
  "Show ada-gps buffer."
  (interactive)
  (if (ada-gps--session-buffer ada-gps-session)
      (pop-to-buffer (ada-gps--session-buffer ada-gps-session)))
  (error "ada-gps session not active")))

;;;;; indenting

(defun ada-gps-indent-line ()
  "Indent current line using the ada-gps indentation engine."
  (interactive)
  (let ((savep (point))
	indent)
    (save-excursion
      (back-to-indentation)
      (when (>= (point) savep) (setq savep nil))

      ;; send complete current line
      (end-of-line)
      (ada-gps-session-send
       (format "compute_indent %d %d" (line-number-at-pos) (1- (position-bytes (point)))) nil t)
      (ada-gps-session-send (buffer-substring-no-properties (point-min) (point)) t nil)
      )
    (with-current-buffer (ada-gps--session-buffer ada-gps-session)
      (goto-char (point-min))
      (if (looking-at (concat " *\\([0-9]+\\)$"))
	  (setq indent (string-to-number (match-string 1)))
	;; gps did not compute indent for some reason
	(when (> ada-gps-debug 0)
	  (message "ada-gps returned '%s'" (buffer-substring-no-properties (point-min) (point-max))))
	(setq indent 0))
      )

    (if savep
	;; point was inside line text; leave it there
	(save-excursion (indent-line-to indent))
      ;; point was before line text; move to start of text
      (indent-line-to indent))
    ))


(defun ada-gps-setup ()
  "Set up a buffer for indenting with ada-gps."
  (set (make-local-variable 'indent-line-function) 'ada-gps-indent-line)
  (ada-gps-require-session)

  ;; FIXME: need this?
  ;; (set (make-local-variable 'comment-indent-function) 'wisi-comment-indent)

  ;; FIXME: fontify 'return' (see ada-wisi-post-local-vars)
  )

(require 'ada-wisi)

(defun ada-gps-or-wisi-setup ()
  "If buffer size > `ada-gps-size-threshold', use ada-gps; otherwise use ada-wisi indentation engine,"
  (if (> (point-max) ada-gps-size-threshold)
      (progn
	(ada-gps-setup)

	;; locally clear global function pointers set by loading ada-wisi
	(set (make-local-variable 'ada-fix-context-clause) nil)
	(set (make-local-variable 'ada-goto-declaration-start) nil)
	(set (make-local-variable 'ada-goto-declaration-end) nil)
	(set (make-local-variable 'ada-goto-declarative-region-start) nil)
	(set (make-local-variable 'ada-goto-end) nil)
	(set (make-local-variable 'ada-in-paramlist-p) nil)
	(set (make-local-variable 'ada-indent-statement) nil)
	(set (make-local-variable 'ada-make-subprogram-body) nil)
	(set (make-local-variable 'ada-next-statement-keyword) nil)
	(set (make-local-variable 'ada-prev-statement-keyword) nil)
	(set (make-local-variable 'ada-reset-parser) nil)
	(set (make-local-variable 'ada-scan-paramlist) nil)
	(set (make-local-variable 'ada-show-parse-error) nil)
	(set (make-local-variable 'ada-which-function) nil)
	)

    (ada-wisi-setup))
  )

(unless (locate-file ada-gps-exec exec-path '("" ".exe"))
  (error "%s not found on `exec-path'" ada-gps-exec))

(add-hook 'ada-mode-hook 'ada-gps-or-wisi-setup)
(setq ada-mode-hook (delq 'ada-wisi-setup ada-mode-hook))

(provide 'ada-gps)
(provide 'ada-indent-engine)

;; end of file
