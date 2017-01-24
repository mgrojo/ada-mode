;; Emacs ada-mode indentation engine, using GPS code in a subprocess.
;;
;; GNAT is provided by AdaCore; see http://libre.adacore.com/
;;
;;; Copyright (C) 2014, 2017  Free Software Foundation, Inc.
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
  "Max size (in characters) for using ada-wisi indentation engine.
Buffers larger than this will use ada-gps indentation engine,
which is faster on large buffers."
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

;; ada-gps-indent-exec declared in ada-mode for auto-detection of indent engine

(defun ada-gps--start-process ()
  "Start the session process running ada_gps."
  (unless (buffer-live-p (ada-gps--session-buffer ada-gps-session))
    ;; user may have killed buffer
    (setf (ada-gps--session-buffer ada-gps-session) (get-buffer-create ada-gps-buffer-name)))

  (let ((exec-file (locate-file ada-gps-indent-exec exec-path '("" ".exe"))))
    (unless exec-file
      (error "%s not found on `exec-path'" ada-gps-indent-exec))

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

(defun ada-gps-show-proc-id ()
  "Display ada-gps process id, for attaching with debugger."
  (interactive)
  (if (process-live-p (ada-gps--session-process ada-gps-session))
      (message "ada-gps process id: %d" (process-id (ada-gps--session-process ada-gps-session)))
    (message "ada-gps process not live")
    ))

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
  (error "ada-gps session not active"))

;;;;; indenting

(defun ada-gps-send-params ()
  "Send indentation params to current gps session."
  (ada-gps-session-send
   (format "set_params %d %d %d" ada-indent ada-indent-broken ada-indent-when)
   t t))

(defconst ada-gps-output-regexp " *\\([0-9]+\\) +\\([0-9]+\\)$"
  "Matches gps process output for one line.")

(defun ada-gps-indent-compute ()
  "For `wisi-indent-fallback'; compute indent for current line."

  ;; always send indent parameters - we don't track what buffer we are in
  (ada-gps-send-params)

  (save-excursion
    ;; send complete current line
    (end-of-line)
    (ada-gps-session-send
     (format "compute_indent %d %d" (line-number-at-pos) (1- (position-bytes (point)))) nil t)
    (ada-gps-session-send (buffer-substring-no-properties (point-min) (point)) t nil)
    )
  (with-current-buffer (ada-gps--session-buffer ada-gps-session)
    (goto-char (point-min))
    (if (looking-at ada-gps-output-regexp)
	(string-to-number (match-string 2))

      ;; gps did not compute indent for some reason
      (when (> ada-gps-debug 0)
	(message "ada-gps returned '%s'" (buffer-substring-no-properties (point-min) (point-max))))
      0)
    ))

(defun ada-gps-indent-line ()
  "Indent current line using the ada-gps indentation engine."
  (interactive)
  (let ((savep (point))
	(indent (ada-gps-indent-compute)))

    (save-excursion
      (back-to-indentation)
      (when (>= (point) savep)
	(setq savep nil)))

    (if savep
	;; point was inside line text; leave it there
	(save-excursion (indent-line-to indent))
      ;; point was before line text; move to start of text
      (indent-line-to indent))
    ))

(defun ada-gps-indent-region (start end)
  "For `indent-region-function'; indent lines in region START END using GPS."

  ;; always send indent parameters - we don't track what buffer we are in
  (ada-gps-send-params)

  ;; send complete lines
  (goto-char end)
  (setq end (line-end-position))

  (let ((source-buffer (current-buffer))
	(start-line (line-number-at-pos start))
	(end-line (line-number-at-pos end)))

    (ada-gps-session-send
     (format "compute_region_indent %d %d %d" start-line end-line (1- (position-bytes end))) nil t)
    (ada-gps-session-send (buffer-substring-no-properties (point-min) (point)) t nil)

    (with-current-buffer (ada-gps--session-buffer ada-gps-session)
      ;; buffer contains two numbers per line; Emacs line number,
      ;; indent. Or an error message.
      (goto-char (point-min))
      (while (not (looking-at ada-gps-prompt))
	(if (looking-at ada-gps-output-regexp)
	    (let ((line (string-to-number (match-string 1)))
		  (indent (string-to-number (match-string 2))))
	      (with-current-buffer source-buffer
		(goto-char (point-min))
		(forward-line (1- line))
		(indent-line-to indent))

	      (forward-line 1))

	  ;; else some error message
	  (when (> ada-gps-debug 0)
	    (message "ada-gps returned '%s'" (buffer-substring-no-properties (point-min) (point-max)))
	    (goto-char (point-max)))
	  ))
    )))
;;;;; setup

(defun ada-gps-setup ()
  "Set up a buffer for indenting with ada-gps."
  (set (make-local-variable 'indent-line-function) 'ada-gps-indent-line)
  (set (make-local-variable 'indent-region-function) 'ada-gps-indent-region)
  (ada-gps-require-session)
  )

(require 'ada-wisi)

(defun ada-gps-or-wisi-setup ()
  "If buffer size > `ada-gps-size-threshold', use ada-gps;
otherwise use ada-wisi indentation engine with ada-gps fallback,"
  ;; ada-gps-size-threshold can be set in file-local variables, which
  ;; are parsed after ada-mode-hook runs.
  (add-hook 'hack-local-variables-hook 'ada-gps-post-local-vars nil t))

(defun ada-gps-post-local-vars ()
  "See `ada-gsp-or-wisi-setup'"
  (if (> (point-max) ada-gps-size-threshold)
      (progn
	(ada-gps-setup)

	;; locally clear global function pointers set by loading ada-wisi
	(set (make-local-variable 'ada-fix-context-clause) nil)
	(set (make-local-variable 'ada-goto-declaration-end) nil)
	(set (make-local-variable 'ada-goto-declaration-start) nil)
	(set (make-local-variable 'ada-goto-declarative-region-start) nil)
	(set (make-local-variable 'ada-goto-end) nil)
	(set (make-local-variable 'ada-goto-subunit-name) nil)
	(set (make-local-variable 'ada-in-case-expression) nil)
	(set (make-local-variable 'ada-in-paramlist-p) nil)
	(set (make-local-variable 'ada-indent-statement) nil)
	(set (make-local-variable 'ada-make-subprogram-body) nil)
	(set (make-local-variable 'ada-next-statement-keyword) nil)
	(set (make-local-variable 'ada-on-context-clause) nil)
	(set (make-local-variable 'ada-prev-statement-keyword) nil)
	(set (make-local-variable 'ada-reset-parser) nil)
	(set (make-local-variable 'ada-scan-paramlist) nil)
	(set (make-local-variable 'ada-show-parse-error) nil)
	(set (make-local-variable 'ada-which-function) nil)
	)

    (ada-wisi-setup)
    (set (make-local-variable 'indent-region-function) nil)
    (setq wisi-indent-fallback 'ada-gps-indent-compute)
    ))

(provide 'ada-gps)

(unless (locate-file ada-gps-indent-exec exec-path '("" ".exe"))
  (error "%s not found on `exec-path'" ada-gps-indent-exec))

(add-hook 'ada-mode-hook 'ada-gps-or-wisi-setup)
(setq ada-mode-hook (delq 'ada-wisi-setup ada-mode-hook))

;; end of file
