;;; wisi-ada-parse.el --- interface to exteral ada_mode_wisi_parse program
;;
;; Copyright (C) 2014  Free Software Foundation, Inc.
;;
;; Author: Stephen Leake <stephen_leake@member.fsf.org>
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

(require 'wisi-parse-common)

(require 'ada-mode)
;; wisi-ada-parse-exec declared in ada-mode for auto-detection of indent engine

(defvar wisi-ada-parse-debug 0)

;;;;; sessions

;; ada_mode_wisi_parse builds internal parser structures on startup,
;; then runs a loop, waiting for parse requests.
;;
;; We only need one process; there is no persistent state.

(cl-defstruct (wisi-ada-parse--session)
  (process nil) ;; running ada_mode_wisi_parse
  (buffer nil))  ;; receives output of ada_mode_wisi_parse

(defvar wisi-ada-parse-session
  (make-wisi-ada-parse--session)
  "The single instance of wisi-ada-parse--session")

(defconst wisi-ada-parse-buffer-name " *ada_mode_wisi_parse*")

(defvar wisi-ada-parse-exec-opts nil
  "list of command-line options for `wisi-ada-parse-exec'.
-v echoes commands.")

(defun wisi-ada-parse--start-process ()
  "Start the session process running ada_mode_wisi_parse."
  (unless (buffer-live-p (wisi-ada-parse--session-buffer wisi-ada-parse-session))
    ;; user may have killed buffer
    (setf (wisi-ada-parse--session-buffer wisi-ada-parse-session) (get-buffer-create wisi-ada-parse-buffer-name)))

  (let ((exec-file (locate-file wisi-ada-parse-exec exec-path '("" ".exe"))))
    (unless exec-file
      (error "%s not found on `exec-path'" wisi-ada-parse-exec))

    (with-current-buffer (wisi-ada-parse--session-buffer wisi-ada-parse-session)
      (erase-buffer); delete any previous messages, prompt
      (setf (wisi-ada-parse--session-process wisi-ada-parse-session)
	    (if wisi-ada-parse-exec-opts
		(start-process wisi-ada-parse-buffer-name (current-buffer) exec-file wisi-ada-parse-exec-opts)
	      (start-process wisi-ada-parse-buffer-name (current-buffer) exec-file)))
      (set-process-query-on-exit-flag (wisi-ada-parse--session-process wisi-ada-parse-session) nil)
      ;; FIXME: check protocol and version numbers?
      (wisi-ada-parse-session-wait)
      )))

(defun wisi-ada-parse-require-session ()
  "Create wisi-ada-parse session if not active."
  (unless (and (wisi-ada-parse--session-process wisi-ada-parse-session)
	       (process-live-p (wisi-ada-parse--session-process wisi-ada-parse-session)))
   (wisi-ada-parse--start-process)))

(defconst wisi-ada-parse-prompt "^;;> "
  "Regexp matching ada_mode_gps_indent prompt; indicates previous command is complete.")

(defun wisi-ada-parse-session-wait ()
  "Wait for the current command to complete."
  (unless (process-live-p (wisi-ada-parse--session-process wisi-ada-parse-session))
    (wisi-ada-parse-show-buffer)
    (error "wisi-ada-parse process died"))

  (with-current-buffer (wisi-ada-parse--session-buffer wisi-ada-parse-session)
    (let ((process (wisi-ada-parse--session-process wisi-ada-parse-session))
	  (search-start (point-min))
	  (wait-count 0)
	  (found nil))
      (while (and (process-live-p process)
		  (progn
		    ;; process output is inserted before point, so move back over it to search it
		    ;; FIXME: could process elisp forms as they arrive
		    ;; FIXME: can take a long time? May need warm fuzzy output
		    (goto-char search-start)
		    (not (setq found (re-search-forward wisi-ada-parse-prompt (point-max) t)))))
	(setq search-start (point));; don't search same text again
	(setq wait-count (1+ wait-count))
	(when (> wisi-ada-parse-debug 0)
	    (message "wisi-ada-parse-session-wait: %d" wait-count))
	(accept-process-output process 0.1))
      (if found
	  (when (> wisi-ada-parse-debug 0)
	    (message "wisi-ada-parse-session-wait: %d" wait-count)
	    (when (> wisi-ada-parse-debug 2)
	      (message "'%s'" (buffer-substring-no-properties (point-min) (point-max)))))

	(wisi-ada-parse-show-buffer)
	(error "ada_mode_wisi_parse process died"))
      )))

(defun wisi-ada-parse-session-send-parse ()
  "Send a parse command to ada_mode_wisi_parse session, followed by the contents of the current buffer.
Does not wait for command to complete."
  (wisi-ada-parse-require-session)

  ;; ada_mode_wisi_parse can't handle non-ASCII, so we don't need string-bytes here.
  (let* ((buf-string (buffer-substring-no-properties (point-min) (point-max)))
	 (cmd (format "parse \"%s\" %d" (buffer-name) (length buf-string)))
	 (msg (format "%02d%s" (length cmd) cmd))
	 (process (wisi-ada-parse--session-process wisi-ada-parse-session)))
    (when (> wisi-ada-parse-debug 0)
      (message msg))
    (with-current-buffer (wisi-ada-parse--session-buffer wisi-ada-parse-session)
      (erase-buffer))
    (process-send-string process msg)

    (process-send-string process buf-string)

    ))

(defun wisi-ada-parse-kill-session ()
  (interactive)
  (when (process-live-p (wisi-ada-parse--session-process wisi-ada-parse-session))
    (process-send-string (wisi-ada-parse--session-process wisi-ada-parse-session) "04quit\n")
    ))

(defun wisi-ada-parse-show-buffer ()
  "Show wisi-ada-parse buffer."
  (interactive)
  (if (wisi-ada-parse--session-buffer wisi-ada-parse-session)
      (switch-to-buffer (wisi-ada-parse--session-buffer wisi-ada-parse-session))
    (error "wisi-ada-parse session not active")))

;;;;; main

(defun wisi-ada-parse ()
  ;; Eval the action forms one at a time; it's not one big form.  We
  ;; also need to ignore comments explicitly, since the buffer is not
  ;; in elisp mode (and we don't want to waste time putting it in that
  ;; mode)
  (wisi-ada-parse-require-session)
  (let ((source-buffer (current-buffer))
	(action-buffer (wisi-ada-parse--session-buffer wisi-ada-parse-session))
	(process (wisi-ada-parse--session-process wisi-ada-parse-session))
	action
	action-end
	(action-count 0)
	(sexp-start (point-min))
	(wait-count 0)
	(need-more nil)
	(done nil))

    (wisi-ada-parse-session-send-parse)
    (set-buffer action-buffer)

    ;; process actions until prompt received
    (while (and (process-live-p process)
		(not done))

      ;; process output is inserted before point, so move back over it to search it
      (goto-char sexp-start)

      ;; process all actions currently in buffer
      (while (and (process-live-p process)
		  (not need-more)
		  (not done))
	(cond
	 ((eobp)
	  (setq need-more t))

	 ((looking-at wisi-ada-parse-prompt)
	  (setq done t))

	 ((looking-at "^;;")
	  ;; debug output
	  (forward-line 1))

	 ((condition-case nil
	      (setq action-end (scan-sexps (point) 1))
	    (error
	     ;; incomplete action
	     (setq need-more t)
	     nil))
	  (when (> wisi-ada-parse-debug 0)
	    (setq action-count (1+ action-count))
	    (message "action-count: %d point: %d point-max: %d" action-count (point) (point-max)))

	  (setq action (buffer-substring-no-properties (point) action-end))
	  (goto-char action-end)
	  (forward-line 1)
	  (setq sexp-start (point))

	  (set-buffer source-buffer)
	  (eval (car (read-from-string action)) t)
	  (set-buffer action-buffer)
	  ))
	)

      (unless done
	;; end of buffer, or process died
	(unless (process-live-p process)
	  (wisi-ada-parse-show-buffer)
	  (error "wisi-ada-parse process died"))

	(setq wait-count (1+ wait-count))
	(when (> wisi-ada-parse-debug 0)
	  (message "wisi-ada-parse-session-wait: %d" wait-count))

	(accept-process-output process) ;; no time-out; that's a race condition
	(setq need-more nil))
      );; while not done

    ;; got command prompt
    (unless (process-live-p process)
      (wisi-ada-parse-show-buffer)
      (error "wisi-ada-parse process died"))

    (set-buffer source-buffer)))

(provide 'wisi-ada-parse)
