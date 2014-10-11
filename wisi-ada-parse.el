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

  (let ((exec-file (locate-file wisi-ada-parse-exec exec-path '("" ".exe")))
	(process-connection-type nil) ;; use a pipe, not a pty; avoid line-by-line reads
	)
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

(defun wisi-ada-parse-xlate-codes (elisp-names args)
  (let ((i 0) ;; arg index
	)
    (while (< i (length args))
      (when (< (aref args i) 0)
	    (aset args i (aref elisp-names (- (aref args i)))))
      (setq i (1+ i))
      )
    ))

(defun wisi-ada-parse-exec-action (elisp-names action)
  (let ((func (aref elisp-names (- (car action))))
	(i 0) ;; arg index
	j ;; sequence arg index
	)
    (while (< i (length (cdr action)))
      (cond
	 ((sequencep (aref (cdr action) i))
	  (wisi-ada-parse-xlate-codes elisp-names (aref (cdr action) i)
	  )

	 (t
	  (when (< (aref (cdr action) i) 0)
	    (aset (cdr action) i (aref elisp-names (- (aref (cdr action) i)))))
	  ))
      (setq i (1+ i))
      )
    (apply func (append (cdr action) nil));; FIXME; rewrite actions to take arrays?
    ))

(defun wisi-ada-parse-execute (elisp-names sexp)
  "Execute encoded SEXP sent from subprocess."
  ;; sexp is an encoded version of a wisi parser action, with the token list prepended:
  ;;
  ;; A typical action is:
  ;; [[token token ...]
  ;;  [
  ;;  (action-name . [arg arg ...])
  ;;  (action-name . [arg arg ...])
  ;;  ...
  ;;  ]
  ;; ]
  ;;
  ;; or, if there is only one action:
  ;; [[token token ...)
  ;;  [action-name arg arg ...]
  ;; ]
  ;;
  ;; arg can be:
  ;; positive integer: token number
  ;; negative integer: symbol
  ;; list of integers

  ;; Translate the codes back to elisp symbols, execute
  (let ((i 0);; token or arg index
	(j 0);; action index
	)

    ;; tokens = (aref sexp 0)
    ;; token = (aref tokens i)
    (while (< i (length (aref sexp 0)))
      (setcar (aref (aref sexp 0) i) (aref elisp-names (car (aref (aref sexp 0) i))))
      (setq i (1+ i)))
    (setq wisi-tokens (append (aref sexp 0) nil));; FIXME; rewrite actions to take arrays?

    (if (arrayp (aref sexp 1))
	;; multiple actions
	(while (< j (length (aref sexp 1)))
	  (wisi-ada-parse-exec-action elisp-names (aref (aref sexp 1) j))
	  (setq j (1+ j)))
      ;; single action
      (wisi-ada-parse-exec-action elisp-names (aref sexp 1)))
    ))

;;;;; main

(defun wisi-ada-parse (elisp-names)
  (wisi-ada-parse-require-session)
  (let ((source-buffer (current-buffer))
	(action-buffer (wisi-ada-parse--session-buffer wisi-ada-parse-session))
	(process (wisi-ada-parse--session-process wisi-ada-parse-session))
	(w32-pipe-read-delay 0) ;; fastest subprocess read
	action
	action-end
	(action-count 0)
	(sexp-start (point-min))
	(wait-count 0)
	(need-more nil)
	(done nil)
	(start-time (float-time))
	start-wait-time)

    (wisi-ada-parse-session-send-parse)
    (set-buffer action-buffer)

    (when (> wisi-ada-parse-debug 0)
      (message "wisi-ada-parse-send: %f point-max: %d"
	       (- (float-time) start-time)
	       (point-max)))

    ;; process responses until prompt received
    (while (and (process-live-p process)
		(not done))

      ;; process all responses currently in buffer
      (while (and (process-live-p process)
		  (not need-more)
		  (not done))

	(goto-char sexp-start)

	(cond
	 ((eobp)
	  (setq need-more t))

	 ((looking-at wisi-ada-parse-prompt)
	  (setq done t))

	 ((looking-at "^;;")
	  ;; debug output
	  (forward-line 1)
	  (setq sexp-start (point)))

	 ((condition-case nil
	      (setq action-end (scan-sexps (point) 1))
	    (error
	     ;; incomplete action
	     (setq need-more t)
	     nil))

	  (setq action-count (1+ action-count))
	  (setq action (car (read-from-string (buffer-substring-no-properties (point) action-end))))
	  (goto-char action-end)
	  (forward-line 1)
	  (setq sexp-start (point))

	  (set-buffer source-buffer)
	  (if (listp action)
	      ;; post-parser action
	      (eval action)
	    ;; encoded parser action
	    (wisi-ada-parse-execute elisp-names action))

	  (set-buffer action-buffer)
	  ))
	)

      (unless done
	;; end of buffer, or process died
	(unless (process-live-p process)
	  (wisi-ada-parse-show-buffer)
	  (error "wisi-ada-parse process died"))

	(setq wait-count (1+ wait-count))
	(setq start-wait-time (float-time))
	(accept-process-output process) ;; no time-out; that's a race condition
	(when (> wisi-ada-parse-debug 0)
	  (message "wisi-ada-parse-session-wait: %d %f %f"
		   wait-count
		   (- (float-time) start-time)
		   (- (float-time) start-wait-time))
	  (message "action-count: %d point: %d point-max: %d" action-count (point) (point-max)))

	(setq need-more nil))
      );; while not done

    ;; got command prompt
    (unless (process-live-p process)
      (wisi-ada-parse-show-buffer)
      (error "wisi-ada-parse process died"))

    (when (> wisi-ada-parse-debug 0)
      (message "total time: %f" (- (float-time) start-time))
      (message "action-count: %d" action-count))

    (set-buffer source-buffer)))

(defun wisi-ada-parse-exec-buffer (action-buffer-name source-buffer-name elisp-names)
  "for debugging; execute encoded actions in ACTION-BUFFER, applying to SOURCE-BUFFER."
  (let ((action-buffer (get-buffer action-buffer-name))
	(source-buffer (get-buffer source-buffer-name))
	action
	action-end
	(action-count 0)
	(done nil)
	(start-time (float-time)))

    (set-buffer action-buffer)
    (goto-char (point-min))

    (while (not done)
      (cond
       ((or (eobp)
	    (looking-at wisi-ada-parse-prompt))
	(setq done t))

       ((looking-at "^;;")
	;; debug output
	(forward-line 1))

       ((setq action-end (scan-sexps (point) 1))
	(setq action-count (1+ action-count))
	(setq action (car (read-from-string (buffer-substring-no-properties (point) action-end))))
	(goto-char action-end)
	(forward-line 1)

	(set-buffer source-buffer)
	(if (and (listp action)
		 (symbolp (car action)))
	    ;; post-parser action
	    (eval action)
	  ;; encoded parser action
	  (if (arrayp action)
	      (wisi-ada-parse-execute-arr elisp-names action)
	    (wisi-ada-parse-execute elisp-names action)))

	(set-buffer action-buffer)
	)))

    (when (> wisi-ada-parse-debug 0)
      (message "total time: %f" (- (float-time) start-time))
      (message "action-count: %d" action-count))
    ))

(provide 'wisi-ada-parse)
