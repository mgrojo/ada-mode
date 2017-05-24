;;; wisi-ext-parse.el --- interface to exteral ada_mode_wisi_parse program
;;
;; Copyright (C) 2014, 2017  Free Software Foundation, Inc.
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

(defvar wisi-ext-parse-exec)
;; wisi-ext-parse-exec declared in ada-mode for auto-detection of indent engine

(defvar wisi-ext-parse-debug 0)

;;;;; sessions

;; ada_mode_wisi_parse builds internal parser structures on startup,
;; then runs a loop, waiting for parse requests.
;;
;; We only need one process; there is no persistent state.

(cl-defstruct (wisi-ext-parse--session)
  (process nil) ;; running ada_mode_wisi_parse
  (buffer nil))  ;; receives output of ada_mode_wisi_parse

(defvar wisi-ext-parse-session
  (make-wisi-ext-parse--session)
  "The single instance of wisi-ext-parse--session")

(defconst wisi-ext-parse-buffer-name " *ada_mode_wisi_parse*")

(defvar wisi-ext-parse-exec-opts nil
  "list of command-line options for `wisi-ext-parse-exec'.
’-v n’ outputs debug info.")

(defvar wisi-ext-parse-new-session nil
  "Non-nil indicates session is new; delay after first command.")

(defun wisi-ext-parse--start-process ()
  "Start the session process running ada_mode_wisi_parse."
  (unless (buffer-live-p (wisi-ext-parse--session-buffer wisi-ext-parse-session))
    ;; user may have killed buffer
    (setf (wisi-ext-parse--session-buffer wisi-ext-parse-session) (get-buffer-create wisi-ext-parse-buffer-name)))

  (let ((exec-file (locate-file wisi-ext-parse-exec exec-path '("" ".exe")))
	(process-connection-type nil) ;; use a pipe, not a pty; avoid line-by-line reads
	)
    (unless exec-file
      (error "%s not found on `exec-path'" wisi-ext-parse-exec))

    (with-current-buffer (wisi-ext-parse--session-buffer wisi-ext-parse-session)
      (erase-buffer); delete any previous messages, prompt
      (setf (wisi-ext-parse--session-process wisi-ext-parse-session)
	    (if wisi-ext-parse-exec-opts
		(apply #'start-process wisi-ext-parse-buffer-name (current-buffer) exec-file wisi-ext-parse-exec-opts)
	      (start-process wisi-ext-parse-buffer-name (current-buffer) exec-file)))
      (set-process-query-on-exit-flag (wisi-ext-parse--session-process wisi-ext-parse-session) nil)
      ;; FIXME: check protocol and version numbers?
      (wisi-ext-parse-session-wait)
      (setq wisi-ext-parse-new-session t)
      )))

(defun wisi-ext-parse-require-session ()
  "Create wisi-ext-parse session if not active."
  (unless (and (wisi-ext-parse--session-process wisi-ext-parse-session)
	       (process-live-p (wisi-ext-parse--session-process wisi-ext-parse-session)))
   (wisi-ext-parse--start-process)))

(defconst wisi-ext-parse-prompt "^;;> "
  "Regexp matching ada_mode_gps_indent prompt; indicates previous command is complete.")

(defun wisi-ext-parse-session-wait ()
  "Wait for the current command to complete."
  (unless (process-live-p (wisi-ext-parse--session-process wisi-ext-parse-session))
    (wisi-ext-parse-show-buffer)
    (error "%s process died" wisi-ext-parse-exec))

  (with-current-buffer (wisi-ext-parse--session-buffer wisi-ext-parse-session)
    (let ((process (wisi-ext-parse--session-process wisi-ext-parse-session))
	  (search-start (point-min))
	  (wait-count 0)
	  (found nil))
      (while (and (process-live-p process)
		  (progn
		    ;; process output is inserted before point, so move back over it to search it
		    (goto-char search-start)
		    (not (setq found (re-search-forward wisi-ext-parse-prompt (point-max) t)))))
	(setq search-start (point));; don't search same text again
	(setq wait-count (1+ wait-count))
	(when (> wisi-ext-parse-debug 0)
	    (message "wisi-ext-parse-session-wait: %d" wait-count))
	(accept-process-output process 0.1))
      (if found
	  (when (> wisi-ext-parse-debug 0)
	    (message "wisi-ext-parse-session-wait: %d" wait-count)
	    (when (> wisi-ext-parse-debug 2)
	      (message "'%s'" (buffer-substring-no-properties (point-min) (point-max)))))

	(wisi-ext-parse-show-buffer)
	(error "%s process died" wisi-ext-parse-exec))
      )))

(defun wisi-ext-parse-session-send-parse ()
  "Send a parse command to ada_mode_wisi_parse session, followed by the contents of the current buffer.
Does not wait for command to complete."
  (wisi-ext-parse-require-session)

  ;; ada_mode_wisi_parse can't handle non-ASCII, so we don't need string-bytes here.
  ;; FIXME: process-send-* should send buffer; avoid this copy
  (let* ((buf-string (buffer-substring-no-properties (point-min) (point-max)))
	 (cmd (format "parse \"%s\" %d" (buffer-name) (length buf-string)))
	 (msg (format "%02d%s" (length cmd) cmd))
	 (process (wisi-ext-parse--session-process wisi-ext-parse-session)))
    (when (> wisi-ext-parse-debug 0)
      (message msg))
    (with-current-buffer (wisi-ext-parse--session-buffer wisi-ext-parse-session)
      (erase-buffer))

    (process-send-string process msg)
    (process-send-string process buf-string)
    ))

(defun wisi-ext-parse-kill-session ()
  (interactive)
  (when (process-live-p (wisi-ext-parse--session-process wisi-ext-parse-session))
    (process-send-string (wisi-ext-parse--session-process wisi-ext-parse-session) "04quit\n")
    ))

(defun wisi-ext-parse-show-buffer ()
  "Show wisi-ext-parse buffer."
  (interactive)
  (if (wisi-ext-parse--session-buffer wisi-ext-parse-session)
      (switch-to-buffer (wisi-ext-parse--session-buffer wisi-ext-parse-session))
    (error "wisi-ext-parse session not active")))

(defun wisi-ext-parse-execute (token-table action-table sexp)
  "Execute encoded SEXP sent from subprocess."
  ;; sexp is an encoded version of a wisi parser action, with the token list prepended:
  ;;
  ;; A typical sexp is: [nonterm [token token ...] action]
  ;; where:
  ;; nonterm - token_id’pos; index into token-table
  ;; token - (token_id’pos (region_begin . region_end))
  ;; action - integer, index into action-table
  ;;
  ;; We don’t get here if wisi-tokens is nil.

  ;; Translate the codes to elisp symbols, build wisi-tok objects for wisi-tokens, execute
  (let ((i 0);; token index
	(nonterm (aref token-table (aref sexp 0)))
	(tokens-1 (aref sexp 1))
	(tokens-2 (make-vector (length (aref sexp 1)) nil)))

    (while (< i (length tokens-1))
      (aset tokens-2 i
	    (make-wisi-tok
	     :token (aref token-table (car (aref tokens-1 i)))
	     :region (nth 1 (aref tokens-1 i))
	     ;; FIXME: need other fields for indent; use wisi elisp parser!
	     ))
      (setq i (1+ i)))

    (funcall (aref action-table (aref sexp 2)) nonterm tokens-2)
    ))

;;;;; main

(defun wisi-ext-parse (token-table action-table)
  (wisi-ext-parse-require-session)
  (let ((source-buffer (current-buffer))
	(action-buffer (wisi-ext-parse--session-buffer wisi-ext-parse-session))
	(process (wisi-ext-parse--session-process wisi-ext-parse-session))
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

    (wisi-ext-parse-session-send-parse)
    (when wisi-ext-parse-new-session
      (setq wisi-ext-parse-new-session nil)
      ;; There is some race condition when first starting emacs;
      ;; loading more emacs code vs starting the external process
      ;; interface or something. It is quite repeatable when running
      ;; unit tests; this delay makes the tests work
      (sit-for 0.2))

    (set-buffer action-buffer)

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

	 ((looking-at wisi-ext-parse-prompt)
	  (setq done t))

	 ((looking-at "\\[")
	  (condition-case nil
	      (setq action-end (scan-sexps (point) 1))
	    (error
	     ;; incomplete action
	     (setq need-more t)
	     nil))

	  (unless need-more
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
	      (wisi-ext-parse-execute token-table action-table action))

	    (set-buffer action-buffer)
	    ))

	 (t
	  ;; debug output
	  (forward-line 1)
	  (setq sexp-start (point)))
	 )
	)

      (unless done
	;; end of buffer, or process died
	(unless (process-live-p process)
	  (wisi-ext-parse-show-buffer)
	  (error "wisi-ext-parse process died"))

	(setq wait-count (1+ wait-count))
	(setq start-wait-time (float-time))
	(accept-process-output process) ;; no time-out; that's a race condition

	(setq need-more nil))
      );; while not done

    ;; got command prompt
    (unless (process-live-p process)
      (wisi-ext-parse-show-buffer)
      (error "%s process died" wisi-ext-parse-exec))

    (when (> wisi-ext-parse-debug 0)
      (message "total time: %f" (- (float-time) start-time))
      (message "action-count: %d" action-count))

    (set-buffer source-buffer)
    ))

(defun wisi-ext-lex ()
  "For timing; just execute lexer in external process."
  (wisi-ext-parse-require-session)
  (let ((source-buffer (current-buffer))
	(process (wisi-ext-parse--session-process wisi-ext-parse-session))
	(w32-pipe-read-delay 0) ;; fastest subprocess read
	(done nil)
	(need-more nil)
	(start-time (float-time))
	start-wait-time)

    (let* ((buf-string (buffer-substring-no-properties (point-min) (point-max)))
	   (cmd (format "lex %d" (length buf-string)))
	   (msg (format "%02d%s" (length cmd) cmd)))

      (with-current-buffer (wisi-ext-parse--session-buffer wisi-ext-parse-session)
	(erase-buffer))

      (process-send-string process msg)
      (process-send-string process buf-string))

    (set-buffer (wisi-ext-parse--session-buffer wisi-ext-parse-session))

    ;; process responses until prompt received
    (while (and (process-live-p process)
		(not done))

      (goto-char (point-min))
      (unless (eobp)
	(forward-line 1)) ;; skip echoed command.

      (cond
       ((eobp)
	(setq need-more t))

       ((looking-at wisi-ext-parse-prompt)
	(setq done t))

       )

      (unless done
	(accept-process-output process) ;; no time-out; that's a race condition
	(setq need-more nil))
      );; while not done

    ;; got command prompt

    (set-buffer source-buffer)))

(defun wisi-ext-compile-actions (actions)
  "Compile grammar semantic action functions into an obarray.
Return a list (ACTION_ARRAY OBARRAY), where:

ACTION_ARRAY is a vector containing the action symbols in the
same order as ACTIONS, to allow lookup by integer index.

OBARRAY is the obarray containing the byte-compiled action
functions."
  (let ((symbol-obarray (make-vector 13 0))
	(table (make-vector (length actions) nil))
        (byte-compile-warnings '(not free-vars)) ;; for "wisi-test-success" in test/wisi/*
	(index 0)
	action name form symbol)
    (while actions
      (setq
       action (pop actions)
       name (car action)
       form (cadr action)
       symbol (intern name symbol-obarray))

      (fset symbol
	    `(lambda (wisi-nterm wisi-tokens)
	     ,form
	     nil))
      (byte-compile symbol)
      (aset table index symbol)
      (setq index (1+ index)))
    (list table symbol-obarray)))

(provide 'wisi-ext-parse)
