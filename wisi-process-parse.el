;;; wisi-process-parse.el --- interface to exteral parse program
;;
;; Copyright (C) 2014, 2017 Free Software Foundation, Inc.
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

(require 'cl-lib)
(require 'queue)
(require 'wisi-parse-common)

(defconst wisi-process-parse-prompt "^;;> "
  "Regexp matching executable prompt; indicates previous command is complete.")

(defconst wisi-process-parse-quit-cmd "04quit\n"
  "Command to external process telling it to quit.")

(defvar wisi-process-parse-debug 0)

;;;;; sessions

;; The executable builds internal parser structures on startup,
;; then runs a loop, waiting for parse requests.
;;
;; We only need one process per language; there is no persistent state.

(cl-defstruct (wisi-process--parser (:include wisi-parser))
  (label nil) 		       ;; string; prefix of buffer name, process name
  (exec-file nil) 	       ;; absolute file name of executable
  (token-table nil) 	       ;; vector of token symbols (terminal and nonterminal), indexed by integer
  (action-table nil) 	       ;; vector of vectors of semantic actions, indexed by nonterminal, production index
  (term-hash nil) 	       ;; hash table with keys of terminal token symbols, value of integer index
  (input-queue (queue-create)) ;; stores wisi-tok tokens returned by wisi-forward-token
  (parse-stack (queue-create)) ;; stores wisi-tok tokens mirroring external parse stack
  (process nil) 	       ;; running ada_mode_wisi_parse
  (buffer nil) 		       ;; receives output of ada_mode_wisi_parse
  (new-session t) 	       ;; Non-nil indicates session is new; delay after first command.
  )

;			       ;;###autoload
(cl-defun wisi-make-process-parser (&key label exec token-table action-table terminal-hashtable)
  "Return a ‘wisi-process--parser’ object."
  (let ((exec-file (locate-file exec exec-path '("" ".exe"))))

    (unless exec-file
      (error "%s not found on `exec-path'" exec))

    (make-wisi-process--parser
     :label label
     :exec-file exec-file
     :token-table token-table
     :action-table action-table
     :term-hash terminal-hashtable
     )))

(defvar wisi-process-parse-exec-opts nil
  "List of command-line options for external parse executable.
’-v n’ outputs debug info.")

(defun wisi-process-parse--start-process (parser)
  "Start or restart the parser process running EXEC."
  (let ((process-connection-type nil) ;; use a pipe, not a pty; avoid line-by-line reads
	(process-name (format " *%s_wisi_parse*" (wisi-process--parser-label parser))))

    (unless (buffer-live-p (wisi-process--parser-buffer parser))
      ;; User may have killed buffer to kill parser.
      (setf (wisi-process--parser-buffer parser)
	    (get-buffer-create process-name)))

    (with-current-buffer (wisi-process--parser-buffer parser)
      (erase-buffer)); delete any previous messages, prompt

    (setf (wisi-process--parser-process parser)
	  (make-process
	   :name process-name
	   :buffer (wisi-process--parser-buffer parser)
	   :command (append (list (wisi-process--parser-exec-file parser))
			    wisi-process-parse-exec-opts)))

    (set-process-query-on-exit-flag (wisi-process--parser-process parser) nil)
    (setf (wisi-process--parser-new-session parser) t)

    ;; FIXME: check protocol and version numbers
    (wisi-process-parse--wait parser)
    ))

(defun wisi-process-parse--require-process (parser)
  "Start the process for PARSER if not already started."
  (unless (process-live-p (wisi-process--parser-process parser))
    (wisi-process-parse--start-process parser)))

(defun wisi-process-parse--wait (parser)
  "Wait for the current command to complete."
  (let ((process (wisi-process--parser-process parser))
	(search-start (point-min))
	(wait-count 0)
	(found nil))

    (with-current-buffer (wisi-process--parser-buffer parser)
      (while (and (process-live-p process)
		  (progn
		    ;; process output is inserted before point, so move back over it to search it
		    (goto-char search-start)
		    (not (setq found (re-search-forward wisi-process-parse-prompt (point-max) t)))))
	(setq search-start (point));; don't search same text again
	(setq wait-count (1+ wait-count))
	(when (> wisi-process-parse-debug 0)
	    (message "wisi-process-parse--wait: %d" wait-count))
	(accept-process-output process 0.1))

      (if found
	  (when (> wisi-process-parse-debug 0)
	    (message "wisi-process-parse--wait: %d" wait-count)
	    (when (> wisi-process-parse-debug 2)
	      (message "'%s'" (buffer-substring-no-properties (point-min) (point-max)))))

	(wisi-process-parse-show-buffer parser)
	(error "%s process died" (wisi-process--parser-exec-file parser)))
      )))

(defun wisi-process-parse-show-buffer (parser)
  "Show PARSER buffer."
  (if (buffer-live-p (wisi-process--parser-buffer parser))
      (switch-to-buffer (wisi-process--parser-buffer parser))
    (error "wisi-process-parse process not active")))

(defun wisi-process-parse--enqueue-input (parser token)
  (queue-enqueue (wisi-process--parser-input-queue parser) token))

(defun wisi-process-parse--dequeue-input (parser)
  (queue-dequeue (wisi-process--parser-input-queue parser)))

(defun wisi-process-parse--push-stack (parser token)
  (queue-prepend (wisi-process--parser-parse-stack parser) token))

(defun wisi-process-parse--pop-stack (parser)
  (queue-dequeue (wisi-process--parser-parse-stack parser)))

(defun wisi-process-parse--send-parse (parser)
  "Send a parse command to PARSER external process, followed by
the tokens returned by ‘wisi-forward-token’ in the current
buffer.  Does not wait for command to complete."
  (let* ((cmd (format "parse \"%s\"" (buffer-name)))
	 (msg (format "%02d%s" (length cmd) cmd))
	 (process (wisi-process--parser-process parser))
	 (term-hash (wisi-process--parser-term-hash parser))
	 (done nil)
	 token term)
    (when (> wisi-process-parse-debug 0)
      (message msg))
    (with-current-buffer (wisi-process--parser-buffer parser)
      (erase-buffer))

    (process-send-string process msg)

    (goto-char (point-min))
    (forward-comment (point-max))
    (condition-case-unless-debug err
	(while (not done)
	  (setq token (wisi-forward-token))
	  (setq term (wisi-tok-token token))
	  (wisi-process-parse--enqueue-input parser token)
	  (process-send-string process (format "%d " (gethash term term-hash)))
	  (setq done (eq wisi-eoi-term term))
	  )
      (wisi-parse-error
       ;; Tell ext process we’re quiting
       (process-send-string process (format "%d " (gethash wisi-eoi-term term-hash)))
       (signal 'wisi-parse-error (cdr err))) ;; tell higher level code about error.
      )

    ))

(defun wisi-process-parse--push_token (parser sexp)
  ;; sexp is  [push_token id]
  ;; see ‘wisi-process-parse--execute’
  (let ((tok (wisi-process-parse--dequeue-input parser)))
    (unless (eq (wisi-tok-token tok) (aref (wisi-process--parser-token-table parser) (1- (aref sexp 1))))
      (error "token id mismatch between parser queue and process output"))

    (wisi-process-parse--push-stack parser tok)
    ))

(defun wisi-process-parse--merge_tokens (parser sexp)
  ;; sexp is  [merge_tokens nonterm [token token ...] production_index]
  ;; see ‘wisi-process-parse--execute’
  (let* ((token-table (wisi-process--parser-token-table parser))
	 (action-table (wisi-process--parser-action-table parser))
	 (first-nonterm (+ 1 (length token-table) (- (length action-table))))
	 ;; includes translation from ada 1 index to elisp 0 index
	 ;; FIXME: move to wisi-process--parser?
	 nonterm
	 nonterm-region
	 nonterm-line
	 nonterm-first
	 nonterm-comment-line
	 nonterm-comment-end
	 (tokens-1 (aref sexp 2))
	 (tokens-2 (make-vector (length (aref sexp 2)) nil)) ;; for wisi-tokens
	 tok i action)

    (setq i (1- (length tokens-1)))
    (while (>= i 0)
      (setq tok (wisi-process-parse--pop-stack parser))
      (unless (eq (wisi-tok-token tok) (aref token-table (1- (aref tokens-1 i))))
	(error "token id mismatch between parser queue and process output"))

      (setf nonterm-region (wisi-and-regions nonterm-region (wisi-tok-region tok)))
      (aset tokens-2 i tok)
      (setq i (1- i)))

    (when (eq wisi--parse-action 'indent)
      ;; Compute rest of nonterm. This duplicates part of
      ;; wisi-parse-reduce. Process tokens last to first to simplify
      ;; algorithms.
      (setq i (1- (length tokens-1)))
      (while (>= i 0)
	(setq tok (aref tokens-1 i))
	(setf (wisi-tok-line tok) (or (wisi-tok-line tok) nonterm-line))
	(if (wisi-tok-nonterminal tok)
	    (when (wisi-tok-first tok)
	      (setf nonterm-first (wisi-tok-first tok)))
	  (setq nonterm-first
		(or (and (wisi-tok-first tok) (wisi-tok-line tok))
		    (wisi-tok-comment-line tok)
		    nonterm-first)))

	(when (and (not nonterm-comment-line)
		   (wisi-tok-comment-line tok))
	  (setf nonterm-comment-line (wisi-tok-comment-line tok))
	  (setq nonterm-comment-end (wisi-tok-comment-end tok)))

	(setq i (1- i))))

    (setq nonterm
	  (make-wisi-tok
	   :token (aref token-table (1- (aref sexp 1)))
	   :region nonterm-region
	   :nonterminal t
	   :line nonterm-line
	   :first nonterm-first
	   :comment-line nonterm-comment-line
	   :comment-end nonterm-comment-end))

    (wisi-process-parse--push-stack parser nonterm)

    (when nonterm-region
      (setq action (aref action-table (- (aref sexp 1) first-nonterm)))
      (when action
	(setq action (aref action (aref sexp 3))))
      (when action
	(funcall action (wisi-tok-token nonterm) tokens-2)))
    ))

(defun wisi-process-parse--recover (parser sexp)
  ;; sexp is [recover [popped_id ...] [skipped_id ...] pushed_id]
  ;; see ‘wisi-process-parse--execute’
  (let ((token-table (wisi-process--parser-token-table parser))
	ids tok)
    (setq ids (aref sexp 1))
    (dotimes (i (length ids))
      (setq tok (wisi-process-parse--pop-stack parser))
      (unless (eq (wisi-tok-token tok) (aref token-table (1- (aref ids i))))
	(error "token id mismatch between parser queue and process output")))

    (setq ids (aref sexp 2))
    (dotimes (i (length ids))
      (setq tok (wisi-process-parse--dequeue-input parser))
      (unless (eq (wisi-tok-token tok) (aref token-table (1- (aref ids i))))
	(error "token id mismatch between parser queue and process output")))

    (wisi-process-parse--push-stack
     parser
     (make-wisi-tok
      :token (aref token-table (1- (aref sexp 3)))
      ;; FIXME: set region to all popped, skipped?
      ))

    ;; FIXME: report invalid region
  ))

(defun wisi-process-parse--execute (parser sexp)
  "Execute encoded SEXP sent from external process."
  ;; sexp is an encoded version of a semantic state or production action
  ;;
  ;; An sexp is: [action arg ...]
  ;; where:
  ;; action - integer, index into action-table
  ;;
  ;; Actions:
  ;;
  ;; [push_token id]
  ;;    pop token (should have id = id) from the input queue, push it
  ;;    on the augmented token stack
  ;;
  ;; [merge_tokens nonterm_id [id ...] production_index]
  ;;    Pop tokens from auqmented stack (should match [id ...]),
  ;;    combine into nonterm, push that onto augmented stack. Call
  ;;    action-table (nonterm_id, procduction_index) with
  ;;    wisi-nonterm, wisi-tokens bound to nonterm, tokens.
  ;;
  ;;    FIXME: don’t need right hand side token ids, just count. Keep for
  ;;    now so can double-check.
  ;;
  ;; [recover [popped_id ...] [skipped_id ...] pushed_id]
  ;;
  ;; where:
  ;; push_token = 1
  ;; merge_tokens = 2
  ;; recover = 3
  ;;
  ;; nonterm_id, *id - token_id’pos; index into token-table (ada 1 origin)
  ;; production_index - index into action-table for nonterm (ada 1 origin)

  (cl-ecase (aref sexp 0)
    (1 (wisi-process-parse--push_token parser sexp))
    (2 (wisi-process-parse--merge_tokens parser sexp))
    (3 (wisi-process-parse--recover parser sexp))))

;;;;; main

(cl-defmethod wisi-parse-kill ((parser wisi-process--parser))
  (when (process-live-p (wisi-process--parser-process parser))
    (process-send-string (wisi-process--parser-process parser) wisi-process-parse-quit-cmd)
    ))

(cl-defmethod wisi-parse-current ((parser wisi-process--parser))
  "Run the external parser on the current buffer."
  (wisi-process-parse--require-process parser)

  (let* ((source-buffer (current-buffer))
	 (action-buffer (wisi-process--parser-buffer parser))
	 (process (wisi-process--parser-process parser))
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

    (queue-clear (wisi-process--parser-input-queue parser))
    (queue-clear (wisi-process--parser-parse-stack parser))

    (wisi-process-parse--send-parse parser)
    (when (wisi-process--parser-new-session parser)
      (setf (wisi-process--parser-new-session parser) nil)
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

	 ((looking-at wisi-process-parse-prompt)
	  (setq done t))

	 ((or (looking-at "\\[") ;; semantic action
	      (looking-at "(")) ;; post-parse expression to eval
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
	      (wisi-process-parse--execute parser action))

	    (set-buffer action-buffer)
	    ))

	 (t
	  ;; debug output, error message
	  (forward-line 1)
	  (setq sexp-start (point)))
	 )
	)

      (unless done
	;; end of buffer, or process died
	(unless (process-live-p process)
	  (wisi-process-parse-show-buffer parser)
	  (error "wisi-process-parse process died"))

	(setq wait-count (1+ wait-count))
	(setq start-wait-time (float-time))
	(accept-process-output process) ;; no time-out; that's a race condition

	(setq need-more nil))
      );; while not done

    ;; got command prompt
    (unless (process-live-p process)
      (wisi-process-parse-show-buffer parser)
      (error "wisi-process-parse process died"))

    (when (> wisi-process-parse-debug 0)
      (message "total time: %f" (- (float-time) start-time))
      (message "action-count: %d" action-count))

    (set-buffer source-buffer)
    ))

(defun wisi-process-compile-tokens (tokens)
  "Compile terminals from TOKENS into a hash-table.
Return a list (TOKENS TERMINAL_HASH_TABLE), where:

TOKENS is an array of token id symbols, terminals first. Last
terminal must be ‘wisi-eoi-term’. \\

TERMINAL_HASH_TABLE is a hash table with keys of terminals from
TOKENS, values integer index into TOKENS."
  (let ((count 0)
	hash-table)

    (while (not (eq wisi-eoi-term (aref tokens count)))
      (setq count (1+ count)))

    (setq hash-table (make-hash-table :test #'eq :size count))

    (while (>= count 0)
      ;; elisp array is 0 orgin, Ada token_id is 1 origin
      (puthash (aref tokens count) (1+ count) hash-table)
      (setq count (1- count)))

    (list tokens hash-table)
    ))

(defun wisi-process-compile-actions (nonterm-actions)
  "Compile grammar semantic action functions into an obarray.
Return a list (ACTION_TABLE OBARRAY), where:

NONTERM-ACTIONS is a list of lists of action entries. Outer list
has one entry for each nonterminal, inner list one entry for each
production with that nonterminal as the left hand side. Each
entry has the form (ACTION_NAME, ACTION_FORM).

ACTION_TABLE is a vector of vectors containing the action
symbols; indexed by nonterminal, production_index.

OBARRAY is the obarray containing the byte-compiled action
functions."
  (let ((table (make-vector (length nonterm-actions) nil))
        (symbol-obarray (make-vector (length nonterm-actions) 0))
	(byte-compile-warnings '(not free-vars)) ;; for "wisi-test-success" in test/wisi/*
	(nonterm-index 0)
	(prod-index 0)
	nonterm action name form symbol)
    (while nonterm-actions
      (setq
       nonterm (pop nonterm-actions)
       prod-index 0)
      (when nonterm
	(aset table nonterm-index (make-vector (length nonterm) nil)))
      (while nonterm
	(setq action (pop nonterm))
	(when action
	  (setq
	   name (car action)
	   form (cadr action)
	   symbol (intern name symbol-obarray))
	  (fset symbol `(lambda (wisi-nterm wisi-tokens) ,form nil))
	  (byte-compile symbol)
	  (aset (aref table nonterm-index) prod-index symbol))
	(setq prod-index (1+ prod-index)))
      (setq nonterm-index (1+ nonterm-index)))
    (list table symbol-obarray)))

(provide 'wisi-process-parse)
