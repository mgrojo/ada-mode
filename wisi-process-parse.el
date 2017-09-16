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
(require 'queue) ;; Gnu ELPA package
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
;; We only need one process per language; there is no persistent state
;; in the process between parses, and processes are too heavy-weight
;; to have one per buffer. We use a global alist of parser objects to
;; find the right one for the current buffer.

(cl-defstruct (wisi-process--parser (:include wisi-parser))
  (label nil)                  ;; string uniquely identifying parser
  (exec-file nil) 	       ;; absolute file name of executable
  (token-table nil) 	       ;; vector of token symbols (terminal and nonterminal), indexed by integer
  (action-table nil) 	       ;; vector of vectors of semantic actions, indexed by nonterminal, production index
  (term-hash nil) 	       ;; hash table with keys of terminal token symbols, value of integer index
  (busy nil)                   ;; t while parser is active
  (lexer-queue (queue-create)) ;; stores wisi-tok tokens returned by wisi-forward-token
  (lookahead-queue (queue-create)) ;; stores wisi-tok tokens during parallel processing and error recover algorithms
  (parse-stack (queue-create)) ;; stores wisi-tok tokens mirroring external parse stack
  (process nil) 	       ;; running ada_mode_wisi_parse
  (buffer nil) 		       ;; receives output of ada_mode_wisi_parse
  (parser-errors nil)          ;; alist of error lists, indexed by parser id.
  (total-wait-time 0.0)        ;; total time during last parse spent waiting for subprocess output.
  (token-count 0)              ;; tokens sent to subprocess during last parse; for profiling.
  (action-count 0)             ;; actions received from subprocess during last parse; for profiling.
  )

(defvar wisi-process--alist nil
  "Alist mapping string label to ‘wisi-process--session’ struct")

;;;###autoload
(cl-defun wisi-make-process-parser (&key label exec token-table action-table terminal-hashtable)
  "Return a ‘wisi-process--parser’ object matching LABEL.
If not found in ‘wisi-process--alist’, create using other parameters."
  (or (cdr (assoc label wisi-process--alist))
      (let ((exec-file (locate-file exec exec-path '("" ".exe")))
	    parser)

	(unless exec-file
	  (error "%s not found on `exec-path'" exec))

	(setq parser
	      (make-wisi-process--parser
	       :label label
	       :exec-file exec-file
	       :token-table token-table
	       :action-table action-table
	       :term-hash terminal-hashtable))

	(push (cons label parser) wisi-process--alist)

	parser
     )))

(defvar wisi-process-parse-exec-opts nil
  "List of command-line options for external parse executable.
’-v n’ outputs debug info.")

(defun wisi-process-parse--require-process (parser)
  "Start the process for PARSER if not already started."
  (unless (process-live-p (wisi-process--parser-process parser))
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
      (setf (wisi-process--parser-busy parser) nil)

      ;; FIXME: check protocol and version numbers
      (wisi-process-parse--wait parser)
      )))

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
      (pop-to-buffer (wisi-process--parser-buffer parser))
    (error "wisi-process-parse process not active")))

(defun wisi-process-parse--push-stack (parser token)
  (queue-prepend (wisi-process--parser-parse-stack parser) token))

(defun wisi-process-parse--pop-stack (parser)
  (queue-dequeue (wisi-process--parser-parse-stack parser)))

(defun wisi-process-parse--send-parse (parser)
  "Send a parse command to PARSER external process, followed by
the tokens returned by ‘wisi-forward-token’ in the current
buffer.  Does not wait for command to complete."
  (let* ((cmd (format "parse \"%s\" %d %d %d"
		      (buffer-name)
		      (1- wisi-debug)
		      (if wisi-mckenzie-cost-limit wisi-mckenzie-cost-limit -1)
		      (if wisi-mckenzie-check-limit wisi-mckenzie-check-limit -1)
		      ))
	 (msg (format "%02d%s" (length cmd) cmd))
	 (process (wisi-process--parser-process parser))
	 (term-hash (wisi-process--parser-term-hash parser))
	 (done nil)
	 (token-count 0)
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
	  (setq token-count (1+ token-count))
	  (setq term (wisi-tok-token token))
	  (queue-enqueue (wisi-process--parser-lexer-queue parser) token)
	  (process-send-string process (format "%d " (gethash term term-hash)))
	  (setq done (eq wisi-eoi-term term))
	  )
      (wisi-parse-error
       ;; Tell ext process we’re quiting
       (process-send-string process (format "%d " (gethash wisi-eoi-term term-hash)))
       (signal 'wisi-parse-error (cdr err))) ;; tell higher level code about error.
      )
    (setf (wisi-process--parser-token-count parser) token-count)
    ))

(defun wisi-process-parse--send-noop (parser)
  "Send a noop command to PARSER external process, followed by
the tokens returned by ‘wisi-forward-token’ in the current
buffer.  Does not wait for command to complete."
  (let* ((cmd "noop ")
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
	  (process-send-string process (format "%d " (gethash term term-hash)))
	  (setq done (eq wisi-eoi-term term))
	  )
      (wisi-parse-error
       ;; Tell ext process we’re quiting
       (process-send-string process (format "%d " (gethash wisi-eoi-term term-hash)))
       (signal 'wisi-parse-error (cdr err))) ;; tell higher level code about error.
      )
    ))

(cl-defmethod wisi-parse-find-token ((parser wisi-process--parser) token-symbol)
  (let* ((stack (wisi-process--parser-parse-stack parser))
	 (sp 0)
	 (tok (queue-nth stack 0)))
    (while (and (< sp (queue-length stack))
		(not (eq token-symbol (wisi-tok-token tok))))
      (setq sp (1+ sp))
      (setq tok (queue-nth stack sp)))
    (if (= sp (queue-length stack))
	(error "token %s not found on parser stack" token-symbol)
      tok)
    ))

(cl-defmethod wisi-parse-prev-token ((parser wisi-process--parser) n)
  (let* ((stack (wisi-process--parser-parse-stack parser)))
    (when (< n (queue-length stack))
      (queue-nth stack n))))

(defun wisi-process-parse--id-to-enum (token-table int-id)
  "Translate INT-ID from process integer token ids to elisp enumeral id
from TOKEN-TABLE."
  (aref token-table (1- int-id)))

(defun wisi-process-parse--check-id (label token-table tok id)
  "Check that TOK id equals ID; throw error if not."
  (let ((enum (wisi-process-parse--id-to-enum token-table id)))
    (unless (and tok enum
		 (eq (wisi-tok-token tok) enum))
      (error (concat
	      (format "%s: token id mismatch; elisp %s, process %s"
		      label (and tok (wisi-tok-debug-image tok)) enum)
	      (unless tok (format ", point %d" (point)))))
      )))

(defun wisi-process-parse--Lexer_To_Lookahead (parser sexp)
  ;; sexp is  [Lexer_To_Lookahead id]
  ;; see ‘wisi-process-parse--execute’
  (let ((tok (queue-dequeue (wisi-process--parser-lexer-queue parser))))
    (wisi-process-parse--check-id "Lexer_To_Lookahead" (wisi-process--parser-token-table parser) tok (aref sexp 1))
    (queue-append (wisi-process--parser-lookahead-queue parser) tok)
    (when (> wisi-debug 1)
      (message "Lexer_To_Lookahead %s" (wisi-tok-debug-image tok)))
    ))

(defun wisi-process-parse--Error (parser sexp)
  ;; sexp is [Error parser_id [expected_id ...]]
  ;; see ‘wisi-process-parse--execute’
  ;; so far, we don’t need to track the invalid region
  (let* ((token-table (wisi-process--parser-token-table parser))
	 (elisp-ids (mapcar (lambda (id) (aref token-table (1- id))) (aref sexp 2)))
	 (pos (car (wisi-tok-region (queue-last (wisi-process--parser-lookahead-queue parser)))))
	 err)

    (goto-char pos)

    (setq err
	  (make-wisi--error
	   :pos (copy-marker pos)
	   :message
	   (format "%s:%d:%d: syntax error; expecting one of '%s'"
		   (file-name-nondirectory (buffer-file-name))
		   (line-number-at-pos (point))
		   (current-column)
		   elisp-ids)))

    (if (assq (aref sexp 1) (wisi-process--parser-parser-errors parser))
	(push err
	      (cdr (assq (aref sexp 1) (wisi-process--parser-parser-errors parser))))
      (push (cons (aref sexp 1) (list err)) (wisi-process--parser-parser-errors parser)))
    ))

(defun wisi-process-parse--Spawn (parser sexp)
  ;; sexp is [Spawn old_parser_id new_parser_id]
  ;; see ‘wisi-process-parse--execute’
  ;; new_parser_id has not been used previously in this parse.
  (let ((old-errors (cdr (assq (aref sexp 1) (wisi-process--parser-parser-errors parser)))))
    (push (cons (aref sexp 2) (cl-copy-seq old-errors))
	  (wisi-process--parser-parser-errors parser))
    ))

(defun wisi-process-parse--Terminate_Parser (parser sexp)
  ;; sexp is [Terminate_Parser parser_id]
  ;; see ‘wisi-process-parse--execute’
  (setf (wisi-process--parser-parser-errors parser)
	(delete (assq (aref sexp 1) (wisi-process--parser-parser-errors parser))
		(wisi-process--parser-parser-errors parser))))

(defun wisi-process-parse--Virtual_To_Lookahead (parser sexp)
  ;; sexp is  [Virtual_To_Lookahead id]
  ;; see ‘wisi-process-parse--execute’
  (let ((tok (make-wisi-tok
	      :token (wisi-process-parse--id-to-enum (wisi-process--parser-token-table parser) (aref sexp 1))
	      :region nil
	      :virtual t)))
    (queue-prepend (wisi-process--parser-lookahead-queue parser) tok)
    (when (> wisi-debug 1)
      (message "Virtual_To_Lookahead %s" (wisi-tok-debug-image tok)))
    ))

(defun wisi-process-parse--Push_Current (parser sexp)
  ;; sexp is  [Push_Current id]
  ;; see ‘wisi-process-parse--execute’
  (let ((tok (queue-dequeue (wisi-process--parser-lookahead-queue parser))))
    (wisi-process-parse--check-id "Push_Current" (wisi-process--parser-token-table parser) tok (aref sexp 1))
    (wisi-process-parse--push-stack parser tok)
    (when (> wisi-debug 1)
      (message "Push_Current %s" (wisi-tok-debug-image tok)))
    ))

(defun wisi-process-parse--Reduce_Stack (parser sexp)
  ;; if wisi-debug > 0 then
  ;;    sexp is [Reduce_Stack nonterm [token token ...] production_index]
  ;; else
  ;;    sexp is  [Reduce_Stack nonterm token-count production_index]
  ;; end if;
  ;; see ‘wisi-process-parse--execute’
  (let* ((token-table (wisi-process--parser-token-table parser))
	 (action-table (wisi-process--parser-action-table parser))
	 (first-nonterm (+ 1 (length token-table) (- (length action-table))))
	 ;; includes translation from ada 1 index to elisp 0 index
	 ;; FIXME: move to wisi-process--parser?
	 (tokens-1 (when (> wisi-debug 1) (aref sexp 2))) ;; token ids from process
	 (token-count (if (> wisi-debug 1) (length tokens-1) (aref sexp 2)))
	 nonterm
	 nonterm-region
	 nonterm-line
	 nonterm-first
	 nonterm-comment-line
	 nonterm-comment-end
	 (virtual-count 0)
	 (empty-count 0)
	 (comment-set nil)
	 (tokens-2 (make-vector token-count nil)) ;; wisi-tok, for wisi-tokens
	 tok i action)

    (setq i (1- token-count))
    (while (>= i 0)
      (setq tok (wisi-process-parse--pop-stack parser))
      (when (> 0 wisi-debug)
	(wisi-process-parse--check-id "Reduce_Stack" token-table tok (aref tokens-1 i)))
      (setq nonterm-region (wisi-and-regions nonterm-region (wisi-tok-region tok)))
      (when (wisi-tok-virtual tok) (setq virtual-count (1+ virtual-count)))
      (when (and (not (wisi-tok-virtual tok))
		 (not (wisi-tok-region tok)))
	(setq empty-count (1+ empty-count)))
      (aset tokens-2 i tok)
      (setq i (1- i)))

    (when (eq wisi--parse-action 'indent)
      ;; Compute rest of nonterm. This duplicates part of
      ;; wisi-parse-reduce. Process tokens last to first to simplify
      ;; algorithms.
      (setq i (1- (length tokens-2)))
      (while (>= i 0)
	(setq tok (aref tokens-2 i))
	(setq nonterm-line (or (wisi-tok-line tok) nonterm-line))
	(if (wisi-tok-nonterminal tok)
	    (when (wisi-tok-first tok)
	      (setq nonterm-first (wisi-tok-first tok)))
	  (setq nonterm-first
		(or (and (wisi-tok-first tok) (wisi-tok-line tok))
		    (wisi-tok-comment-line tok)
		    nonterm-first)))

	(when (and (not comment-set)
		   (wisi-tok-region tok))
	  ;; comment-line reports only trailing comments; it is set
	  ;; from the last non-nil token in nonterm.
	  (setq nonterm-comment-line (wisi-tok-comment-line tok))
	  (setq nonterm-comment-end (wisi-tok-comment-end tok))
	  (setq comment-set t))

	(setq i (1- i))))

    (setq nonterm
	  (make-wisi-tok
	   :token (aref token-table (1- (aref sexp 1)))
	   :region nonterm-region
	   :virtual (and (< 0 virtual-count)
			 (= (length tokens-1) (+ virtual-count empty-count)))
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
	(when (> wisi-debug 1) (message "%s" action))
	(funcall action (wisi-tok-token nonterm) tokens-2)))
    ))

(defun wisi-process-parse--Discard_Lookahead (parser sexp)
  ;; sexp is  [Discard_Lookahead  parser_id token_id]
  ;; see ‘wisi-process-parse--execute’
  (let ((tok (queue-dequeue (wisi-process--parser-lookahead-queue parser))))
    (wisi-process-parse--check-id "Discard_Lookahead" (wisi-process--parser-token-table parser) tok (aref sexp 2))
    (let ((data (car (cdr (assq (aref sexp 1) (wisi-process--parser-parser-errors parser))))))
      (setf (wisi--error-deleted data) (append (wisi--error-deleted data) (list tok))))
    (when (> wisi-debug 1)
      (message "Discard_Lookahead/ %s" (wisi-tok-debug-image tok)))
    ))

(defun wisi-process-parse--Discard_Stack (parser sexp)
  ;; sexp is  [Discard_Stack parser_id token_id]
  ;; see ‘wisi-process-parse--execute’
  (let ((tok (wisi-process-parse--pop-stack parser)))
    (wisi-process-parse--check-id "Discard_Stack" (wisi-process--parser-token-table parser) tok (aref sexp 2))
    (let ((data (car (cdr (assq (aref sexp 1) (wisi-process--parser-parser-errors parser))))))
      (setf (wisi--error-popped data) (append (wisi--error-popped data) (list tok))))
    (when (> wisi-debug 1)
      (message "Discard_Stack %s" (wisi-tok-debug-image tok)))
    ))

(defun wisi-process-parse--Recover (parser sexp)
  ;; sexp is [Recover parser_id [inserted ...]]
  ;; see ‘wisi-process-parse--execute’
  ;; so far, we don’t need to track the invalid region
  ;; popped, deleted are set in --Discard_Lookahead, --Discard_Stack above.
  (let* ((token-table (wisi-process--parser-token-table parser))
	 (elisp-ids (mapcar (lambda (id) (aref token-table (1- id))) (aref sexp 2)))
	 (data (car (cdr (assq (aref sexp 1) (wisi-process--parser-parser-errors parser))))))
    (setf (wisi--error-inserted data) elisp-ids)
    ))

(defun wisi-process-parse--execute (parser sexp)
  "Execute encoded SEXP sent from external process."
  ;; sexp is an encoded version of a semantic state or production action
  ;;
  ;; An sexp is: [action arg ...]
  ;; where:
  ;; action - integer, index into action-table
  ;;
  ;; FIXME: most don’t need Ada to send token ids. Keep for now so can
  ;; double-check; change to send only if wisi-debug > 0.
  ;;
  ;; Actions:
  ;;
  ;; [Lexer_To_Lookahead id]
  ;;    Parser read ID from parser lexer, added it to back of
  ;;    lookahead queue. Move the corresponding augmented token from
  ;;    the front of the elisp lexer queue to the back of the elisp
  ;;    lookahead queue.
  ;;
  ;; [Error [expected_id ...]]
  ;;    The token at the front of the parser input queue caused a
  ;;    syntax error; save information for later
  ;;    reporting. ’expected_id ...’ is a list of the token ids
  ;;    expected by the grammar at that point. Mark the start of an
  ;;    invalid buffer region.
  ;;
  ;; [Spawn old_parser_id new_parser_id]
  ;;    Copy error information from old parser to new parser.
  ;;
  ;; [Terminate_Parser parser_id]
  ;;    Discard error information for parser.
  ;;
  ;; [Virtual_To_Lookahead id]
  ;;    Parser retrieved ID from an error recover solution, and added
  ;;    it to the front of the lookahead queue.  Add null augmented
  ;;    info, add to front of elisp lookahead queue.
  ;;
  ;; [Push_Current id]
  ;;    Parser pushed current token id onto the parser stack; remove
  ;;    the current token from front of the elisp lookahead queue,
  ;;    push it on the elisp augmented token parse stack.
  ;;
  ;; [Reduce_Stack nonterm_id token-count production_index] wisi-debug = 0
  ;; [Reduce_Stack nonterm_id [id ...] production_index] wisi-debug > 0
  ;;    Parser reduced [id ...] tokens on the parser parse stack to
  ;;    nonterm_id, using production production_index in the current
  ;;    state (on top of the parser parse stack before the
  ;;    reduction). Perform the same opertion on the elisp augmented
  ;;    parse stack. Call the semantic function given in action-table
  ;;    (nonterm_id, production_index) with wisi-nonterm, wisi-tokens
  ;;    bound to nonterm, tokens.
  ;;
  ;; [Discard_Lookahead parser_id token_id]
  ;;    Parser error recovery discarded id from the front of the
  ;;    parser lookahead queue; do the same with the corresponding
  ;;    augmented token at the front of the elisp lookahead queue. Add
  ;;    its buffer region to the current invalid region.
  ;;
  ;; [Discard_Stack parser_id token_id]
  ;;    Parser error recovery discarded id from the top of the
  ;;    parser parse stack; do the same with the corresponding
  ;;    augmented token at the top of the elisp parse stack. Add its
  ;;    buffer region to the current invalid region.
  ;;
  ;; [Recover parser_id [inserted_id ...]]
  ;;    The parser finished a successful error recovery. Tokens popped
  ;;    off the parse stack (deleted before error point) are reported
  ;;    in Discard_Stack, tokens deleted after error point in
  ;;    Discard_Lookahead. Virtual tokens inserted before error report
  ;;    are reported here. Mark the end of an invalid buffer region.
  ;;
  ;; Numeric operation codes given in case expression below
  ;;
  ;; nonterm_id, *id - token_id’pos; index into token-table (process 1 origin)
  ;; production_index - index into action-table for nonterm (process 0 origin)

  (cl-ecase (aref sexp 0)
    (1  (wisi-process-parse--Lexer_To_Lookahead parser sexp))
    (2  (wisi-process-parse--Error parser sexp))
    (3  (wisi-process-parse--Spawn parser sexp))
    (4  (wisi-process-parse--Terminate_Parser parser sexp))
    (5  (wisi-process-parse--Virtual_To_Lookahead parser sexp))
    (6  (wisi-process-parse--Push_Current parser sexp))
    (7  (wisi-process-parse--Reduce_Stack parser sexp))
    (8  (wisi-process-parse--Discard_Lookahead parser sexp))
    (9  (wisi-process-parse--Discard_Stack parser sexp))
    (10 (wisi-process-parse--Recover parser sexp))
    ))

;;;;; main

(cl-defmethod wisi-parse-kill ((parser wisi-process--parser))
  (when (process-live-p (wisi-process--parser-process parser))
    (process-send-string (wisi-process--parser-process parser) wisi-process-parse-quit-cmd)
    (sit-for 1.0)
    (when (process-live-p (wisi-process--parser-process parser))
      (kill-process (wisi-process--parser-process parser)))
    ))

(cl-defmethod wisi-parse-current ((parser wisi-process--parser))
  "Run the external parser on the current buffer."
  (wisi-process-parse--require-process parser)

  ;; font-lock can trigger a face parse while navigate or indent parse
  ;; is active, due to ‘accept-process-output’ below. font-lock cannot
  ;; hang (it is called from an idle timer), so don’t wait. Don’t
  ;; throw an error either; there is no syntax error.
  (if (wisi-process--parser-busy parser)
      (progn
	(setf (wisi-parser-errors parser)
	      (list
	       (make-wisi--error
		:pos 0
		:message (format "%s:%d:%d: parser busy (try ’wisi-kill-parser’)"
				 (file-name-nondirectory (buffer-file-name)) 1 1))
	       ))
	(message "%s parse abandoned; parser busy" wisi--parse-action)
	(goto-char (point-min))
	;; Leaving point at point-min sets wisi-cache-max to
	;; point-min, so we know parsing still needs to be
	;; done. However, font-lock thinks the buffer is done.  We
	;; tried signaling ’quit here to force font-lock to try again,
	;; but that did not work. This only happens in unit tests, so
	;; they can call ’wisi-parse-buffer ’face’.
	)
    (condition-case-unless-debug err
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
	       start-wait-time)

	  (setf (wisi-process--parser-busy parser) t)
	  (setf (wisi-process--parser-total-wait-time parser) 0.0)

	  ;; reset parser
	  (setf (wisi-parser-errors parser) nil)
	  (setf (wisi-process--parser-parser-errors parser) nil)
	  (queue-clear (wisi-process--parser-lexer-queue parser))
	  (queue-clear (wisi-process--parser-lookahead-queue parser))
	  (queue-clear (wisi-process--parser-parse-stack parser))

	  (wisi-process-parse--send-parse parser)

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
		    (looking-at "(")) ;; error or other expression to eval
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
		      ;; error of some sort
		      (cond
		       ((equal '(parse_error) action)
			(dolist (item (wisi-process--parser-parser-errors parser))
			  (setf (wisi-parser-errors parser)
				(append (wisi-parser-errors parser)
					(cdr item))))
			;; Parser detected a syntax error, and recovery failed, so signal it.
			(signal 'wisi-parse-error
				(wisi--error-message (car (wisi-parser-errors parser)))))

		       ((and (eq 'error (car action))
			     (string-prefix-p "bad command:" (cadr action)))
			;; Parser dropped bytes, is treating token ids
			;; as commands. Kill the process to kill the
			;; pipes; there is no other way to flush them.
			(kill-process (wisi-process--parser-process parser))
			(signal 'wisi-parse-error "parser lost sync; killed"))

		       (t
			;; Some other error
			(eval action)))

		    ;; encoded parser action
		    (unless wisi-action-disable
		      (condition-case-unless-debug err
			  (wisi-process-parse--execute parser action)
			(wisi-parse-error
			 ;; From an action; missing statement-action in grammar, etc.
			 (push (make-wisi--error :pos (point) :message (cdr err)) (wisi-parser-errors parser))
			 (signal (car err) (cdr err))))))

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
		(wisi-process-parse-show-buffer parser)
		(error "wisi-process-parse process died"))

	      (setq wait-count (1+ wait-count))
	      (setq start-wait-time (float-time))

	      ;; If we specify no time-out here, we get messages about
	      ;; "blocking call with quit inhibited", when this is
	      ;; called by font-lock from the display engine.
	      ;;
	      ;; Specifying just-this-one t prevents C-q from
	      ;; interrupting this.
	      ;;
	      ;; FIXME: but now we have a race condition between
	      ;; reading the output and waiting for it?
	      (accept-process-output process 1.0 nil nil)

	      (setf (wisi-process--parser-total-wait-time parser)
		    (+ (wisi-process--parser-total-wait-time parser)
		       (- (float-time) start-wait-time)))

	      (setq need-more nil))
	    );; while not done

	  ;; got command prompt
	  (dolist (item (wisi-process--parser-parser-errors parser))
	    (setf (wisi-parser-errors parser)
		  (append (wisi-parser-errors parser)
			  (cdr item))))

	  (unless (process-live-p process)
	    (wisi-process-parse-show-buffer parser)
	    (error "wisi-process-parse process died"))

	  (setf (wisi-process--parser-action-count parser) action-count)

	  (setf (wisi-process--parser-busy parser) nil)
	  (set-buffer source-buffer)
	  ;; If we get here, the parse succeeded; move point to end of
	  ;; buffer as the elisp parser does.
	  (goto-char (point-max))
	  )

      (wisi-parse-error
       (setf (wisi-process--parser-busy parser) nil)
       (signal (car err) (cdr err)))

      (error
       (setf (wisi-process--parser-busy parser) nil)
       (signal (car err) (cdr err))
       ))))

(defun wisi-process-send-tokens-noop ()
  "Run lexer, send tokens to subprocess; otherwise no operation.
For use with ’wisi-time’."
  (wisi-process-parse--require-process wisi--parser)
  (if (wisi-process--parser-busy wisi--parser)
      (error "%s parser busy" wisi--parse-action)

    ;; not busy
    (let* ((source-buffer (current-buffer))
	   (action-buffer (wisi-process--parser-buffer wisi--parser))
	   (process (wisi-process--parser-process wisi--parser))
	   (sexp-start (point-min))
	   (need-more nil)
	   (done nil))

      (setf (wisi-process--parser-busy wisi--parser) t)
      (wisi-process-parse--send-noop wisi--parser)
      (set-buffer action-buffer)
      (while (and (process-live-p process)
		  (not done))
	(goto-char sexp-start)
	(cond
	 ((eobp)
	  (setq need-more t))

	 ((looking-at wisi-process-parse-prompt)
	  (setq done t))

	 (t
	  (forward-line 1)
	  (setq sexp-start (point)))
	 )

	(unless done
	  ;; end of buffer, or process died
	  (unless (process-live-p process)
	    (wisi-process-parse-show-buffer wisi--parser)
	    (error "wisi-process-parse process died"))

	  (accept-process-output process 1.0 nil nil)
	  (setq need-more nil))
	)
      (set-buffer source-buffer)
      (setf (wisi-process--parser-busy wisi--parser) nil)
      )))

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

;;;;; debugging
(defun wisi-process-parse-ids-to-enum (token-table &rest int-ids)
  "Translate INT-IDS from process integer token ids to elisp enumeral ids.
Returns reversed sequence."
  (let ((enum-ids nil))
    (cl-dolist (i int-ids)
      (push (aref token-table (1- i)) enum-ids))
    enum-ids))

(provide 'wisi-process-parse)
