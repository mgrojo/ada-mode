;; wisi-elisp-parse.el --- Wisi parser  -*- lexical-binding:t -*-

;; Copyright (C) 2013-2015, 2017  Free Software Foundation, Inc.

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

;; An extended LALR parser, that handles shift/reduce and
;; reduce/reduce conflicts by spawning parallel parsers to follow each
;; path.

;;; Code:

(require 'cl-lib)
(require 'wisi-parse-common)

(defvar wisi-elisp-parse-max-parallel-current (cons 0 0)
  "Cons (count . point); Maximum number of parallel parsers used in most recent parse,
point at which that max was spawned.")

(defvar wisi-debug-identical 0
  "Debug terminating identical parsers.
0 - keep lower-numbered parser.
1 - keep higher-numbered parser.
2 - error.")

(cl-defstruct (wisi-elisp-parser-state
	    (:copier nil))
  label ;; integer identifying parser for debug

  active
  ;; 'shift  - need new token
  ;; 'reduce - need reduce
  ;; 'accept - parsing completed
  ;; 'error  - failed, error not reported yet
  ;; nil     - terminated
  ;;
  ;; 'pending-shift, 'pending-reduce - newly created parser

  stack
  ;; Each stack item takes two slots: wisi-tok, state

  sp ;; stack pointer

  pending
  ;; list of (action-symbol stack-fragment)
  )

(cl-defstruct (wisi-elisp-parser (:include wisi-parser))
  actions
  gotos
  next-token
  )

;;;###autoload
(defun wisi-make-elisp-parser (automaton next-token)
  "Return ‘wisi-parser’ object.

- AUTOMATON is the parse table generated by `wisi-compile-grammar'.

- LEXER is a function with no argument called by the parser to
  obtain the next token from the current buffer after point, as a
  wisi-tok object (normally ‘wisi-forward-token’)."
  (make-wisi-elisp-parser
   :actions (aref automaton 0)
   :gotos (aref automaton 1)
   :next-token next-token))

(cl-defmethod wisi-parse-kill ((_parser wisi-elisp-parser))
  nil)

(cl-defmethod wisi-parse-current ((parser wisi-elisp-parser))
  "Parse current buffer from beginning."

  (let* ((actions (wisi-elisp-parser-actions parser))
	 (gotos   (wisi-elisp-parser-gotos parser))
	 (parser-states ;; vector of parallel parser states
	  (vector
	   (make-wisi-elisp-parser-state
	    :label 0
	    :active  'shift
	    :stack   (make-vector wisi-parse-max-stack-size nil)
	    :sp      0
	    :pending nil)))
	 (active-parser-count 1)
	 active-parser-count-prev
	 (active 'shift)
	 (token nil)
	 some-pending
	 )

    (setf (wisi-parser-errors parser) nil)

    (goto-char (point-min))
    (forward-comment (point-max))
    (aset (wisi-elisp-parser-state-stack (aref parser-states 0)) 0 0)

    (setq token (funcall (wisi-elisp-parser-next-token parser)))
    (setq wisi-elisp-parse-max-parallel-current (cons 0 0))

    (while (not (eq active 'accept))
      (setq active-parser-count-prev active-parser-count)
      (setq some-pending nil)
      (dotimes (parser-index (length parser-states))
	(when (eq active (wisi-elisp-parser-state-active (aref parser-states parser-index)))
	  (let* ((parser-state (aref parser-states parser-index))
		 (result (wisi-elisp-parse-1 token parser-state (> active-parser-count 1) actions gotos)))
	    (when result
	      ;; spawn a new parser
	      (when (= active-parser-count wisi-parse-max-parallel)
		(let* ((state (aref (wisi-elisp-parser-state-stack parser-state)
				    (wisi-elisp-parser-state-sp parser-state)))
		       (msg (wisi-error-msg (concat "too many parallel parsers required in grammar state %d;"
						    " simplify grammar, or increase `wisi-elisp-parse-max-parallel'")
					    state)))
		  (push (make-wisi--error :pos (point) :message msg) (wisi-parser-errors parser))
		  (signal 'wisi-parse-error msg)))

	      (let ((j (wisi-elisp-parse-free-parser parser-states)))
		(cond
		 ((= j -1)
		  ;; Add to parser-states; the new parser won't be executed
		  ;; again in this parser-index loop.
		  (setq parser-states (vconcat parser-states (vector nil)))
		  (setq j (1- (length parser-states))))
		 ((< j parser-index)
		  ;; The new parser won't be executed again in this
		  ;; parser-index loop; nothing to do.
		  )
		 (t
		  ;; Don't let the new parser execute again in this
		  ;; parser-index loop.
		  (setq some-pending t)
		  (setf (wisi-elisp-parser-state-active result)
			(cl-case (wisi-elisp-parser-state-active result)
			  (shift 'pending-shift)
			  (reduce 'pending-reduce)
			 )))
		  )
		(setq active-parser-count (1+ active-parser-count))
		(when (> active-parser-count (car wisi-elisp-parse-max-parallel-current))
		  (setq wisi-elisp-parse-max-parallel-current (cons active-parser-count (point))))
		(setf (wisi-elisp-parser-state-label result) j)
		(aset parser-states j result))
	      (when (> wisi-debug 1)
                (message "spawn parser (%d active)" active-parser-count)))

	    (when (eq 'error (wisi-elisp-parser-state-active parser-state))
	      (setq active-parser-count (1- active-parser-count))
	      (when (> wisi-debug 1)
                (message "terminate parser (%d active)" active-parser-count))
	      (cl-case active-parser-count
		(0
		 (cond
		  ((= active-parser-count-prev 1)
		   ;; We were not in a parallel parse; abandon parsing, report the error.
		   (let* ((state (aref (wisi-elisp-parser-state-stack parser-state)
				       (wisi-elisp-parser-state-sp parser-state)))
			  (msg (wisi-error-msg "syntax error in grammar state %d; unexpected %s, expecting one of %s"
					       state
					       (wisi-token-text token)
					       (mapcar 'car (aref actions state)))))
		     (push (make-wisi--error :pos (point) :message msg) (wisi-parser-errors parser))
		     (signal 'wisi-parse-error msg)))
		  (t
		   ;; Report errors from all parsers that failed on this token.
		   (let ((msg))
		     (dotimes (_ (length parser-states))
		       (let* ((parser-state (aref parser-states parser-index))
			      (state (aref (wisi-elisp-parser-state-stack parser-state)
					   (wisi-elisp-parser-state-sp parser-state))))
			 (when (eq 'error (wisi-elisp-parser-state-active parser-state))
			   (setq msg
				 (concat msg
					 (when msg "\n")
					 (wisi-error-msg
					  "syntax error in grammar state %d; unexpected %s, expecting one of %s"
					  state
					  (wisi-token-text token)
					  (mapcar 'car (aref actions state)))))
			   )))
		     (push (make-wisi--error :pos (point) :message msg) (wisi-parser-errors parser))
		     (signal 'wisi-parse-error msg)))
		  ))

		(1
		 (setf (wisi-elisp-parser-state-active parser-state) nil); Don't save error for later.
		 (wisi-elisp-parse-execute-pending (aref parser-states (wisi-elisp-parse-active-parser parser-states))))

		(t
		 ;; We were in a parallel parse, and this parser
		 ;; failed; mark it inactive, don't save error for
		 ;; later.
		 (setf (wisi-elisp-parser-state-active parser-state) nil)
		 )))
	    )));; end dotimes

      (when some-pending
	;; Change pending-* parsers to *.
	(dotimes (parser-index (length parser-states))
	  (cond
	   ((eq (wisi-elisp-parser-state-active (aref parser-states parser-index)) 'pending-shift)
	    (setf (wisi-elisp-parser-state-active (aref parser-states parser-index)) 'shift))
	   ((eq (wisi-elisp-parser-state-active (aref parser-states parser-index)) 'pending-reduce)
	    (setf (wisi-elisp-parser-state-active (aref parser-states parser-index)) 'reduce))
	   )))

      (setq active (wisi-elisp-parsers-active parser-states active-parser-count))
      (when (eq active 'shift)
	(when (> active-parser-count 1)
	  (setq active-parser-count (wisi-elisp-parse-elim-identical parser parser-states active-parser-count)))

	(setq token (funcall (wisi-elisp-parser-next-token parser))))
    )
    (when (> active-parser-count 1)
      (error "ambiguous parse result"))))

(defun wisi-elisp-parsers-active-index (parser-states)
  ;; only called when active-parser-count = 1
  (let ((result nil)
	(i 0))
    (while (and (not result)
		(< i (length parser-states)))
      (when (wisi-elisp-parser-state-active (aref parser-states i))
	(setq result i))
      (setq i (1+ i)))
    result))

(defun wisi-elisp-parsers-active (parser-states active-count)
  "Return the type of parser cycle to execute.
PARSER-STATES[*].active is the last action a parser took. If it
was `shift', that parser used the input token, and should not be
executed again until another input token is available, after all
parsers have shifted the current token or terminated.

Returns one of:

`accept' : all PARSER-STATES have active set to nil or `accept' -
done parsing

`shift' : all PARSER-STATES have active set to nil, `accept', or
`shift' - get a new token, execute `shift' parsers.

`reduce' : some PARSER-STATES have active set to `reduce' - no new
token, execute `reduce' parsers."
  (let ((result nil)
	(i 0)
	(shift-count 0)
	(accept-count 0)
	active)
    (while (and (not result)
		(< i (length parser-states)))
      (setq active (wisi-elisp-parser-state-active (aref parser-states i)))
      (cond
	((eq active 'shift) (setq shift-count (1+ shift-count)))
	((eq active 'reduce) (setq result 'reduce))
	((eq active 'accept) (setq accept-count (1+ accept-count)))
	)
      (setq i (1+ i)))

    (cond
     (result )
     ((= accept-count active-count)
      'accept)
     ((= (+ shift-count accept-count) active-count)
      'shift)
     (t
      ;; all parsers in error state; should not get here
      (error "all parsers in error state; programmer error"))
     )))

(defun wisi-elisp-parse-free-parser (parser-states)
  "Return index to a non-active parser in PARSER-STATES, -1 if there is none."
  (let ((result nil)
	(i 0))
    (while (and (not result)
		(< i (length parser-states)))
      (when (not (wisi-elisp-parser-state-active (aref parser-states i)))
	(setq result i))
      (setq i (1+ i)))
    (if result result -1)))

(defun wisi-elisp-parse-active-parser (parser-states)
  "Return index to the first active parser in PARSER-STATES."
  (let ((result nil)
	(i 0))
    (while (and (not result)
		(< i (length parser-states)))
      (when (wisi-elisp-parser-state-active (aref parser-states i))
	(setq result i))
      (setq i (1+ i)))
    (unless result
      (error "no active parsers"))
    result))

(defun wisi-elisp-parse-elim-identical (parser parser-states active-parser-count)
  "Check for parsers in PARSER-STATES that have reached identical states eliminate one.
Return new ACTIVE-PARSER-COUNT. Assumes all parsers have active
nil, `shift', or `accept'."
  ;; parser-states passed by reference; active-parser-count by copy
  ;; see test/ada_mode-slices.adb for example
  (dotimes (parser-i (1- (length parser-states)))
    (when (wisi-elisp-parser-state-active (aref parser-states parser-i))
      (dotimes (parser-j (- (length parser-states) parser-i 1))
	(when (wisi-elisp-parser-state-active (aref parser-states (+ parser-i parser-j 1)))
	  (when (eq (wisi-elisp-parser-state-sp (aref parser-states parser-i))
		     (wisi-elisp-parser-state-sp (aref parser-states (+ parser-i parser-j 1))))
	    (let ((compare t)
		  exec)
	      (dotimes (stack-i (wisi-elisp-parser-state-sp (aref parser-states parser-i)))
		(setq
		 compare
		 (and compare ;; bypass expensive 'arefs' after first stack item compare fail
		      (equal (aref (wisi-elisp-parser-state-stack (aref parser-states parser-i)) stack-i)
			     (aref (wisi-elisp-parser-state-stack (aref parser-states (+ parser-i parser-j 1)))
				   stack-i)))))
	      (when compare
		;; parser stacks are identical
		(setq active-parser-count (1- active-parser-count))
		(when (> wisi-debug 1)
		  (message "terminate identical parser %d (%d active)"
			   (+ parser-i parser-j 1) active-parser-count)
		  (let ((state-i (aref parser-states parser-i))
			(state-j (aref parser-states (+ parser-i parser-j 1))))
		    (message "%d actions:" (wisi-elisp-parser-state-label state-i))
		    (mapc #'wisi-elisp-parse-debug-put-action (wisi-elisp-parser-state-pending state-i))

		    (message "%d actions:" (wisi-elisp-parser-state-label state-j))
		    (mapc #'wisi-elisp-parse-debug-put-action (wisi-elisp-parser-state-pending state-j))
		    ))
		(cl-ecase wisi-debug-identical
		  (0
		   (setq exec parser-i)
		   (setf (wisi-elisp-parser-state-active (aref parser-states (+ parser-i parser-j 1))) nil))

		  (1
		   (setq exec (+ parser-i parser-j 1))
		   (setf (wisi-elisp-parser-state-active (aref parser-states parser-i)) nil))

		  (2
		   (let ((msg "identical parser stacks"))
		     (push (make-wisi--error :pos (point) :message msg) (wisi-parser-errors parser))
		     (signal 'wisi-parse-error msg)))
		  )
		(when (= active-parser-count 1)
		  ;; The actions for the two parsers are not
		  ;; identical, but most of the time either is good
		  ;; enough for indentation and navigation, so we just
		  ;; do the actions for the one that is not
		  ;; terminating. Some times, a significant action is
		  ;; lost. In that case, turn on
		  ;; ‘wisi-debug-identical’ to investigate fixing it.
		  (wisi-elisp-parse-execute-pending (aref parser-states exec)))
		))))
	)))
  active-parser-count)

(defun wisi-elisp-parse-exec-action (func nonterm tokens)
  "Execute action if TOKENS not null."
  ;; `tokens' is null when all tokens in a grammar statement are
  ;; optional and not present.
  (if (< 0 (length tokens))
      (when wisi--parse-action
	(funcall func nonterm tokens))

    (when (> wisi-debug 1)
      (message "... action skipped; no tokens"))
    ))

(defvar wisi-elisp-parser-state nil
  "Let-bound in `wisi-elisp-parse-reduce', used in `wisi-parse-find-token'.")

(defun wisi-elisp-parse-debug-put-action (action)
  ;; Action is (semantic-function nonterm [tokens])
  (message "%s [%s]"
	   (nth 0 action)
	   (mapcar #'wisi-tok-debug-image (nth 2 action))))

(defun wisi-elisp-parse-execute-pending (parser-state)
  (let ((wisi-elisp-parser-state parser-state);; reference, for wisi-parse-find-token
	(pending (wisi-elisp-parser-state-pending parser-state)))

    (when (> wisi-debug 1)
      (message "%d: pending actions:" (wisi-elisp-parser-state-label parser-state)))

    (while pending
      (when (> wisi-debug 1) (wisi-elisp-parse-debug-put-action (car pending)))

      (let ((func-args (pop pending)))
	(wisi-elisp-parse-exec-action (nth 0 func-args) (nth 1 func-args) (cl-caddr func-args)))
      )
    (setf (wisi-elisp-parser-state-pending parser-state) nil)
    ))

(defmacro wisi-elisp-parse-action (i al)
  "Return the parser action.
I is a token item number and AL is the list of (item . action)
available at current state.  The first element of AL contains the
default action for this state."
  `(cdr (or (assq ,i ,al) (car ,al))))

(defun wisi-elisp-parse-1 (token parser-state pendingp actions gotos)
  "Perform one shift or reduce on PARSER-STATE.
If PENDINGP, push actions onto PARSER-STATE.pending; otherwise execute them.
See `wisi-elisp-parse' for full details.
Return nil or new parser (a wisi-elisp-parser-state struct)."
  (let* ((state (aref (wisi-elisp-parser-state-stack parser-state)
		(wisi-elisp-parser-state-sp parser-state)))
	 (parse-action (wisi-elisp-parse-action (wisi-tok-token token) (aref actions state)))
	 new-parser-state)

    (when (> wisi-debug 1)
      ;; output trace info
      (if (> wisi-debug 2)
	  (progn
	    ;; put top 10 stack items
	    (let* ((count (min 20 (wisi-elisp-parser-state-sp parser-state)))
		   (msg (make-vector (+ 1 count) nil)))
	      (dotimes (i count)
		(aset msg (- count i)
		      (aref (wisi-elisp-parser-state-stack parser-state)
			    (- (wisi-elisp-parser-state-sp parser-state) i)))
		)
	      (message "%d: %s: %d: %s"
		       (wisi-elisp-parser-state-label parser-state)
		       (wisi-elisp-parser-state-active parser-state)
		       (wisi-elisp-parser-state-sp parser-state)
		       msg))
	    (message "   %d: %s: %s" state (wisi-tok-debug-image token) parse-action))
	(message "%d: %d: %s: %s" (wisi-elisp-parser-state-label parser-state) state token parse-action)))

    (when (and (listp parse-action)
	       (not (symbolp (car parse-action))))
      ;; Conflict; spawn a new parser.
      (setq new-parser-state
	    (make-wisi-elisp-parser-state
	     :active  nil
	     :stack   (vconcat (wisi-elisp-parser-state-stack parser-state))
	     :sp      (wisi-elisp-parser-state-sp parser-state)
	     :pending (wisi-elisp-parser-state-pending parser-state)))

      (wisi-elisp-parse-2 (cadr parse-action) token new-parser-state t gotos)
      (setq pendingp t)
      (setq parse-action (car parse-action))
      );; when

    ;; current parser
    (wisi-elisp-parse-2 parse-action token parser-state pendingp gotos)

    new-parser-state))

(defun wisi-elisp-parse-2 (action token parser-state pendingp gotos)
  "Execute parser ACTION (must not be a conflict).
Return nil."
  (cond
   ((eq action 'accept)
    (setf (wisi-elisp-parser-state-active parser-state) 'accept))

   ((eq action 'error)
    (setf (wisi-elisp-parser-state-active parser-state) 'error))

   ((natnump action)
    ;; Shift token and new state (= action) onto stack
    (let ((stack (wisi-elisp-parser-state-stack parser-state)); reference
	  (sp (wisi-elisp-parser-state-sp parser-state))); copy
      (setq sp (+ sp 2))
      (aset stack (1- sp) token)
      (aset stack sp action)
      (setf (wisi-elisp-parser-state-sp parser-state) sp))
    (setf (wisi-elisp-parser-state-active parser-state) 'shift))

   (t
    (wisi-elisp-parse-reduce action parser-state pendingp gotos)
    (setf (wisi-elisp-parser-state-active parser-state) 'reduce))
   ))

(defun wisi-elisp-parse-first-last (stack i j)
  "Return a pair (FIRST . LAST), indices for the first and last
non-empty tokens for a nonterminal; or nil if all tokens are
empty. STACK is the parser stack.  I and J are the indices in
STACK of the first and last tokens of the nonterminal."
  (let ((start (car (wisi-tok-region (aref stack i))))
        (end   (cdr (wisi-tok-region (aref stack j)))))
    (while (and (or (not start) (not end))
		(/= i j))
      (cond
       ((not start)
	;; item i is an empty production
	(setq start (car (wisi-tok-region (aref stack (setq i (+ i 2)))))))

       ((not end)
	;; item j is an empty production
	(setq end (cdr (wisi-tok-region (aref stack (setq j (- j 2)))))))

       (t (setq i j))))

    (when (and start end)
      (cons i j))
    ))

(cl-defmethod wisi-parse-find-token ((_parser wisi-elisp-parser) token-symbol)
  "Find token with TOKEN-SYMBOL on current parser stack, return token struct.
For use in grammar actions."
  ;; Called from wisi-parse-exec-action in wisi-parse-reduce
  (let* ((stack (wisi-elisp-parser-state-stack wisi-elisp-parser-state))
	 (sp (1- (wisi-elisp-parser-state-sp wisi-elisp-parser-state)))
	 (tok (aref stack sp)))
    (while (and (> sp 0)
		(not (eq token-symbol (wisi-tok-token tok))))
      (setq sp (- sp 2))
      (setq tok (aref stack sp)))
    (if (= sp 0)
	(error "token %s not found on parser stack" token-symbol)
      tok)
    ))

(cl-defmethod wisi-parse-prev-token ((_parser wisi-elisp-parser) n)
  ;; IMPROVEME: store stack in parser
  (let* ((stack (wisi-elisp-parser-state-stack wisi-elisp-parser-state))
	 (sp (1- (wisi-elisp-parser-state-sp wisi-elisp-parser-state)))
	 (i (- sp (* 2 n))))
    (when (> i 0)
      (aref stack i))))

(defun wisi-elisp-parse-reduce (action parser-state pendingp gotos)
  "Reduce PARSER-STATE.stack, and execute or pend ACTION."
  (let* ((wisi-elisp-parser-state parser-state);; reference, for wisi-parse-find-token
	 (stack (wisi-elisp-parser-state-stack parser-state)); reference
	 (sp (wisi-elisp-parser-state-sp parser-state)); copy
	 (token-count (nth 2 action))
	 (nonterm (nth 0 action))
	 (first-last (when (> token-count 0)
		       (wisi-elisp-parse-first-last stack (- sp (* 2 (1- token-count)) 1) (1- sp))))
	 (nonterm-region (when first-last
			   (cons
			    (car (wisi-tok-region (aref stack (car first-last))))
			    (cdr (wisi-tok-region (aref stack (cdr first-last)))))))
	 (post-reduce-state (aref stack (- sp (* 2 token-count))))
	 (new-state (cdr (assoc nonterm (aref gotos post-reduce-state))))
	 (tokens (make-vector token-count nil))
	 line first comment-line comment-end)

    (when (not new-state)
      (error "no goto for %s %d" nonterm post-reduce-state))

    (dotimes (i token-count)
      (let ((tok (aref stack (- sp (* 2 i) 1))))
	(when (nth 1 action)
	  ;; don't need wisi-tokens for a null user action
	  (aset tokens (- token-count i 1) tok))

	(when (eq wisi--parse-action 'indent)
	  (setq line (or (wisi-tok-line tok) line))
	  (if (wisi-tok-nonterminal tok)
	      (when (wisi-tok-first tok)
		(setq first (wisi-tok-first tok)))
	    (setq first (or (and (wisi-tok-first tok) (wisi-tok-line tok))
			    (wisi-tok-comment-line tok)
			    first))
	    ))))

    (when (and (eq wisi--parse-action 'indent)
	       first-last)
      (let ((tok (aref stack (cdr first-last))))
	(when (wisi-tok-comment-line tok)
	  (setq comment-line (wisi-tok-comment-line tok))
	  (setq comment-end (wisi-tok-comment-end tok)))))

    (setq sp (+ 2 (- sp (* 2 token-count))))
    (aset stack (1- sp)
	  (make-wisi-tok
	   :token nonterm
	   :region nonterm-region
	   :nonterminal t
	   :line line
	   :first first
	   :comment-line comment-line
	   :comment-end comment-end))
    (aset stack sp new-state)
    (setf (wisi-elisp-parser-state-sp parser-state) sp)

    (when (nth 1 action)
      ;; nothing to do for a null user action
      (if pendingp
	  (if (wisi-elisp-parser-state-pending parser-state)
	      (setf (wisi-elisp-parser-state-pending parser-state)
		    (append (wisi-elisp-parser-state-pending parser-state)
			    (list (list (nth 1 action) nonterm tokens))))
	    (setf (wisi-elisp-parser-state-pending parser-state)
		  (list (list (nth 1 action) nonterm tokens))))

	;; Not pending.
	(wisi-elisp-parse-exec-action (nth 1 action) nonterm tokens)
	))
    ))

(provide 'wisi-elisp-parse)
;;; wisi-elisp-parse.el ends here
