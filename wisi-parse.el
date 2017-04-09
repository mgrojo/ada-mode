;;; wisi-parse.el --- Wisi parser  -*- lexical-binding:t -*-

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

;; WORKAROUND: for some reason, this condition doesn't work in batch mode!
;; (when (and (= emacs-major-version 24)
;; 	   (= emacs-minor-version 2))
  (require 'wisi-compat-24.2)
;;)

(defvar wisi-parse-max-parallel 15
  "Maximum number of parallel parsers for acceptable performance.
If a file needs more than this, it's probably an indication that
the grammar is excessively redundant.")

(defvar wisi-parse-max-parallel-current (cons 0 0)
  "Cons (count . point); Maximum number of parallel parsers used in most recent parse,
point at which that max was spawned.")

(defvar wisi-parse-max-stack-size 500
  "Maximum parse stack size")

(defvar wisi-debug 0
  "wisi debug mode:
0 : normal - ignore parse errors, for indenting new code
1 : report parse errors (for running tests)
2 : show parse states, position point at parse errors, debug-on-error works in parser
3 : also show top 10 items of parser stack.")

(cl-defstruct (wisi-parser-state
	    (:copier nil))
  label ;; integer identifying parser for debug

  active
  ;; 'shift  - need new token
  ;; 'reduce - need reduce
  ;; 'accept - parsing completed
  ;; 'error  - failed, error not reported yet
  ;; nil     - terminated
  ;;
  ;; 'pending-shift, 'pending-reduce - newly created parser; see wisi-parse

  stack
  ;; Each stack item takes two slots: wisi-tok, state

  sp ;; stack pointer

  pending
  ;; list of (action-symbol stack-fragment)
  )

(defun wisi-error-msg (message &rest args)
  (let ((line (line-number-at-pos))
	(col (- (point) (line-beginning-position))))
    (format
     "%s:%d:%d: %s"
       (file-name-nondirectory (buffer-name)) ;; buffer-file-name is sometimes nil here!?
       line col
       (apply 'format message args))))

(defvar wisi-parse-error nil)
(put 'wisi-parse-error
     'error-conditions
     '(error wisi-parse-error))
(put 'wisi-parse-error
     'error-message
     "wisi parse error")

(defvar-local wisi--cache-max
  (list
   (cons 'face nil)
   (cons 'navigate nil)
   (cons 'indent nil))
  "Alist of maximimum position in buffer where parser text properties are valid.")

(defun wisi-cache-max (&optional parse-action)
  ;; (move-marker (wisi-cache-max) foo) works
  (cdr (assoc (or parse-action wisi--parse-action) wisi--cache-max)))

(cl-defstruct wisi-tok
  token  ;; symbol from a token table
  region ;; cons giving buffer region containing token text

  nonterminal ;; t if a nonterminal

  ;; The following are set if parsing for indent.

  line ;; Line number at start of token. Nil for empty nonterminals

  first
  ;; For terminals, t if token is the first token on a line.
  ;;
  ;; For nonterminals, line number of first contained line that needs
  ;; indenting; it is a comment, or begins with a contained token.
  ;;
  ;; Otherwise nil.

  ;; The following are non-nil if token (terminal or non-terminal) is
  ;; followed by blank or comment lines, or if not parsing for indent.
  comment-line ;; first blank or comment line following token
  comment-end ;; position at end of blank or comment lines
  )

(defun wisi-token-text (token)
  "Return buffer text from token range."
  (let ((region (wisi-tok-region token)))
    (and region
       (buffer-substring-no-properties (car region) (cdr region)))))

(defun wisi-parse (automaton lexer)
  "Parse current buffer from bob using the automaton specified in AUTOMATON.

- AUTOMATON is the parse table generated by `wisi-compile-grammar'.

- LEXER is a function with no argument called by the parser to
  obtain the next token from the current buffer after point, as a
  wisi-tok object."

  ;; FIXME: (aref automaton 3) is the obarray storing the semantic actions;
  ;; not used here (see related FIXME in wisi-compile)
  (let* ((actions (aref automaton 0))
	 (gotos   (aref automaton 1))
	 (parser-states ;; vector of parallel parser states
	  (vector
	   (make-wisi-parser-state
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

    (goto-char (point-min))
    (forward-comment (point-max))
    (aset (wisi-parser-state-stack (aref parser-states 0)) 0 0)

    (setq token (funcall lexer))
    (setq wisi-parse-max-parallel-current (cons 0 0))

    (while (not (eq active 'accept))
      (setq active-parser-count-prev active-parser-count)
      (setq some-pending nil)
      (dotimes (parser-index (length parser-states))
	(when (eq active (wisi-parser-state-active (aref parser-states parser-index)))
	  (let* ((parser-state (aref parser-states parser-index))
		 (result (wisi-parse-1 token parser-state (> active-parser-count 1) actions gotos)))
	    (when result
	      ;; spawn a new parser
	      (when (= active-parser-count wisi-parse-max-parallel)
		(signal 'wisi-parse-error
			(let ((state (aref (wisi-parser-state-stack parser-state)
					   (wisi-parser-state-sp parser-state))))
			  (wisi-error-msg (concat "too many parallel parsers required in grammar state %d;"
						  " simplify grammar, or increase `wisi-parse-max-parallel'")
						  state))))

	      (let ((j (wisi-free-parser parser-states)))
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
		  (setf (wisi-parser-state-active result)
			(cl-case (wisi-parser-state-active result)
			  (shift 'pending-shift)
			  (reduce 'pending-reduce)
			 )))
		  )
		(setq active-parser-count (1+ active-parser-count))
		(when (> active-parser-count (car wisi-parse-max-parallel-current))
		  (setq wisi-parse-max-parallel-current (cons active-parser-count (point))))
		(setf (wisi-parser-state-label result) j)
		(aset parser-states j result))
	      (when (> wisi-debug 1)
                (message "spawn parser (%d active)" active-parser-count)))

	    (when (eq 'error (wisi-parser-state-active parser-state))
	      (setq active-parser-count (1- active-parser-count))
	      (when (> wisi-debug 1)
                (message "terminate parser (%d active)" active-parser-count))
	      (cl-case active-parser-count
		(0
		 (cond
		  ((= active-parser-count-prev 1)
		   ;; We were not in a parallel parse; report the error.
		   (let ((state (aref (wisi-parser-state-stack parser-state)
                                      (wisi-parser-state-sp parser-state))))
		     (signal 'wisi-parse-error
			     (wisi-error-msg "syntax error in grammar state %d; unexpected %s, expecting one of %s"
					     state
					     (wisi-token-text token)
					     (mapcar 'car (aref actions state))))
		     ))
		  (t
		   ;; Report errors from all parsers that failed on this token.
		   (let ((msg))
		     (dotimes (_ (length parser-states))
		       (let* ((parser-state (aref parser-states parser-index))
			      (state (aref (wisi-parser-state-stack parser-state)
					   (wisi-parser-state-sp parser-state))))
			 (when (eq 'error (wisi-parser-state-active parser-state))
			   (setq msg
				 (concat msg
					 (when msg "\n")
					 (wisi-error-msg
					  "syntax error in grammar state %d; unexpected %s, expecting one of %s"
					  state
					  (wisi-token-text token)
					  (mapcar 'car (aref actions state)))))
			   )))
		     (signal 'wisi-parse-error msg)))
		  ))

		(1
		 (setf (wisi-parser-state-active parser-state) nil); Don't save error for later.
		 (let ((parser-state (aref parser-states (wisi-active-parser parser-states))))
		   (wisi-execute-pending (wisi-parser-state-label parser-state)
					 (wisi-parser-state-pending parser-state))
		   (setf (wisi-parser-state-pending parser-state) nil)
		   ))
		(t
		 ;; We were in a parallel parse, and this parser
		 ;; failed; mark it inactive, don't save error for
		 ;; later.
		 (setf (wisi-parser-state-active parser-state) nil)
		 )))
	    )));; end dotimes

      (when some-pending
	;; Change pending-* parsers to *.
	(dotimes (parser-index (length parser-states))
	  (cond
	   ((eq (wisi-parser-state-active (aref parser-states parser-index)) 'pending-shift)
	    (setf (wisi-parser-state-active (aref parser-states parser-index)) 'shift))
	   ((eq (wisi-parser-state-active (aref parser-states parser-index)) 'pending-reduce)
	    (setf (wisi-parser-state-active (aref parser-states parser-index)) 'reduce))
	   )))

      (setq active (wisi-parsers-active parser-states active-parser-count))
      (when (eq active 'shift)
	(when (> active-parser-count 1)
	  (setq active-parser-count (wisi-parse-elim-identical parser-states active-parser-count)))

	(setq token (funcall lexer)))
    )
    (when (> active-parser-count 1)
      (error "ambiguous parse result"))))

(defun wisi-parsers-active-index (parser-states)
  ;; only called when active-parser-count = 1
  (let ((result nil)
	(i 0))
    (while (and (not result)
		(< i (length parser-states)))
      (when (wisi-parser-state-active (aref parser-states i))
	(setq result i))
      (setq i (1+ i)))
    result))

(defun wisi-parsers-active (parser-states active-count)
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
      (setq active (wisi-parser-state-active (aref parser-states i)))
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

(defun wisi-free-parser (parser-states)
  "Return index to a non-active parser in PARSER-STATES, -1 if there is none."
  (let ((result nil)
	(i 0))
    (while (and (not result)
		(< i (length parser-states)))
      (when (not (wisi-parser-state-active (aref parser-states i)))
	(setq result i))
      (setq i (1+ i)))
    (if result result -1)))

(defun wisi-active-parser (parser-states)
  "Return index to the first active parser in PARSER-STATES."
  (let ((result nil)
	(i 0))
    (while (and (not result)
		(< i (length parser-states)))
      (when (wisi-parser-state-active (aref parser-states i))
	(setq result i))
      (setq i (1+ i)))
    (unless result
      (error "no active parsers"))
    result))

(defun wisi-parse-elim-identical (parser-states active-parser-count)
  "Check for parsers in PARSER-STATES that have reached identical states eliminate one.
Return new ACTIVE-PARSER-COUNT. Assumes all parsers have active
nil, `shift', or `accept'."
  ;; parser-states passed by reference; active-parser-count by copy
  ;; see test/ada_mode-slices.adb for example
  (dotimes (parser-i (1- (length parser-states)))
    (when (wisi-parser-state-active (aref parser-states parser-i))
      (dotimes (parser-j (- (length parser-states) parser-i 1))
	(when (wisi-parser-state-active (aref parser-states (+ parser-i parser-j 1)))
	  (when (eq (wisi-parser-state-sp (aref parser-states parser-i))
		     (wisi-parser-state-sp (aref parser-states (+ parser-i parser-j 1))))
	    (let ((compare t))
	      (dotimes (stack-i (wisi-parser-state-sp (aref parser-states parser-i)))
		(setq
		 compare
		 (and compare ;; bypass expensive 'arefs' after first stack item compare fail
		      (equal (aref (wisi-parser-state-stack (aref parser-states parser-i)) stack-i)
			     (aref (wisi-parser-state-stack (aref parser-states (+ parser-i parser-j 1))) stack-i)))))
	      (when compare
		;; parser stacks are identical
		(setq active-parser-count (1- active-parser-count))
		(when (> wisi-debug 1)
		  (message "terminate identical parser %d (%d active)"
			   (+ parser-i parser-j 1) active-parser-count))
		(setf (wisi-parser-state-active (aref parser-states (+ parser-i parser-j 1))) nil)
		(when (= active-parser-count 1)
		  ;; the actions for the two parsers are not
		  ;; identical, but either is good enough for
		  ;; indentation and navigation, so we just do the
		  ;; actions for the one that is not terminating.
		  (let ((parser-state (aref parser-states parser-i)))
		    (wisi-execute-pending (wisi-parser-state-label parser-state)
					  (wisi-parser-state-pending parser-state))
		    (setf (wisi-parser-state-pending parser-state) nil)
		    ))
		))))
	)))
  active-parser-count)

(defun wisi-parse-max-pos (tokens)
  "Return max position in tokens, or point if tokens nil."
  (let ((result (if tokens 0 (point))))
    (mapc
     (lambda (token)
       ;; a token has a null region when it is an optional token that is empty
       (when (cdr (wisi-tok-region token))
	 (setq result (max (cdr (wisi-tok-region token)) result))))
     tokens)
    result)
  )

(defun wisi-parse-exec-action (func nonterm tokens)
  "Execute action if all tokens past wisi-cache-max."
  ;; We don't execute actions if all tokens are before wisi-cache-max,
  ;; because later actions can update existing caches, and if the
  ;; parse fails that won't happen. It also saves time.
  ;;
  ;; Also skip if no tokens; nothing to do. This can happen when all
  ;; tokens in a grammar statement are optional.
  (if (< 0 (length tokens))
      (if (and wisi--parse-action
	       (>= (wisi-parse-max-pos tokens) (wisi-cache-max)))

	  (funcall func nonterm tokens)

	(when (> wisi-debug 1)
	  (message "... action skipped; before wisi-cache-max %d" (marker-position (wisi-cache-max)))))

    (when (> wisi-debug 1)
      (message "... action skipped; no tokens"))
    ))

(defun wisi-execute-pending (parser-label pending)
  (when (> wisi-debug 1) (message "%d: pending actions:" parser-label))
  (while pending
    (when (> wisi-debug 1) (message "%s" (car pending)))

    (let ((func-args (pop pending)))
      (wisi-parse-exec-action (nth 0 func-args) (nth 1 func-args) (cl-caddr func-args)))
    ))

(defmacro wisi-parse-action (i al)
  "Return the parser action.
I is a token item number and AL is the list of (item . action)
available at current state.  The first element of AL contains the
default action for this state."
  `(cdr (or (assq ,i ,al) (car ,al))))

(defun wisi-parse-1 (token parser-state pendingp actions gotos)
  "Perform one shift or reduce on PARSER-STATE.
If PENDINGP, push actions onto PARSER-STATE.pending; otherwise execute them.
See `wisi-parse' for full details.
Return nil or new parser (a wisi-parse-state struct)."
  (let* ((state (aref (wisi-parser-state-stack parser-state)
		(wisi-parser-state-sp parser-state)))
	 (parse-action (wisi-parse-action (wisi-tok-token token) (aref actions state)))
	 new-parser-state)

    (when (> wisi-debug 1)
      ;; output trace info
      (if (> wisi-debug 2)
	  (progn
	    ;; put top 10 stack items
	    (let* ((count (min 20 (wisi-parser-state-sp parser-state)))
		   (msg (make-vector (+ 1 count) nil)))
	      (dotimes (i count)
		(aset msg (- count i)
		      (aref (wisi-parser-state-stack parser-state) (- (wisi-parser-state-sp parser-state) i)))
		)
	      (message "%d: %s: %d: %s"
		       (wisi-parser-state-label parser-state)
		       (wisi-parser-state-active parser-state)
		       (wisi-parser-state-sp parser-state)
		       msg))
	    (message "   %d: %s: %s" state token parse-action))
	(message "%d: %d: %s: %s" (wisi-parser-state-label parser-state) state token parse-action)))

    (when (and (listp parse-action)
	       (not (symbolp (car parse-action))))
      ;; Conflict; spawn a new parser.
      (setq new-parser-state
	    (make-wisi-parser-state
	     :active  nil
	     :stack   (vconcat (wisi-parser-state-stack parser-state))
	     :sp      (wisi-parser-state-sp parser-state)
	     :pending (wisi-parser-state-pending parser-state)))

      (wisi-parse-2 (cadr parse-action) token new-parser-state t gotos)
      (setq pendingp t)
      (setq parse-action (car parse-action))
      );; when

    ;; current parser
    (wisi-parse-2 parse-action token parser-state pendingp gotos)

    new-parser-state))

(defun wisi-parse-2 (action token parser-state pendingp gotos)
  "Execute parser ACTION (must not be a conflict).
Return nil."
  (cond
   ((eq action 'accept)
    (setf (wisi-parser-state-active parser-state) 'accept))

   ((eq action 'error)
    (setf (wisi-parser-state-active parser-state) 'error))

   ((natnump action)
    ;; Shift token and new state (= action) onto stack
    (let ((stack (wisi-parser-state-stack parser-state)); reference
	  (sp (wisi-parser-state-sp parser-state))); copy
      (setq sp (+ sp 2))
      (aset stack (1- sp) token)
      (aset stack sp action)
      (setf (wisi-parser-state-sp parser-state) sp))
    (setf (wisi-parser-state-active parser-state) 'shift))

   (t
    (wisi-parse-reduce action parser-state pendingp gotos)
    (setf (wisi-parser-state-active parser-state) 'reduce))
   ))

(defun wisi-first-last (stack i j)
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

(defvar wisi-parser-state nil
  "Let-bound in `wisi-parse-reduce', used in `wisi-parse-find-token'.")

(defun wisi-parse-find-token (token-symbol)
  "Find token with TOKEN-SYMBOL on current parser stack, return token struct.
For use in grammar actions."
  ;; Called from wisi-parse-exec-action in wisi-parse-reduce
  (let* ((stack (wisi-parser-state-stack wisi-parser-state))
	 (sp (1- (wisi-parser-state-sp wisi-parser-state)))
	 (tok (aref stack sp)))
    (while (and (> sp 0)
		(not (eq token-symbol (wisi-tok-token tok))))
      (setq sp (- sp 2))
      (setq tok (aref stack sp)))
    (if (= sp 0)
	(error "token %s not found on parser stack" token-symbol)
      tok)
    ))

(defun wisi-parse-first-token (token-number)
  "Find first token on LINE in `wisi-tokens' or on current parser stack,
return token struct.  For use in grammar actions."
  ;; Called from wisi-parse-exec-action in wisi-parse-reduce
  (let* ((line (wisi-tok-line (aref wisi-tokens (1- token-number))))
	 (stack (wisi-parser-state-stack wisi-parser-state))
	 (sp (1- (wisi-parser-state-sp wisi-parser-state)))
	 tok
	 (wisi-tokens-done nil)
	 (done nil))
    (while (and (> sp 0)
		(not done))
      (if wisi-tokens-done
	  (setq tok (aref stack sp))
	(setq tok (aref wisi-tokens (1- token-number))))

      (when (and
	     (wisi-tok-first tok)
	     (or (if (wisi-tok-nonterminal tok)
		     (= line (wisi-tok-first tok))
		   (= line (wisi-tok-line tok)))))
	(setq done t))

      (if wisi-tokens-done
	  (setq sp (- sp 2))
	(setq token-number (1- token-number))
	(setq wisi-tokens-done (= 0 token-number)))
      )

    (if (= sp 0)
	(error "first token on line %s not found on wisi-tokens or parser stack" line)
      tok)
    ))

(defun wisi-parse-reduce (action parser-state pendingp gotos)
  "Reduce PARSER-STATE.stack, and execute or pend ACTION."
  (let* ((wisi-parser-state parser-state);; reference, for wisi-parse-find-token
	 (stack (wisi-parser-state-stack parser-state)); reference
	 (sp (wisi-parser-state-sp parser-state)); copy
	 (token-count (nth 2 action))
	 (nonterm (nth 0 action))
	 (first-last (when (> token-count 0)
		       (wisi-first-last stack (- sp (* 2 (1- token-count)) 1) (1- sp))))
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
    (setf (wisi-parser-state-sp parser-state) sp)

    (when (nth 1 action)
      ;; nothing to do for a null user action
      (if pendingp
	  (if (wisi-parser-state-pending parser-state)
	      (setf (wisi-parser-state-pending parser-state)
		    (append (wisi-parser-state-pending parser-state)
			    (list (list (nth 1 action) nonterm tokens))))
	    (setf (wisi-parser-state-pending parser-state)
		  (list (list (nth 1 action) nonterm tokens))))

	;; Not pending.
	(wisi-parse-exec-action (nth 1 action) nonterm tokens)
	))
    ))

(provide 'wisi-parse)
;;; wisi-parse.el ends here
