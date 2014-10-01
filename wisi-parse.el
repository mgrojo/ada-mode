;;; wisi-parse.el --- Wisi parser

;; Copyright (C) 2013, 2014  Free Software Foundation, Inc.

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
(require 'semantic/wisent)
(require 'wisi-parse-common)

(defvar wisi-parse-max-parallel 15
  "Maximum number of parallel parsers for acceptable performance.
If a file needs more than this, it's probably an indication that
the grammar is excessively redundant.")

(defvar wisi-parse-max-parallel-current (cons 0 0)
  "Cons (count . point); Maximum number of parallel parsers used in most recent parse,
point at which that max was spawned.")

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
  ;; Each stack item takes two slots: (token-symbol token-text (token-start . token-end)), state
  ;; token-text is nil for nonterminals.
  ;; this is _not_ the same as the wisent-parse stack; that leaves out token-symbol.

  sp ;; stack pointer

  pending
  ;; list of (action-symbol stack-fragment)
  )

(defun wisi-parse (automaton lexer)
  "Parse current buffer from bob using the automaton specified in AUTOMATON.

- AUTOMATON is the parse table generated by `wisi-compile-grammar'.

- LEXER is a function with no argument called by the parser to
  obtain the next token from the current buffer after point, as a
  list (symbol text start . end), where `symbol' is the terminal
  symbol, `text' is the token string, `start . end' is the range
  in the buffer."
  (let* ((actions (aref automaton 0))
	 (gotos   (aref automaton 1))
	 (parser-states ;; vector of parallel parser states
	  (vector
	   (make-wisi-parser-state
	    :label 0
	    :active  'shift
	    :stack   (make-vector wisent-parse-max-stack-size nil)
	    :sp      0
	    :pending nil)))
	 (active-parser-count 1)
	 active-parser-count-prev
	 (active 'shift)
	 (token nil)
	 some-pending
	 )

    (goto-char (point-min))
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
						  " simplify grammar, or increase `wisi-parse-max-parallel'"
						  state)))))

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
was 'shift, that parser used the input token, and should not be
executed again until another input token is available, after all
parsers have shifted the current token or terminated.

Returns one of:

'accept : all PARSER-STATES have active set to nil or 'accept -
done parsing

'shift : all PARSER-STATES have active set to nil, 'accept, or
'shift - get a new token, execute 'shift parsers.

'reduce : some PARSER-STATES have active set to 'reduce - no new
token, execute 'reduce parsers."
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
nil, 'shift, or 'accept."
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
       (when (cddr token)
	 (setq result (max (cddr token) result))))
     tokens)
    result)
  )

(defun wisi-parse-exec-action (func tokens)
  "Execute action if all tokens past wisi-cache-max."
  ;; We don't execute actions if all tokens are before wisi-cache-max,
  ;; because later actions can update existing caches, and if the
  ;; parse fails that won't happen. It also saves time.
  (if (>= (wisi-parse-max-pos tokens) wisi-cache-max)

      (funcall func tokens)

    (when (> wisi-debug 1)
      (message "... action skipped; before wisi-cache-max"))
    ))

(defun wisi-execute-pending (parser-label pending)
  (when (> wisi-debug 1) (message "%d: pending actions:" parser-label))
  (while pending
    (when (> wisi-debug 1) (message "%s" (car pending)))

    (let ((func-args (pop pending)))
      (wisi-parse-exec-action (car func-args) (cadr func-args)))
    ))

(defun wisi-parse-1 (token parser-state pendingp actions gotos)
  "Perform one shift or reduce on PARSER-STATE.
If PENDINGP, push actions onto PARSER-STATE.pending; otherwise execute them.
See `wisi-parse' for full details.
Return nil or new parser (a wisi-parse-state struct)."
  (let* ((state (aref (wisi-parser-state-stack parser-state)
		(wisi-parser-state-sp parser-state)))
	 (parse-action (wisent-parse-action (car token) (aref actions state)))
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

(defun wisi-nonterm-bounds (stack i j)
  "Return a pair (START . END), the buffer region for a nonterminal.
STACK is the parser stack.  I and J are the indices in STACK of
the first and last tokens of the nonterminal."
  (let ((start (cadr (aref stack i)))
        (end   (cddr (aref stack j))))
    (while (and (or (not start) (not end))
		(/= i j))
      (cond
       ((not start)
	;; item i is an empty production
	(setq start (cadr (aref stack (setq i (+ i 2))))))

       ((not end)
	;; item j is an empty production
	(setq end (cddr (aref stack (setq j (- j 2))))))

       (t (setq i j))))
    (and start end (cons start end))))

(defun wisi-parse-reduce (action parser-state pendingp gotos)
  "Reduce PARSER-STATE.stack, and execute or pend ACTION."
  (let* ((stack (wisi-parser-state-stack parser-state)); reference
	 (sp (wisi-parser-state-sp parser-state)); copy
	 (token-count (or (nth 2 action) 0))
	 (nonterm (nth 0 action))
	 (nonterm-region (when (> token-count 0)
			   (wisi-nonterm-bounds stack (- sp (* 2 (1- token-count)) 1) (1- sp))))
	 (post-reduce-state (aref stack (- sp (* 2 token-count))))
	 (new-state (cdr (assoc nonterm (aref gotos post-reduce-state))))
	 tokens)
    (when (not new-state)
      (error "no goto for %s %d" nonterm post-reduce-state))
    (if (= 1 token-count)
	(setq tokens (list (aref stack (1- sp))))
      (dotimes (i token-count)
	(push (aref stack (- sp (* 2 i) 1)) tokens)))
    (setq sp (+ 2 (- sp (* 2 token-count))))
    (aset stack (1- sp) (cons nonterm nonterm-region))
    (aset stack sp new-state)
    (setf (wisi-parser-state-sp parser-state) sp)
    (if pendingp
	(if (wisi-parser-state-pending parser-state)
	    (setf (wisi-parser-state-pending parser-state)
		  (append (wisi-parser-state-pending parser-state)
			  (list (list (nth 1 action) tokens))))
	  (setf (wisi-parser-state-pending parser-state)
		(list (list (nth 1 action) tokens))))

      ;; Not pending.
      (wisi-parse-exec-action (nth 1 action) tokens)
      )
    ))

(provide 'wisi-parse)
;;; wisi-parse.el ends here
