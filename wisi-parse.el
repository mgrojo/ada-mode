;; Wisi parser
;;
;; An extended LALR parser, that handles shift/reduce and
;; reduce/reduce conflicts by spawning parallel parsers to follow each
;; path.

(require 'semantic/wisent)
(require 'cl)

(defstruct (wisi-parser-state
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

(defun wisi-error-msg (message &rest args)
  (let ((line (line-number-at-pos))
	(col (- (point) (line-beginning-position))))
    (format
     "%s:%d:%d: %s"
       (file-name-nondirectory (buffer-name)) ;; buffer-file-name is sometimes nil here!?
       line col
       (apply 'format message args))))

(defun wisi-error (message &rest args)
  (error (wisi-error-msg message args)))

(defvar wisi-parse-error nil)
(put 'wisi-parse-error
     'error-conditions
     '(error wisi-parse-error))
(put 'wisi-parse-error
     'error-message
     "wisi parse error")

(defun wisi-parse (automaton lexer)
  "Parse input using the automaton specified in AUTOMATON.

- AUTOMATON is the parse table generated by `wisi-compile-grammar'.

- LEXER is a function with no argument called by the parser to
  obtain the next token in input, as a list (symbol text start
  . end), where `symbol' is the terminal symbol, `text' is the
  token string, `start . end' is the range in the buffer."
  (let* ((actions (aref automaton 0))
	 (gotos   (aref automaton 1))
	 (parser-states ;; vector of parallel parser states
	  (vector
	   (make-wisi-parser-state
	    :label 0
	    :active  'shift
	    :stack   (make-vector wisent-parse-max-stack-size nil)
	    ;; FIXME: better error message when stack overflows, so
	    ;; user can set wisent-parse-max-stack-size in file-local
	    ;; vars.
	    :sp      0
	    :pending nil)))
	 (active-parser-count 1)
	 active-parser-count-prev
	 (active 'shift)
	 (token (funcall lexer))
	 some-pending)

    (aset (wisi-parser-state-stack (aref parser-states 0)) 0 0) ;; Initial state

    (while (not (eq active 'accept))
      (setq active-parser-count-prev active-parser-count)
      (setq some-pending nil)
      (dotimes (parser-index (length parser-states))
	(when (eq active (wisi-parser-state-active (aref parser-states parser-index)))
	  (let* ((parser-state (aref parser-states parser-index))
		 (result (wisi-parse-1 token parser-state (> active-parser-count 1) actions gotos)))
	    (when result
	      ;; spawn a new parser
	      ;; FIXME: put new parser on new-parser-list, add to parser-states after this parser-index loop
	      (let ((j (wisi-free-parser parser-states)))
		(cond
		 ((= j -1)
		  ;; add to parser-states; the new parser won't be executed again in this parser-index loop
		  (setq parser-states (vconcat parser-states (vector nil)))
		  (setq j (1- (length parser-states))))
		 ((< j parser-index)
		  ;; the new parser won't be executed again in this parser-index loop; nothing to do
		  )
		 (t
		  ;; don't let the new parser execute again in this parser-index loop
		  (setq some-pending t)
		  (setf (wisi-parser-state-active result)
			(case (wisi-parser-state-active result)
			  (shift 'pending-shift)
			  (reduce 'pending-reduce)
			 )))
		  )
		(setq active-parser-count (1+ active-parser-count))
		(setf (wisi-parser-state-label result) j)
		(aset parser-states j result))
	      (when (> wisi-debug 1) (message "spawn parser (%d active)" active-parser-count)))

	    (when (eq 'error (wisi-parser-state-active parser-state))
	      (setq active-parser-count (1- active-parser-count))
	      (when (> wisi-debug 1) (message "terminate parser (%d active)" active-parser-count))
	      (case active-parser-count
		(0
		 (cond
		  ((= active-parser-count-prev 1)
		   ;; we were not in a parallel parse; report the error
		   (let ((state (aref (wisi-parser-state-stack parser-state) (wisi-parser-state-sp parser-state))))
		     (signal 'wisi-parse-error
			     (wisi-error-msg "syntax error in grammar state %d; unexpected %s, expecting one of %s"
					     state
					     (nth 1 token)
					     (mapcar 'car (aref actions state))))
		     ))
		  (t
		   ;; report errors from all parsers that failed on this token
		   (let ((msg))
		     (dotimes (index (length parser-states))
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
					  (nth 1 token)
					  (mapcar 'car (aref actions state)))))
			   )))
		     (signal 'wisi-parse-error msg)))
		  ))

		(1
		 (setf (wisi-parser-state-active parser-state) nil); don't save error for later
		 (wisi-execute-pending (wisi-parser-state-pending
					(aref parser-states (wisi-active-parser parser-states))))
		 (setf (wisi-parser-state-pending
			(aref parser-states (wisi-active-parser parser-states)))
		       nil))
		(t
		 ;; we were in a parallel parse, and this parser
		 ;; failed; mark it inactive, don't save error for
		 ;; later
		 (setf (wisi-parser-state-active parser-state) nil)
		 )))
	    )));; end dotimes

      (when some-pending
	;; change pending-* parsers to *
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

(defun wisi-parsers-active (parser-states active-count)
  "Return the type of parser cycle to execute.
PARSER-STATES[*].active is the last action a parser took. If it
was 'shift, that parser used the input token, and should not be
executed again until another input token is available, after all
parsers have shifted the current token or terminated.

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
     (t (error "unexpected result in wisi-parsers-active"))
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
		 (and compare
		      (equal (aref (wisi-parser-state-stack (aref parser-states parser-i)) stack-i)
			     (aref (wisi-parser-state-stack (aref parser-states (+ parser-i parser-j 1))) stack-i)))))
	      (when compare
		;; parser stacks are identical
		(setq active-parser-count (1- active-parser-count))
		(when (> wisi-debug 1) (message "terminate identical parser %d (%d active)"
						(+ parser-i parser-j 1) active-parser-count))
		(when (= active-parser-count 1)
		  ;; the actions for the two parsers are not
		  ;; identical, but either is good enough for
		  ;; indentation and navigation, so we just do one.
		  (wisi-execute-pending (wisi-parser-state-pending (aref parser-states (+ parser-i parser-j 1))))
		  (setf (wisi-parser-state-pending (aref parser-states (+ parser-i parser-j 1))) nil)

		  ;; clear pending of other parser so it can be reused
		  (setf (wisi-parser-state-pending (aref parser-states parser-i)) nil))

		(setf (wisi-parser-state-active (aref parser-states (+ parser-i parser-j 1))) nil))
	      )))
	)))
  active-parser-count)

(defun wisi-execute-pending (pending)
  (while pending
    (apply (pop pending))))

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
  (let ((start (caddr (aref stack i)))
        (end   (cdddr (aref stack j))))
    (while (and (or (not start) (not end))
		(/= i j))
      (cond
       ((not start)
	;; item i is an empty production
	(setq start (caddr (aref stack (setq i (+ i 2))))))

       ((not end)
	;; item j is an empty production
	(setq end (cdddr (aref stack (setq j (- j 2))))))

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
    (aset stack (1- sp) (cons nonterm (cons nil nonterm-region)))
    (aset stack sp new-state)
    (setf (wisi-parser-state-sp parser-state) sp)
    (if pendingp
	(if (wisi-parser-state-pending parser-state)
	    (setf (wisi-parser-state-pending parser-state)
		  (append (wisi-parser-state-pending parser-state)
			  (list (list (nth 1 action) tokens))))
	  (setf (wisi-parser-state-pending parser-state)
		(list (list (nth 1 action) tokens))))
      (funcall (nth 1 action) tokens))
    ))

(provide 'wisi-parse)
;; end of file
