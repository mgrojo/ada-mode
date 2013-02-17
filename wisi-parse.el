;; Wisi parser
;;
;; An extended LALR parser, that handles shift/reduce and
;; reduce/reduce conflicts by spawning parallel parsers to follow each
;; path.

(require 'semantic/wisent)
(require 'cl)

(defstruct (wisi-parser-state
	    (:copier nil))
  active ;; nil when terminated
  stack
  sp ;; stack pointer

  pending
  ;; list of (action-symbol stack-fragment)

  state ;; LALR parser state
  )

(defun wisi-parse (automaton lexer &optional error)
  "Parse input using the automaton specified in AUTOMATON.

- AUTOMATON is the parse table generated by `wisi-compile-grammar'.

- LEXER is a function with no argument called by the parser to
  obtain the next token in input, as a list (symbol text start
  . end), where `symbol' is the terminal symbol, `text' is the
  token string, `start . end' is the range in the buffer.

- ERROR is an optional reporting function called when a parse error
  occurs.  It receives a message string to report.  It defaults to the
  function `wisent-message'."
  (let* ((actions (aref automaton 0))
	 (gotos   (aref automaton 1))
	 (parser-states ;; vector of parallel parser states
	  (vector
	   (make-wisi-parser-state
	    :active t
	    :stack   (make-vector wisent-parse-max-stack-size nil);; FIXME: grow stack dynamically, save per-buffer max
	    :sp      0
	    :pending nil
	    :state   nil)))
	 (max-parser-state 0)
	 (active-parser-count 1)
         (wisent-parse-error-function (or error 'wisent-message))
	 (done nil)
	 i)

    (setq wisent-lookahead nil)
    ;; FIXME: global so next invocation of wisent-parse can see it?
    ;; Which means we should not set it nil here!

    (dotimes (i (1+ max-parser-state))
      (aset (wisi-parser-state-stack (aref parser-states i)) 0 0)) ;; Initial state

    (while (not done)
      (dotimes (i (1+ max-parser-state))
	(when (wisi-parser-state-active (aref parser-states i))
	  (let* ((state (aref parser-states i))
		 (result (wisi-parse-1 (funcall lexer) state (> active-parser-count 1) gotos)))
	    (cond
	     ((eq 'accept result)
	      (wisi-execute-pending state)
	      (setq done t))

	     ((eq 'shift result) nil)

	     ((eq 'error result)
	      ;; terminate this parser
	      (setf (wisi-parser-state-active state) nil)
	      (setq active-parser-count (1- active-parser-count))
	      (when (= active-parser-count 1)
		(wisi-execute-pending (aref parser-states (wisi-find-active parser-states)))))

	     ((listp result)
	      ;; spawn a new parser
	      (let ((j (wisi-free-parser parser-states)))
		(when (= j -1)
		  (setq parser-states
			(vconcat parser-states
				 (vector
				  (make-wisi-parser-state
				   :active t
				   :stack   (make-vector wisent-parse-max-stack-size nil)
				   :sp      0
				   :pending nil
				   :state   nil))))
		  (setq max-parser-state (1+ max-parser-state))
		  (setq j max-parser-state))
		(setq active-parser-count (1+ active-parser-count))
		(let ((state (aref parser-states j)))
		  (setf (wisi-parser-state-active state) t)
		  (setf (wisi-parser-state-stack state) (nth 0 result))
		  (setf (wisi-parser-state-sp state) (nth 1 result))
		  (setf (wisi-parser-state-pending state) (list (nth 2 result))))
		))
	     )));; end when/let/cond
	));; end while/while
    ))

(defun wisi-parse-1 (token parser-state pendingp gotos)
  "Parse TOKEN, modifying PARSER-STATE; perform all possible reductions.
If PENDINGP, push actions onto parser-state.pending; otherwise execute them.
See `wisi-parse' for full details.
Return one of:
'accept: parser completed
'shift : need next token
'error : terminate this parser
'(stack sp pending shift/reduce): spawn a new parser, and need next token."
  (let* ((loop-action t)
	 (result nil)
	 tmp-result)
    (while loop-action
      (setf (wisi-parser-state-state parser-state)
	    (aref (wisi-parser-state-stack parser-state)
		  (wisi-parser-state-sp parser-state)))
      (setq loop-action (wisent-parse-action (car token) (aref actions (wisi-parser-state-state parser-state))))

      (when (listp loop-action)
	;; conflict; spawn a new parser
	(let ((stack-copy (wisi-parser-state-stack parser-state))
	      (sp-copy (wisi-parser-state-sp parser-state))
	      (pending-copy (wisi-parser-state-pending parser-state)))
	  ;; conflict can be either reduce/reduce, or shift/reduce; we
	  ;; don't need to handle 'accept or 'error here.
	  (wisi-parse-2 (cadr loop-action) token stack-copy sp-copy gotos t pending-copy)

	  ;; we continue wisi-parse-1 with the current parser, until
	  ;; it hits a shift. There can be only reductions between a
	  ;; conflict and a shift; no more conflicts, accepts, or errors.
	  (when result
	    (error "multiple conflicts in wisi-parse-1"))

	  (setq result (list (stack-copy sp-copy pending-copy)))
	  (setq pendingp t)
	  (setq loop-action (car loop-action))
	  ))

      ;; current parser
      (setq tmp-result
	    (wisi-parse-2 loop-action
			  token
			  (wisi-parser-state-stack parser-state)
			  (wisi-parser-state-sp parser-state)
			  pendingp
			  (wisi-parser-state-pending parser-state)
			  gotos))
      (when (memq tmp-result '(accept error shift))
	(setq loop-action nil))

      (setq result (or result tmp-result)))

    result))

(defun wisi-parse-2 (action token stack sp pendingp pending gotos)
  "Execute parser ACTION (must not be a conflict).
Return one of 'accept, 'error, 'shift 'reduce."
  (cond
   ((eq action 'accept)
    'accept)

   ((eq action 'error)
    'error)

   ((natnump action)
    ;; Shift token and new state (= action) onto stack
    (setq sp (+ sp 2))
    (aset stack (1- sp) token)
    (aset stack sp action)
    'shift)

   (t
    (wisi-parse-reduce action stack sp pendingp pending gotos)
    'reduce)))

(defun wisi-parse-reduce (action stack sp pendingp gotos)
  "Reduce STACK, and execute or pend ACTION."
  (let ((nonterm (nth 0 action))
	(token-count (nth 2 action))
	(post-reduce-state (aref stack (- sp (2 * token-count))))
	tokens)
    (do ((i 0)) (i > token-count)
      (push (aref stack (- sp (* 2 i) 1)) tokens))
    (setq sp (+ 2 (- sp (2 * token-count))))
    (aset stack (1- sp) nonterm)
    (aset stack sp (assoc nonterm (aref gotos post-reduce-state)))
    (if pendingp
	(push (append (nth 1 action) tokens))
      (funcall (nth 1 action) tokens))
    ))

(provide 'wisi-parse)
;; end of file
