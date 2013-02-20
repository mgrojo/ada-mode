;; Wisi parser
;;
;; An extended LALR parser, that handles shift/reduce and
;; reduce/reduce conflicts by spawning parallel parsers to follow each
;; path.

(require 'semantic/wisent)
(require 'cl)

(defstruct (wisi-parser-state
	    (:copier nil))
  active
  ;; 'shift  - need new token
  ;; 'reduce - need reduce
  ;; 'accept - parsing completed
  ;; nil     - terminated

  stack
  ;; Each stack item takes two slots: (token-symbol token-text (token-start . token-end)), state
  ;; token-text is nil for nonterminals.
  ;; this is _not_ the same as the wisent-parse stack; that leaves out token-symbol.

  sp ;; stack pointer

  pending
  ;; list of (action-symbol stack-fragment)
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
	    :active  'shift
	    :stack   (make-vector wisent-parse-max-stack-size nil);; FIXME: grow stack dynamically, save per-buffer max
	    :sp      0
	    :pending nil)))
	 (active-parser-count 1)
         (wisent-parse-error-function (or error 'wisent-message))
	 (active 'shift)
	 (token (funcall lexer)))

    (aset (wisi-parser-state-stack (aref parser-states 0)) 0 0) ;; Initial state

    (while (not (eq active 'accept))
      (dotimes (parser-index (length parser-states))
	(when (eq active (wisi-parser-state-active (aref parser-states parser-index)))
	  (let* ((parser-state (aref parser-states parser-index))
		 (result (wisi-parse-1 token parser-state (> active-parser-count 1) actions gotos)))
	    (when result
	      ;; spawn a new parser
	      (let ((j (wisi-free-parser parser-states)))
		(when (= j -1)
		  (setq parser-states (vconcat parser-states (vector nil)))
		  (setq j (1- (length parser-states))))
		(setq active-parser-count (1+ active-parser-count))
		(aset parser-states j result)))

	    (when (eq 'error (wisi-parser-state-active parser-state))
	      ;; terminate this parser
	      (setf (wisi-parser-state-active parser-state) nil)
	      (setq active-parser-count (1- active-parser-count))
	      (case active-parser-count
		(0
		 ;; FIXME: better error handling for input syntax
		 ;; error? not clear which parser(s) should try to
		 ;; continue past error.
		 ;; FIXME: at least a better message to the user
		 ;; if prev-active-parser-count = 1, handle like wisent-parse does
		 (error "no parsers left"))

		(1
		 (wisi-execute-pending (wisi-parser-state-pending
					(aref parser-states (wisi-active-parser parser-states)))))
		(t nil)))

	    )));; end dotimes

      (setq active (wisi-parsers-active parser-states active-parser-count))
      (when (eq active 'shift)
	(setq token (funcall lexer)))
    )
    (when (> active-parser-count 1)
      (error "ambiguous parse result"))))

(defun wisi-parsers-active (parser-states active-count)
  "Return the type of parser cycle to execute.
PARSER-STATES.active is the last action the parser took. If it
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

(defun wisi-execute-pending (pending)
  (while pending
    (apply 'funcall (pop pending))))

(defun wisi-parse-1 (token parser-state pendingp actions gotos)
  "Perform one shift or reduce on PARSER-STATE.
If PENDINGP, push actions onto PARSER-STATE.pending; otherwise execute them.
See `wisi-parse' for full details.
Return nil or new parser (a wisi-parse-state struct)."
  (let* ((state (aref (wisi-parser-state-stack parser-state)
		(wisi-parser-state-sp parser-state)))
	 (parse-action (wisent-parse-action (car token) (aref actions state)))
	 new-parser-state)

    (when (and (listp parse-action)
	       (not (symbolp (car parse-action))))
      ;; conflict; spawn a new parser
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
    (aset stack (1- sp) (cons nonterm (cons nil nonterm-region)))
    (aset stack sp new-state)
    (setf (wisi-parser-state-sp parser-state) sp)
    (if pendingp
	(push (list (nth 1 action) tokens) (wisi-parser-state-pending parser-state))
      (funcall (nth 1 action) tokens))
    ))

(provide 'wisi-parse)
;; end of file
