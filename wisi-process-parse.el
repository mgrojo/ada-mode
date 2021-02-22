;;; wisi-process-parse.el --- interface to external parse program
;;
;; Copyright (C) 2014, 2017 - 2021 Free Software Foundation, Inc.
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
(require 'wisi-parse-common)

(defconst wisi-process-parse-protocol-version "6"
  "Defines data exchanged between this package and the background process.
Must match emacs_wisi_common_parse.ads Protocol_Version.")

(defconst wisi-process-parse-prompt "^;;> "
  "Regexp matching executable prompt; indicates previous command is complete.")

(defconst wisi-process-parse-quit-cmd "004quit\n"
  "Command to external process telling it to quit.")

;;;;; sessions

;; The executable builds internal parser structures on startup,
;; then runs a loop, waiting for parse requests.
;;
;; We only need one process per language; there is no persistent state
;; in the process between parses, and processes are too heavy-weight
;; to have one per buffer. We use a global alist of parser objects to
;; find the right one for the current buffer.

(cl-defstruct (wisi-process--parser (:include wisi-parser))
  (label nil)             ;; string uniquely identifying parser
  language-protocol-version ;; string identifying language-specific params
  (exec-file nil) 	  ;; absolute file name of executable
  (exec-opts nil)         ;; list of process start options for executable
  (token-table nil)       ;; vector of token symbols, indexed by integer
  (face-table nil) 	  ;; vector of face symbols, indexed by integer
  (busy nil)              ;; t while parser is active
  (process nil) 	  ;; running *_wisi_parse executable
  (buffer nil) 		  ;; receives output of executable
  line-begin              ;; vector of beginning-of-line positions in buffer
  (total-wait-time 0.0)   ;; total time during last parse spent waiting for subprocess output.
  (response-count 0)      ;; responses received from subprocess during last parse; for profiling.
  end-pos                 ;; last character position parsed
  language-action-table   ;; array of function pointers, each taking an sexp sent by the process
  )

(cl-defmethod wisi-parser-transaction-log-buffer-name ((parser wisi-process--parser))
  (concat "*"(wisi-process--parser-label parser) "-wisi-parser-log*"))

(defvar wisi-process--alist nil
  "Alist mapping string label to ‘wisi-process--session’ struct")

(defvar wisi-file_not_found nil
  "Signal handled internally by functions in this file")
(put 'wisi-file_not_found
     'error-conditions
     '(wisi-file_not_found))

;;;###autoload
(defun wisi-process-parse-get (parser)
  "Return a ‘wisi-process--parser’ object matching PARSER label.
If label found in ‘wisi-process--alist’, return that.
Otherwise add PARSER to ‘wisi-process--alist’, return it."
  (or (cdr (assoc (wisi-process--parser-label parser) wisi-process--alist))
      (let ((exec-file (locate-file (wisi-process--parser-exec-file parser) exec-path '("" ".exe"))))

	(unless exec-file
	  (error "%s not found on `exec-path'; run 'build.sh' in the ELPA package."
		 (wisi-process--parser-exec-file parser)))

	(push (cons (wisi-process--parser-label parser) parser) wisi-process--alist)

	parser
     )))

(defun wisi-process-parse-set-exec (label exec-file)
  "Change the EXEC-FILE for parsers with LABEL."
  (let ((parser (cdr (assoc label wisi-process--alist))))
    (when parser
      (wisi-parse-kill parser)
      (setf (wisi-process--parser-exec-file parser) exec-file))))

(defun wisi-process-parse--check-version (parser)
  "Verify protocol version reported by process."
  ;; The process has just started; the first non-comment line in the
  ;; process buffer contains the process and language protocol versions.
  (with-current-buffer (wisi-process--parser-buffer parser)
    (goto-char (point-min))
    (if (search-forward-regexp "protocol: process version \\([0-9]+\\) language version \\([0-9]+\\)" nil t)
	(unless (and (match-string 1)
		     (string-equal (match-string 1) wisi-process-parse-protocol-version)
		     (match-string 2)
		     (string-equal (match-string 2) (wisi-process--parser-language-protocol-version parser)))
	  (wisi-parse-kill parser)
	  (error "%s parser process protocol version mismatch: elisp %s %s, process %s %s"
		 (wisi-process--parser-label parser)
		 wisi-process-parse-protocol-version (wisi-process--parser-language-protocol-version parser)
		 (match-string 1) (match-string 2)))
      ;; Search failed
      (error "%s parser process protocol version message not found"
	     (wisi-process--parser-label parser))
    )))

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

      (wisi-parse-log-message parser "create process")

      (setf (wisi-process--parser-process parser)
	    (make-process
	     :name process-name
	     :buffer (wisi-process--parser-buffer parser)
	     :command (append (list (wisi-process--parser-exec-file parser))
			      (wisi-process--parser-exec-opts parser))))

      (set-process-query-on-exit-flag (wisi-process--parser-process parser) nil)

      (wisi-process-parse--wait parser)
      (wisi-process-parse--check-version parser)
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
	(accept-process-output process 0.1))

      (unless found
	(wisi-parse-log-message parser "process died")
	(error "%s process died" (wisi-process--parser-exec-file parser)))
      )))

(defun wisi-process-parse-show-buffer (parser)
  "Show PARSER buffer."
  (if (buffer-live-p (wisi-process--parser-buffer parser))
      (pop-to-buffer (wisi-process--parser-buffer parser))
    (error "wisi-process-parse process not active")))

(defun wisi-process-parse--send-parse (parser parse-action begin send-end parse-end)
  "Send a full or partial PARSE-ACTION command to PARSER external process.
The command is followed by the content of the current buffer from
BEGIN thru SEND-END.  Does not wait for command to
complete. PARSE-END is end of desired parse region."
  ;; Must match "full/partial parse" command arguments read by
  ;; emacs_wisi_common_parse.adb Get_Parse_Params.
  ;; Parse_Kind is always Partial here; that really means "legacy".
  (let* ((cmd (format "parse 0 %d \"%s\" %d %d %d %d %d %d %d %d %d \"%s\" %d %d %d %d %d \"%s\""
		      (cl-ecase parse-action
			(navigate 0)
			(face 1)
			(indent 2))
		      (if (buffer-file-name) (buffer-file-name) (buffer-name))
		      (position-bytes begin)
		      (position-bytes send-end)
		      (position-bytes (min (point-max) parse-end))
		      begin ;; begin_char_pos
		      send-end ;; end_char_pos
		      (line-number-at-pos begin)
		      (line-number-at-pos send-end) ;; line-end (at EOI)

		      ;; begin_indent. Example:
		      ;;
		      ;; end if;
		      ;;
		      ;;    if ...
		      ;;    end if;
		      ;;
		      ;; Indenting 'if ...'; ada-wisi-expand-region
		      ;; returns BEGIN after first 'end if;', SEND-END
		      ;; after second 'end if'. Begin_indent is first
		      ;; 'end if;'
		      (save-excursion
			(goto-char begin)
			(back-to-indentation)
			(current-column))

		      (if (or (and (= begin (point-min)) (= parse-end (point-max)))
			      (< (point-max) wisi-partial-parse-threshold))
			  0 1) ;; partial parse active
		      wisi-parser-verbosity
		      (or wisi-mckenzie-task-count -1)
		      (or wisi-mckenzie-zombie-limit -1)
		      (or wisi-mckenzie-enqueue-limit -1)
		      (or wisi-parse-max-parallel -1)
		      (- (position-bytes send-end) (position-bytes begin)) ;; byte_count: send-end is after last byte
		      (wisi-parse-format-language-options parser)
		      ))
	 (msg (format "%04d%s" (length cmd) cmd))
	 (process (wisi-process--parser-process parser)))

    (with-current-buffer (wisi-process--parser-buffer parser)
      (erase-buffer))

    (wisi-parse-log-message parser cmd)
    (process-send-string process msg)

    ;; we don't log the buffer text; may be huge
    (process-send-string process (buffer-substring-no-properties begin send-end))

    ;; We don’t wait for the send to complete here.
    ))

(defun wisi-process-parse--send-incremental-parse (parser full)
  "Send an incremental parse command to PARSER external process.
If FULL, do initial full parse.  Does not wait for command to
complete."
  ;; Must match "incremental parse" command arguments read by
  ;; emacs_wisi_common_parse.adb Get_Parse_Params.
  (let* ((cmd
	  (apply #'format
		 (concat
		  "parse %d \"%s\" \"%s\" %d %d %d %d %d "
		  (if full "%d %d" "%s")
		  " \"%s\""
		  )
		 (append
		  (list
		   (if full 2 1) ;; Parse_Kind
		   (if (buffer-file-name) (buffer-file-name) (buffer-name))
		   wisi-parser-verbosity
		   (or wisi-mckenzie-task-count -1)
		   (or wisi-mckenzie-zombie-limit -1)
		   (or wisi-mckenzie-enqueue-limit -1)
		   (or wisi-parse-max-parallel -1)
		   (if full (- (position-bytes (point-max)) (position-bytes (point-min))) 0) ;; byte_count
		   )
		  (if full
		      (list
		       (point-max) ;; end_char_pos (after last char)
		       (line-number-at-pos (1- (point-max))) ;; End_Line (at last char)
		       )
		    (if wisi--changes
			  (list (prin1-to-string (nreverse wisi--changes)));; wisi--changes is in reverse time order.
		      ;; Incremental parse requested, but
		      ;; wisi--changes is empty. This is most likely
		      ;; after a wisi-reset-parser; either in a unit
		      ;; test or by the user.
		      "()"
		      ))
		  (list (wisi-parse-format-language-options parser))
		  )))
	 (msg (format "%04d%s" (length cmd) cmd))
	 (process (wisi-process--parser-process parser)))

    (with-current-buffer (wisi-process--parser-buffer parser)
      (erase-buffer))

    (wisi-parse-log-message parser cmd)
    (process-send-string process msg)
    (setq wisi--changes nil)
    (when full
      (process-send-string process (buffer-substring-no-properties (point-min) (point-max))))

    ;; We don’t wait for the send to complete here.
    ))

(defun wisi-process-parse--send-action (parser parse-action begin end)
  "Send a post-parse PARSE-ACTION command to PARSER external process.
Does not wait for command to complete."
  ;; Must match emacs_wisi_common_parse.adb Get_Parse_Action.
  (let* ((cmd (format "post-parse \"%s\" \"%s\" %d %d %d %d %d \"%s\""
		      (if (buffer-file-name) (buffer-file-name) (buffer-name))
		      wisi-parser-verbosity
		      (cl-ecase parse-action
			(navigate 0)
			(face 1)
			(indent 2))
		      (position-bytes begin)
		      begin
		      (position-bytes end)
		      end
		      (wisi-parse-format-language-options parser)
		      ))
	 (msg (format "%04d%s" (length cmd) cmd))
	 (process (wisi-process--parser-process parser)))

    (with-current-buffer (wisi-process--parser-buffer parser)
      (erase-buffer))

    (wisi-parse-log-message parser cmd)
    (process-send-string process msg)

    ;; We don’t wait for the send to complete here.
    ))

(defun wisi-process-parse--send-refactor (parser refactor-action edit-begin)
  "Send a refactor command to PARSER external process, followed
by the content of the current buffer from PARSE-BEGIN thru
PARSE-END, wait for command to complete. PARSER will respond with
one or more Edit messages."
  ;; Must match "refactor" command arguments read by
  ;; emacs_wisi_common_parse.adb Get_Refactor_Params.
  (let* ((cmd (format "refactor %d \"%s\" %d \"%s\""
		      refactor-action
		      (if (buffer-file-name) (buffer-file-name) (buffer-name))
		      (position-bytes edit-begin)
		      wisi-parser-verbosity
		      ))
	 (msg (format "%04d%s" (length cmd) cmd))
	 (process (wisi-process--parser-process parser)))

    (with-current-buffer (wisi-process--parser-buffer parser)
      (erase-buffer))

    (wisi-parse-log-message parser msg)
    (process-send-string process msg)
    (wisi-process-parse--wait parser)
    ))

(defun wisi-process-parse--marker-or-nil (item)
  (if (= -1 item) nil (copy-marker item t)))

(defun wisi-process-parse--Navigate_Cache (parser sexp)
  ;; sexp is [Navigate_Cache pos statement_id id length class containing_pos prev_pos next_pos end_pos]
  ;; see ‘wisi-process-parse--execute’
  (let ((pos (aref sexp 1)))
    (with-silent-modifications
      (put-text-property
       pos
       (1+ pos)
       'wisi-cache
       (wisi-cache-create
	:nonterm    (aref (wisi-process--parser-token-table parser) (aref sexp 2))
	:token      (aref (wisi-process--parser-token-table parser) (aref sexp 3))
	:last       (aref sexp 4)
	:class      (aref wisi-class-list (aref sexp 5))
	:containing (wisi-process-parse--marker-or-nil (aref sexp 6))
	:prev       (wisi-process-parse--marker-or-nil (aref sexp 7))
	:next       (wisi-process-parse--marker-or-nil (aref sexp 8))
	:end        (wisi-process-parse--marker-or-nil (aref sexp 9))
	)))
    ))

(defun wisi-process-parse--Name_Property (parser sexp)
  ;; sexp is [Name_Property first-pos last-pos]
  ;; see ‘wisi-process-parse--execute’
  ;; implements wisi-name-action
  (with-silent-modifications
    (put-text-property (aref sexp 1) (1+ (aref sexp 2)) 'wisi-name t)))

(defun wisi-process-parse--Face_Property (parser sexp)
  ;; sexp is [Face_Property first-pos last-pos face-index]
  ;; see ‘wisi-process-parse--execute’
  ;; implements wisi--face-action-1
  (with-silent-modifications
    (add-text-properties
     (aref sexp 1)
     (1+ (aref sexp 2))
     (list 'font-lock-face (aref (wisi-process--parser-face-table parser) (aref sexp 3))
	   'fontified t)
     )))

(defun wisi-process-parse--Indent (parser sexp)
  ;; sexp is [Indent line-number indent]
  ;; see ‘wisi-process-parse--execute’
  (let ((pos (aref (wisi-process--parser-line-begin parser) (1- (aref sexp 1)))))
    (with-silent-modifications
      (when (< (point-min) pos)
	(put-text-property
	 (1- pos)
	 pos
	 'wisi-indent
	 (aref sexp 2)))
      )))

(defun wisi-process-parse--Lexer_Error (parser sexp)
  ;; sexp is [Lexer_Error char-position <message> <repair-char>]
  ;; see ‘wisi-process-parse--execute’
  (let ((pos (aref sexp 1))
	err)

    (goto-char pos);; for current-column

    (setq err
	  (make-wisi--lexer-error
	   :pos (copy-marker pos)
	   :message
	   (format "%s:%d:%d: %s"
		   (if (buffer-file-name) (file-name-nondirectory (buffer-file-name)) "")
		   ;; file-name can be nil during vc-resolve-conflict
		   (line-number-at-pos pos)
		   (current-column)
		   (aref sexp 2))
	   :inserted (when (= 4 (length sexp)) (aref sexp 3))))

    (push err (wisi-parser-lexer-errors parser))
    ))

(defun wisi-process-parse--Parser_Error (parser sexp)
  ;; sexp is [Parser_Error char-position <string>]
  ;; see ‘wisi-process-parse--execute’
  (let ((pos (aref sexp 1))
	err)

    (goto-char pos);; for current-column

    (setq err
	  (make-wisi--parse-error
	   :pos (copy-marker pos)
	   :message
	   (format "%s:%d:%d: %s"
		   (if (buffer-file-name) (file-name-nondirectory (buffer-file-name)) "")
		   ;; file-name can be nil during vc-resolve-conflict
		   (line-number-at-pos pos)
		   (1+ (current-column))
		   (aref sexp 2))))

    (push err (wisi-parser-parse-errors parser))
    ))

(defun wisi-process-parse--Check_Error (parser sexp)
  ;; sexp is [Check_Error code name-1-pos name-2-pos <string>]
  ;; see ‘wisi-process-parse--execute’
  (let* ((name-1-pos (aref sexp 2))
	 (column-at-pos (lambda (pos) (goto-char pos)(current-column)))
	 (name-2-pos (aref sexp 3))
	 (file-name (if (buffer-file-name) (file-name-nondirectory (buffer-file-name)) "")))
    ;; file-name can be nil during vc-resolve-conflict

    (when (= 0 name-1-pos)
      (setq name-1-pos name-2-pos)
      (setq name-2-pos 0))


    (push (make-wisi--parse-error
	   :pos (copy-marker name-1-pos)
	   :pos-2 (copy-marker name-2-pos)
	   :message
	   (format
	    (concat "%s:%d:%d: %s"
		    (when (> 0 name-2-pos) " %s:%d:%d"))
	    file-name (line-number-at-pos name-1-pos) (funcall column-at-pos name-1-pos)
	    (aref sexp 4)
	    (when (> 0 name-2-pos)
	      file-name (line-number-at-pos name-2-pos) (funcall column-at-pos name-2-pos))))
	  (wisi-parser-parse-errors parser))
    ))

(defun wisi-process-parse--find-err (pos errors)
  (let ((result))
    (dolist (err errors)
      (when (or (= pos (wisi--parse-error-pos err))
		(and (wisi--parse-error-pos-2 err) (= pos (wisi--parse-error-pos-2 err))))
	(setq result err)))
    result))

(defun wisi-process-parse--Recover (parser sexp)
  ;; sexp is [Recover [error-pos edit-pos [inserted] [deleted] deleted-region]...]
  ;; see ‘wisi-process-parse--execute’
  ;; convert to list of wisi--parse-error-repair, add to corresponding error
  (let ((token-table (wisi-process--parser-token-table parser)))

    (unless (= 1 (length sexp))
      (cl-do ((i 1 (1+ i))) ((= i (length sexp)))
	(let* ((error-pos (aref (aref sexp i) 0))
	       (edit-pos (aref (aref sexp i) 1))
	       (err (wisi-process-parse--find-err error-pos (wisi-parser-parse-errors parser))))
          (when err
	    (cl-nsubst
	     (push
	      (make-wisi--parse-error-repair
	       :pos (copy-marker edit-pos)
	       :inserted (mapcar (lambda (id) (aref token-table id)) (aref (aref sexp i) 2))
	       :deleted  (mapcar (lambda (id) (aref token-table id)) (aref (aref sexp i) 3))
	       :deleted-region (aref (aref sexp i) 4))
	      (wisi--parse-error-repair err)) ;; new
	     err ;; old
	     (wisi-parser-parse-errors parser) ;; tree
	     :test (lambda (old el) (= (wisi--parse-error-pos old) (wisi--parse-error-pos err)))))
	   )))
    ))

(defun wisi-process-parse--End (parser sexp)
  ;; sexp is [End pos]
  ;; see ‘wisi-process-parse--execute’
  (setf (wisi-process--parser-end-pos parser) (1+ (aref sexp 1))))

(defun wisi-process-parse--Edit (parser sexp)
  ;; sexp is [Edit begin end text]
  (save-excursion
    (delete-region (aref sexp 1) (1+ (aref sexp 2)))
    (goto-char (aref sexp 1))
    (insert (aref sexp 3))))

(defun wisi-process-parse--Language (parser sexp)
  ;; sexp is [Language language-action ...]
  (funcall (aref (wisi-process--parser-language-action-table parser) (aref sexp 1)) sexp))

(defun wisi-process-parse--execute (parser sexp)
  "Execute encoded SEXP sent from external process."
  ;; sexp is [action arg ...]; an encoded instruction that we need to execute
  ;;
  ;; Actions:
  ;;
  ;; [Navigate_Cache pos statement_id id length class containing_pos prev_pos next_pos end_pos]
  ;;    Set a wisi-cache text-property.
  ;;    *pos          : integer buffer position; -1 if nil (not set)
  ;;    *id           : integer index into parser-token-table
  ;;    length        : integer character count
  ;;    class         : integer index into wisi-class-list
  ;;
  ;; [Name_Property first-pos last-pos]
  ;;
  ;; [Face_Property first-pos last-pos face-index]
  ;;    Set a font-lock-face text-property
  ;;    face-index: integer index into parser-elisp-face-table
  ;;
  ;; [Indent line-number indent]
  ;;    Set an indent text property
  ;;
  ;; [Lexer_Error char-position <message> <repair-char>]
  ;;    The lexer detected an error at char-position.
  ;;
  ;;    If <repair-char> is not ASCII NUL, it was inserted immediately
  ;;    after char-position to fix the error.
  ;;
  ;; [Parser_Error char-position <message>]
  ;;    The parser detected a syntax error; save information for later
  ;;    reporting.
  ;;
  ;;    If error recovery is successful, there can be more than one
  ;;    error reported during a parse.
  ;;
  ;; [Check_Error code name-1-pos name-2-pos <string>]
  ;;    The parser detected an in-parse action error; save information
  ;;    for later reporting. Either of the name-*-pos may be 0,
  ;;    indicating a missing name.
  ;;
  ;;    If error recovery is successful, there can be more than one
  ;;    error reported during a parse.
  ;;
  ;; [Recover [error-pos edit-pos [inserted] [deleted] deleted-region]...]
  ;;    The parser finished a successful error recovery.
  ;;
  ;;    error-pos: Buffer position where error was detected
  ;;
  ;;    edit-pos: Buffer position of inserted/deleted tokens
  ;;
  ;;    inserted: Virtual tokens (terminal or non-terminal) inserted
  ;;    before edit-pos.
  ;;
  ;;    deleted: Tokens deleted after edit-pos.
  ;;
  ;;    deleted-region: source buffer char region containing deleted tokens
  ;;
  ;;    Args are token ids; index into parser-token-table. Save the
  ;;    information for later use by ’wisi-repair-error’.
  ;;
  ;; [Edit begin end text]
  ;;    Replace region BEGIN . END with TEXT; normally the result of a
  ;;    refactor command.
  ;;
  ;; [Language ...]
  ;;    Dispatch to a language-specific action, via
  ;;    `wisi-process--parser-language-action-table'.
  ;;
  ;;
  ;; Numeric action codes are given in the case expression below

  (cl-ecase (aref sexp 0)
    (1  (wisi-process-parse--Navigate_Cache parser sexp))
    (2  (wisi-process-parse--Face_Property parser sexp))
    (3  (wisi-process-parse--Indent parser sexp))
    (4  (wisi-process-parse--Lexer_Error parser sexp))
    (5  (wisi-process-parse--Parser_Error parser sexp))
    (6  (wisi-process-parse--Check_Error parser sexp))
    (7  (wisi-process-parse--Recover parser sexp))
    (8  (wisi-process-parse--End parser sexp))
    (9  (wisi-process-parse--Name_Property parser sexp))
    (10 (wisi-process-parse--Edit parser sexp))
    (11 (wisi-process-parse--Language parser sexp))
    ))

;;;;; main

(cl-defgeneric wisi-parse-reset ((parser wisi-process--parser))
  (setf (wisi-process--parser-busy parser) nil))

(cl-defmethod wisi-parse-kill ((parser wisi-process--parser))
  (when (process-live-p (wisi-process--parser-process parser))
    ;; We used to send a quit command first, to be nice. But there's
    ;; no timeout on that, so it would hang when the process
    ;; executable is not reading command input.

    ;; Don't let font-lock start a parse for face while waiting for
    ;; the process to die.
    (setf (wisi-process--parser-busy parser) t)
    (wisi-parse-log-message parser "kill process")
    (kill-process (wisi-process--parser-process parser)))
  (setf (wisi-process--parser-busy parser) nil))

(defvar wisi--lexer nil) ;; wisi-elisp-lexer.el
(declare-function wisi-elisp-lexer-reset "wisi-elisp-lexer")

(defun wisi-process-parse--prepare (parser)
  ;; font-lock can trigger a face parse via a timer delay while
  ;; navigate or indent parse is active, due to
  ;; ‘accept-process-output’ in w-p-p--handle-messages. Signaling an
  ;; error tells font-lock to try again later.
  (if (wisi-process--parser-busy parser)
      (progn
  	(setf (wisi-parser-parse-errors parser)
	      (list
	       (make-wisi--parse-error
		:pos 0
		:message (format "%s:%d:%d: parser busy (try ’wisi-kill-parser’)"
				 (if (buffer-file-name) (file-name-nondirectory (buffer-file-name)) "") 1 1))
	       ))
	(error "parse abandoned; parser busy")
	)

    ;; It is not possible for a background elisp function (ie
    ;; font-lock) to interrupt this code between checking and setting
    ;; parser-busy; background elisp can only run when we call
    ;; accept-process-output in w-p-p--handle-messages.
    (setf (wisi-process--parser-busy parser) t)

    ;; If the parser process has not started yet,
    ;; wisi-process-parse--require-process calls
    ;; wisi-process-parse--wait, which can let font-lock invoke the
    ;; parser again. Thus this call must be after we set
    ;; wisi-process--parser-busy t
    (wisi-process-parse--require-process parser)

    (setf (wisi-process--parser-total-wait-time parser) 0.0)
    (setf (wisi-parser-lexer-errors parser) nil)
    (setf (wisi-parser-parse-errors parser) nil)

    ;; We don't erase the parser-buffer here, because we call --send*
    ;; without --prepare in response to wisi-file_not_found.
    ))

(defun wisi-process-parse--handle-messages (parser)
  (let ((response-buffer (wisi-process--parser-buffer parser))
        (source-buffer (current-buffer))
	log-start)
    (condition-case-unless-debug err
	(let* ((process (wisi-process--parser-process parser))
	       (w32-pipe-read-delay 0) ;; fastest subprocess read
	       response
	       response-end
	       (response-count 0)
	       sexp-start
	       (need-more nil) ;; point-max if need more, to check for new input
	       (done nil)
	       start-wait-time)

	  (set-buffer response-buffer)
	  (setq log-start (point-min))

	  ;; User will set wisi-process-time-out in source-buffer, but
	  ;; we reference it from response-buffer.
	  (setq-local wisi-process-time-out (with-current-buffer source-buffer wisi-process-time-out))

	  (setq sexp-start (point-min))

	  ;; process responses until prompt received
	  (while (not done)

	    ;; process all complete responses currently in buffer
	    (while (and (not need-more)
			(not done))

	      (goto-char sexp-start)

	      (cond
	       ((eobp)
		(setq need-more (point-max)))

	       ((looking-at wisi-process-parse-prompt)
		(setq done t))

	       ((or (looking-at "\\[") ;; encoded action
		    (looking-at "(")) ;; error or other elisp expression to eval
		(condition-case nil
		    (setq response-end (scan-sexps (point) 1))
		  (error
		   ;; incomplete response
		   (setq need-more (point-max))
		   nil))

		(unless need-more
		  (setq response-count (1+ response-count))
		  (setq response (car (read-from-string (buffer-substring-no-properties (point) response-end))))

		  (goto-char response-end)
		  (forward-line 1)
		  (setq sexp-start (point))

		  (cond
		   ((listp response)
		    ;; non-syntax error of some sort
		    (cond
		     ((equal 'file_not_found (car response))
		      ;; Parser does not have full text for file; most
		      ;; likely because it crashed or was killed since
		      ;; we last did a full parse. Signal it; caller
		      ;; will do a full parse.
		      (wisi-parse-log-message parser (buffer-substring log-start (point)))
		      (set-buffer source-buffer)
		      (signal 'wisi-file_not_found nil))

		     ((equal '(parse_error) response)
		      ;; Parser detected a syntax error, and recovery failed, so signal it.
		      (if (wisi-parser-parse-errors parser)
			  (signal 'wisi-parse-error
				  (wisi--parse-error-message (car (wisi-parser-parse-errors parser))))

			;; Can have no errors when testing a new parser
			(push
			 (make-wisi--parse-error :pos 0 :message "parser failed with no message")
			 (wisi-parser-parse-errors parser))
			(signal 'wisi-parse-error "parser failed with no message")))

		     ((equal 'parse_error (car response))
		      ;; Parser detected some other error non-fatal error, so signal it.
		      (push
		       (make-wisi--parse-error :pos 0 :message (cadr response))
		       (wisi-parser-parse-errors parser))
		      (signal 'wisi-parse-error (cdr response)))

		     ((and (eq 'error (car response))
			   (string-prefix-p "bad command:" (cadr response)))
		      ;; Parser dropped bytes, is treating buffer
		      ;; content bytes as commands. Kill the process
		      ;; to kill the pipes; there is no other way to
		      ;; flush them.
		      (kill-process (wisi-process--parser-process parser))
		      (signal 'wisi-parse-error "parser lost sync; killed"))

		     (t
		      ;; Some other error
		      (condition-case-unless-debug err
			  (eval response)
			(error
			 (push (make-wisi--parse-error
				:pos (point)
				:message (cadr err))
			       (wisi-parser-parse-errors parser))
			 (signal (car err) (cdr err)))))
		     ))

		   ((arrayp response)
		    ;; encoded action
		    (set-buffer source-buffer) ;; for put-text-property in actions
		    (condition-case-unless-debug err
			(wisi-process-parse--execute parser response)

		      (wisi-parse-error
		       (push (make-wisi--parse-error
			      :pos (point)
			      :message (cadr err))
			     (wisi-parser-parse-errors parser))
		       (signal (car err) (cdr err)))

		      (error ;; ie from un-commented [C:\Windows\system32\KERNEL32.DLL], or bug in action code above.
		       (error "parser failed"))
		      ))
		   )

		  (set-buffer response-buffer)
		  ))

	       (t
		;; debug output
		(forward-line 1)
		(setq sexp-start (point)))
	       )
	      )

	    (unless done
	      ;; end of response buffer
	      (unless (process-live-p process)
		(wisi-parse-log-message parser "process died")
		(error "parser process died"))

	      (setq start-wait-time (float-time))

	      ;; If we specify no time-out here, we get messages about
	      ;; "blocking call with quit inhibited", when this is
	      ;; called by font-lock from the display engine.
	      ;;
	      ;; Specifying just-this-one t prevents C-q from
	      ;; interrupting this?
	      (accept-process-output
	       process
	       wisi-process-time-out
	       nil ;; milliseconds
	       nil)  ;; just-this-one

	      (setf (wisi-process--parser-total-wait-time parser)
		    (+ (wisi-process--parser-total-wait-time parser)
		       (- (float-time) start-wait-time)))

	      (when (and (= (point-max) need-more)
			 (> (wisi-process--parser-total-wait-time parser) wisi-process-time-out))
		(error (concat "wisi-process-parse timing out; increase `wisi-process-time-out'?"
			       " (or bad syntax in process output)")))

	      (setq need-more nil))
	    );; while not done

	  ;; got command prompt
	  (set-buffer response-buffer)
	  (wisi-parse-log-message parser (buffer-substring log-start (point)))

	  (setf (wisi-process--parser-response-count parser) response-count)
	  (setf (wisi-process--parser-busy parser) nil)
	  (set-buffer source-buffer)
	  )

      ;; These do _not_ catch 'wisi-file_not_found
      (wisi-parse-error
       (set-buffer response-buffer)
       (wisi-parse-log-message parser (buffer-substring log-start (point)))
       (setf (wisi-process--parser-busy parser) nil)
       (set-buffer source-buffer)
       (signal (car err) (cdr err)))

      (error
       (set-buffer response-buffer)
       (wisi-parse-log-message parser (buffer-substring log-start (point)))
       (setf (wisi-process--parser-busy parser) nil)
       (set-buffer source-buffer)
       (signal (car err) (cdr err)))
      )))

(cl-defmethod wisi-parse-current ((parser wisi-process--parser) parse-action begin send-end parse-end)
  (wisi-process-parse--prepare parser)
  (let ((total-line-count (1+ (count-lines (point-max) (point-min)))))
    (setf (wisi-process--parser-line-begin parser) (wisi--set-line-begin total-line-count))
    (wisi-process-parse--send-parse parser parse-action begin send-end parse-end)

    ;; We reset the elisp lexer, because post-parse actions may use it.
    (when wisi--lexer
      (wisi-elisp-lexer-reset total-line-count wisi--lexer))
    )
  (wisi-process-parse--handle-messages parser)
  (cons begin (wisi-process--parser-end-pos parser))
  )

(cl-defmethod wisi-parse-incremental ((parser wisi-process--parser) &optional full)
  (wisi-process-parse--prepare parser)
  (let ((total-line-count (1+ (count-lines (point-max) (point-min)))))
    (setf (wisi-process--parser-line-begin parser) (wisi--set-line-begin total-line-count))
    (wisi-process-parse--send-incremental-parse parser full)
    ;; We reset the elisp lexer, because post-parse actions may use it.
    (when wisi--lexer
      (wisi-elisp-lexer-reset total-line-count wisi--lexer))
    (condition-case-unless-debug _err
	(wisi-process-parse--handle-messages parser)
      ('wisi-file_not_found
       (message "parsing buffer ...")
       (wisi-process-parse--send-incremental-parse parser t)
       (message "parsing buffer ... done")
       (wisi-process-parse--handle-messages parser)
       ))
    ))

(cl-defmethod wisi-post-parse ((parser wisi-process--parser) parse-action begin end)
  (wisi-process-parse--prepare parser)
  (wisi-process-parse--send-action parser parse-action begin end)
  (condition-case-unless-debug _err
      (wisi-process-parse--handle-messages parser)
    ('wisi-file_not_found
     (message "parsing buffer ...")
     (wisi-process-parse--send-incremental-parse parser t)
     (message "parsing buffer ... done")
     (wisi-process-parse--send-action parser parse-action begin end)
     (wisi-process-parse--handle-messages parser)
     )))

(cl-defmethod wisi-refactor ((parser wisi-process--parser) refactor-action stmt-begin stmt-end edit-begin)
  (wisi-process-parse--prepare parser)
  (cond
   (wisi-incremental-parse-enable
    (when wisi--changes
      (wisi-process-parse--send-incremental-parse parser nil)))

   (t
    ;; IMPROVEME: stmt-begin, stmt-end not used if only incremental supported.
    (wisi-process-parse--send-parse parser 'navigate stmt-begin stmt-end stmt-end)
    (wisi-process-parse--handle-messages parser)
    (wisi-process-parse--prepare parser)
    ))

  (wisi-process-parse--send-refactor parser refactor-action edit-begin)
  (condition-case-unless-debug _err
      (wisi-process-parse--handle-messages parser)
    ('wisi-file_not_found
     ;; We must have wisi-incremental-parse-enable t, with no changes.
     (message "parsing buffer ...")
     (wisi-process-parse--send-incremental-parse parser t)
     (message "parsing buffer ... done"))
    (wisi-process-parse--send-refactor parser refactor-action edit-begin)
    (wisi-process-parse--handle-messages parser)
    ))

;;;;; debugging
(defun wisi-process-parse-soft-kill (parser)
  "Send 'quit' command to parser, for repeatable termination in unit tests."
  (let ((process (wisi-process--parser-process parser)))
    (wisi-parse-log-message parser "soft kill process")
    (process-send-string process "004quit")
    (while (process-live-p process)
      (accept-process-output process))))

(defun wisi-process-parse-save-text (parser save-file-name)
  (let* ((cmd
	  (format "save_text \"%s\" \"%s\""
		  (if (buffer-file-name) (buffer-file-name) (buffer-name))
		  save-file-name))
	 (msg (format "%04d%s" (length cmd) cmd))
	 (process (wisi-process--parser-process parser)))
    (with-current-buffer (wisi-process--parser-buffer parser)
      (erase-buffer))

    (wisi-parse-log-message parser cmd)
    (process-send-string process msg)
    (wisi-process-parse--handle-messages parser)))

(provide 'wisi-process-parse)
