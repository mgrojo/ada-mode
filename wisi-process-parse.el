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
  (total-wait-time 0.0)   ;; total time during last parse spent waiting for subprocess output.
  (response-count 0)      ;; responses received from subprocess during last parse; for profiling.
  end-pos                 ;; last character position parsed
  language-action-table   ;; array of function pointers, each taking an sexp sent by the process
  query-result            ;; holds result of wisi-process-parse--Query
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
	      (get-buffer-create process-name))
	(with-current-buffer (wisi-process--parser-buffer parser)
	  (emacs-lisp-mode))) ;; for comment syntax

      (with-current-buffer (wisi-process--parser-buffer parser)
	(erase-buffer));; delete any previous messages, prompt

      (wisi-parse-log-message parser "create process")

      (setf (wisi-process--parser-process parser)
	    (make-process
	     :name process-name
	     :buffer (wisi-process--parser-buffer parser)

	     :coding 'utf-8-unix
	     ;; We don't need utf-8-dos for reading when the parser is
	     ;; compiled for Windows; ASCII.CR is easy to ignore.
	     ;;
	     ;; See test/mixed_unix_dos_line_ends.adb; we'd like to
	     ;; have truly "no conversion" here, so the count of sent
	     ;; bytes to be the same as computed in *--send. But even
	     ;; utf-8-emacs strips the stray ^M from that buffer.

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

(defun wisi-process-parse--add-cmd-length (cmd)
  "Return CMD (a string) with length prefixed."
  ;; Characters in cmd length must match emacs_wisi_common_parse.adb
  ;; Get_Command_Length. If the actual length overflows the alloted
  ;; space, we will get a protocol_error from the parser
  ;; eventually. Caller should prevent that and send an alternate
  ;; command.
  (format "%04d%s" (string-bytes cmd) cmd))

(defun wisi-process-parse--send-parse (parser parse-action begin send-end parse-end)
  "Send a partial PARSE-ACTION command to PARSER external process.
The command is followed by the content of the current buffer from
BEGIN thru SEND-END.  Does not wait for command to
complete. PARSE-END is end of desired parse region."
  ;; Must match "full/partial parse" command arguments read by
  ;; emacs_wisi_common_parse.adb Get_Parse_Params.
  ;; Parse_Kind is always Partial here; that really means "legacy".
  (let* ((cmd (format "parse 0 %d \"%s\" %d %d %d %d %d %d %d %d \"%s\" %d %d %d %d %d \"%s\""
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
	 (process (wisi-process--parser-process parser)))

    (with-current-buffer (wisi-process--parser-buffer parser)
      (erase-buffer))

    (wisi-parse-log-message parser cmd)
    (process-send-string process (wisi-process-parse--add-cmd-length cmd))

    ;; we don't log the buffer text; may be huge
    (process-send-string process (buffer-substring-no-properties begin send-end))

    ;; We don’t wait for the send to complete here.
    ))

(defun wisi-process-parse--send-incremental-parse (parser full)
  "Send an incremental parse command to PARSER external process.
If FULL, do initial full parse.  Does not wait for command to
complete."
  ;; First check length of changes list. For example, in almost any
  ;; buffer, changing the indent of every line (done in unit tests)
  ;; builds a change list that is longer than the buffer text, and no
  ;; faster to parse. So for long change lists, fall back to full
  ;; parse.
  ;;
  ;; For now, we define "long" by the command length character count
  ;; limit set by `wisi-process-parse--add-cmd-length'. For 4
  ;; characters, this is hit by
  ;; test/ada_mode-conditional_expressions.adb.
  (let ((changes
	 ;; wisi--changes is in reverse time order.
	 (prin1-to-string (nreverse wisi--changes))))
    (when (> (length changes) 9999)
      (setq full t))

    ;; Must match "incremental parse" command arguments read by
    ;; emacs_wisi_common_parse.adb Get_Parse_Params.
    (let* ((cmd
	    (apply #'format
		   (concat
		    "parse %d \"%s\" \"%s\" %d %d %d %d %d "
		    (if full "%d" "%s")
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
			 )
		      (list changes))
		    (list (wisi-parse-format-language-options parser))
		    )))
	   (process (wisi-process--parser-process parser)))

      (with-current-buffer (wisi-process--parser-buffer parser)
	(erase-buffer))

      (wisi-parse-log-message parser cmd)
      (process-send-string process (wisi-process-parse--add-cmd-length cmd))
      (setq wisi--changes nil)
      (when full
	(process-send-string process (buffer-substring-no-properties (point-min) (point-max))))

      ;; We don’t wait for the send to complete here.
      )))

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
		      ;; indent-region passes markers
		      (if (markerp begin) (marker-position begin) begin)
		      (position-bytes (min (point-max) end))
		      (min (point-max) (if (markerp end) (marker-position end) end))
		      (wisi-parse-format-language-options parser)
		      ))
	 (process (wisi-process--parser-process parser)))

    (with-current-buffer (wisi-process--parser-buffer parser)
      (erase-buffer))

    (wisi-parse-log-message parser cmd)
    (process-send-string process (wisi-process-parse--add-cmd-length cmd))

    ;; We don’t wait for the send to complete here.
    ))

(defun wisi-process-parse--send-refactor (parser refactor-action edit-begin)
  "Send a refactor command to PARSER external process, wait for
command to complete. PARSER will respond with one or more Edit
messages."
  ;; Must match "refactor" command arguments read by
  ;; emacs_wisi_common_parse.adb Get_Refactor_Params.
  (let* ((cmd (format "refactor \"%s\" %d %d \"%s\""
		      (if (buffer-file-name) (buffer-file-name) (buffer-name))
		      refactor-action
		      (position-bytes edit-begin)
		      wisi-parser-verbosity
		      ))
	 (process (wisi-process--parser-process parser)))

    (with-current-buffer (wisi-process--parser-buffer parser)
      (erase-buffer))

    (wisi-parse-log-message parser cmd)
    (process-send-string process (wisi-process-parse--add-cmd-length cmd))
    (wisi-process-parse--wait parser)
    ))

(defun wisi-process-parse--send-query (parser query point)
  "Send a query command to PARSER external process, wait for command to complete. PARSER will respond with
one or more Query messages."
  ;; Must match "query-tree" command arguments read by
  ;; emacs_wisi_common_parse.adb Process_Stream "query-tree"
  (let* ((cmd (format "query-tree \"%s\" %d %d"
		      (if (buffer-file-name) (buffer-file-name) (buffer-name))
		      (cdr (assoc query wisi-parse-tree-queries))
		      point
		      ))
	 (process (wisi-process--parser-process parser)))

    (with-current-buffer (wisi-process--parser-buffer parser)
      (erase-buffer))

    (wisi-parse-log-message parser cmd)
    (process-send-string process (wisi-process-parse--add-cmd-length cmd))
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
       (min (1+ pos) (point-max))
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
  ;; sexp is [Indent line-number line-begin-char-pos indent]
  ;; see ‘wisi-process-parse--execute’
  (let ((pos (aref sexp 2)))
    (with-silent-modifications
      (when (< (point-min) pos)
	(put-text-property
	 (1- pos)
	 pos
	 'wisi-indent
	 (aref sexp 3)))
      )))

(defun wisi-process-parse--Lexer_Error (parser sexp)
  ;; sexp is [Lexer_Error char-position <message> <repair-char>]
  ;; see ‘wisi-process-parse--execute’
  (let ((pos (min (point-max) (aref sexp 1)))
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
	 (name-2-pos (aref sexp 3))
	 (column-at-pos (lambda (pos) (goto-char pos)(current-column)))
	 (file-name (if (buffer-file-name) (file-name-nondirectory (buffer-file-name)) "")))
    ;; file-name can be nil during vc-resolve-conflict

    (when (not name-1-pos)
      (setq name-1-pos name-2-pos)
      (setq name-2-pos 0))

    (when (not name-2-pos)
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

(defun wisi-process-parse--Query (parser sexp)
  ;; sexp is [Query query-label ...] see `wisi-parse-tree-queries
  (cl-ecase (car (rassoc (aref sexp 1) wisi-parse-tree-queries))
    (bounds
     (setf (wisi-process--parser-query-result parser)
	   (list (aref sexp 2)(aref sexp 3)(aref sexp 4)(aref sexp 5))))

    (containing-statement
     (setf (wisi-process--parser-query-result parser)
	   (when (aref sexp 2)
	     (list (aref sexp 2) (cons (aref sexp 3) (aref sexp 4))))))

    (nonterm
     (setf (wisi-process--parser-query-result parser)
	   (when (aref sexp 2)
	     (aref (wisi-process--parser-token-table parser) (aref sexp 2)))))

    (virtuals
     ;; sexp is [Query query-label [...]]
     ;; IMPROVEME: convert to elisp token_ids. for now we only need the count in unit tests.
     (setf (wisi-process--parser-query-result parser)
	     (aref sexp 2)))

    (print
     (setf (wisi-process--parser-query-result parser)
	   t))
    ))

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
  ;; [Query query-label ...]
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
    (12 (wisi-process-parse--Query parser sexp))
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
		:pos (point-min)
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

    ;; We con't clear errors here; only clear before parse, not post-parse.

    ;; We don't erase the parser-buffer here, because we call --send*
    ;; without --prepare in response to wisi-file_not_found.
    ))

(defun wisi-process-parse--handle-messages (parser)
  ;; signals 'wisi-file_not_found if parser reports (file-not-found)
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
	       (need-more nil) ;; t need more input to complete current sexp
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
		(setq need-more t))

	       ((looking-at wisi-process-parse-prompt)
		(setq done t))

	       ((wisi-in-comment-p)
		;; In debug output. Just move to beginning of comment;
		;; sexp loop will handle moving forward.
		;;
		;; (must be after prompt check; the prompt is a comment)
		(goto-char (line-beginning-position))
		(setq sexp-start (point)))

	       ((or (looking-at "\\[") ;; encoded action
		    (looking-at "(")) ;; error or other elisp expression to eval
		(condition-case nil
		    (setq response-end (scan-sexps (point) 1))
		  (error
		   ;; incomplete response
		   (setq need-more t)
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
		       (make-wisi--parse-error :pos (point-min) :message (cadr response))
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

		      (error ;; ie from un-commented [C:\Windows\system32\KERNEL32.DLL], or bug in action code above.
		       (error "elisp processing of post-parse message failed"))
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

	      (when (and need-more
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

(defun wisi-process-parse--handle-messages-file-not-found (parser action)
  (funcall action)
  (condition-case-unless-debug _err
      (wisi-process-parse--handle-messages parser)
    ('wisi-file_not_found
     (message "parsing buffer ...")
     (wisi-process-parse--send-incremental-parse parser t)
     (message "parsing buffer ... done")
     (funcall action)
     (wisi-process-parse--handle-messages parser)
     )))

(cl-defmethod wisi-parse-current ((parser wisi-process--parser) parse-action begin send-end parse-end)
  (wisi-process-parse--prepare parser)
  (setf (wisi-parser-lexer-errors parser) nil)
  (setf (wisi-parser-parse-errors parser) nil)
  (wisi-process-parse--send-parse parser parse-action begin send-end parse-end)
  (wisi-process-parse--handle-messages parser)
  (cons begin (wisi-process--parser-end-pos parser))
  )

(cl-defmethod wisi-parse-incremental ((parser wisi-process--parser) &optional full)
  (wisi-process-parse--prepare parser)
  (setf (wisi-parser-lexer-errors parser) nil)
  (setf (wisi-parser-parse-errors parser) nil)
  (wisi-process-parse--send-incremental-parse parser full)
  (condition-case-unless-debug _err
      (wisi-process-parse--handle-messages parser)
    ('wisi-file_not_found
     (message "parsing buffer ...")
     (wisi-process-parse--send-incremental-parse parser t)
     (message "parsing buffer ... done")
     (wisi-process-parse--handle-messages parser)
     )))

(cl-defmethod wisi-post-parse ((parser wisi-process--parser) parse-action begin end)
  (wisi-process-parse--prepare parser)
  (wisi-process-parse--handle-messages-file-not-found
   parser
   (lambda () (wisi-process-parse--send-action parser parse-action begin end)))
)

(cl-defmethod wisi-refactor ((parser wisi-process--parser) refactor-action stmt-begin stmt-end edit-begin)
  (cond
   (wisi-incremental-parse-enable
    (when wisi--changes
      (wisi-parse-incremental parser nil)))

   (t
    ;; IMPROVEME: stmt-begin, stmt-end not used if only incremental supported.
    (wisi-process-parse--prepare parser)
    (wisi-process-parse--send-parse parser 'navigate stmt-begin stmt-end stmt-end)
    (wisi-process-parse--handle-messages parser)
    ))

  (wisi-process-parse--prepare parser)
  (wisi-process-parse--handle-messages-file-not-found
   parser
   (lambda () (wisi-process-parse--send-refactor parser refactor-action edit-begin))))

(cl-defmethod wisi-parse-tree-query ((parser wisi-process--parser) query point)
  (cl-assert wisi-incremental-parse-enable)
  (when wisi--changes
    (wisi-parse-incremental parser nil))

  (wisi-process-parse--prepare parser)
  (setf (wisi-process--parser-query-result parser) nil)
  (wisi-process-parse--handle-messages-file-not-found
   parser
   (lambda () (wisi-process-parse--send-query parser query point)))
  (wisi-process--parser-query-result parser))

;;;;; debugging
(defun wisi-process-parse-soft-kill (parser)
  "Send 'quit' command to parser, for repeatable termination in unit tests."
  (let ((process (wisi-process--parser-process parser)))
    (wisi-parse-log-message parser "soft kill process")
    (process-send-string process (wisi-process-parse--add-cmd-length "quit"))
    (while (process-live-p process)
      (accept-process-output process))))

(defun wisi-process-parse-save-text (parser save-file-name auto)
  (let* ((cmd
	  (format (concat (if auto "save_text_auto" "save_text")
			    " \"%s\" \"%s\"")
		  (if (buffer-file-name) (buffer-file-name) (buffer-name))
		  save-file-name))
	 (process (wisi-process--parser-process parser)))
    (with-current-buffer (wisi-process--parser-buffer parser)
      (erase-buffer))

    (wisi-parse-log-message parser cmd)
    (process-send-string process (wisi-process-parse--add-cmd-length cmd))
    (wisi-process-parse--handle-messages parser)))

(defun wisi-process-log-to-cmd (&optional cmd-buffer-name)
  "Convert parser log in current buffer to command file in CMD-BUFFER-NAME."
  (interactive)
  (unless cmd-buffer-name
    (setq cmd-buffer-name "debug.cmd"))
  (let ((log-buffer (current-buffer))
	(log-buffer-point (point))
	(cmd-buffer (get-buffer-create cmd-buffer-name))
	edit begin end source-file)
    (set-buffer cmd-buffer)
    (erase-buffer)

    (set-buffer log-buffer)
    (goto-char (point-min))

    ;; get options from full parse line; we assume they don't change
    (unless (search-forward-regexp "parse \\([02]\\)" nil t)
      (user-error "log buffer overflowed; increase wisi-parser-transaction-log-buffer-size"))

    (cl-ecase (string-to-number (match-string 1))
      (0 ;; partial parse
       (user-error "can't create command file for partial parse"))

      (2 ;; full parse
       (looking-at " \"\\([^\"]*\\)\" \"\\([^\"]*\\)\" \\([-0-9]+\\) \\([-0-9]+\\) \\([-0-9]+\\) \\([-0-9]+\\) [-0-9]+ [-0-9]+ \"\\([^\"]*\\)\"")
       (let ((verbosity (match-string 2))
	     (mckenzie_task_count (match-string 3))
	     (mckenzie_zombie_limit (match-string 4))
	     (mckenzie_enqueue_limit (match-string 5))
	     (parse_max_parallel (match-string 6))
	     (language_param (match-string 7)))
	 (setq source-file (match-string 1))

	 (set-buffer cmd-buffer)
	 (setq-local comment-start "-- ")

	 (insert "-- -*- comment-start: \"" comment-start "\" -*-" "\n")

	 (insert "file " source-file "\n")

	 (insert "verbosity " verbosity "\n")

	 (when (or (not (string-equal mckenzie_task_count "-1"))
		   (not (string-equal mckenzie_zombie_limit "-1"))
		   (not (string-equal mckenzie_enqueue_limit "-1")))
	   (insert "mckenzie_options ")

	   (when (not (string-equal mckenzie_task_count "-1"))
	     (insert "task_count=" mckenzie_task_count " "))

	   (when (not (string-equal mckenzie_zombie_limit "-1"))
	     (insert "zombie_limit=" mckenzie_zombie_limit " "))

	   (when (not (string-equal mckenzie_enqueue_limit "-1"))
	     (insert "enqueue_limit=" mckenzie_enqueue_limit))

	   ;; parse-max-parallel is not available
	   (insert "\n"))

	 (insert "language_params " language_param "\n\n")

	 (insert "parse_full " source-file "\n\n"))))

    (set-buffer log-buffer)
    (goto-char (point-min))
    (while (search-forward-regexp "^\\(parse 1\\|post-parse\\|query-tree\\|refactor\\) \"\\([^\"]+\\)\"" nil t)
      (cond
       ((string-equal (match-string 1) "parse 1")
	(search-forward-regexp "((")
	(setq begin (match-beginning 0))
	(search-forward-regexp "\")) ") ;; avoid matching 'is (Float (A));'
	(setq edit (buffer-substring begin (match-end 0)))

	(set-buffer cmd-buffer)
	(goto-char (point-max))
	(insert "parse_incremental ")
	(setq begin (point))
	(insert edit)
	(setq end (copy-marker (point) t))
	(goto-char begin)
	(while (search-forward "\n" end t)
	  (delete-char -1)
	  (insert "\\n"))
	(goto-char (point-max))
	(insert "\n\n")

	(set-buffer log-buffer))

       ((and (string-equal (match-string 1) "post-parse")
	     (string-equal (match-string 2) source-file))
	(goto-char (match-end 0))
	(looking-at " \"[^\"]*\" \\([0-9]+\\) \\([0-9]+ [0-9]+ [0-9]+ [0-9]+\\)")
	(let ((action (string-to-number (match-string 1)))
	      (args (match-string 2)))
	  (set-buffer cmd-buffer)
	  (goto-char (point-max))
	  (insert "--  post_parse "
		  (cl-ecase action
		    (0 "navigate ")
		    (1 "face ")
		    (2 "indent "))
		  args "\n\n")
	  (set-buffer log-buffer)))

       ((and (string-equal (match-string 1) "query-tree")
	     (string-equal (match-string 2) source-file))
	(goto-char (match-end 0))
	(looking-at " \\([0-9]+\\) \\([0-9]+\\)")
	(let ((label (string-to-number (match-string 1)))
	      (pos (match-string 2)))
	  (set-buffer cmd-buffer)
	  (goto-char (point-max))
	  (insert "--  query_tree "
		  (cl-ecase label
		    (0 "bounds ")
		    (1 "containing_statement ")
		    (2 "nonterm ")
		    (3 "virtuals ")
		    (4 "print "))
		  pos "\n\n")
	  (set-buffer log-buffer)))

       ((and (string-equal (match-string 1) "refactor")
	     (string-equal (match-string 2) source-file))
	(goto-char (match-end 0))
	(looking-at " \\([0-9]+\\) \\([0-9]+\\)")
	(let ((label (string-to-number (match-string 1)))
	      (pos (match-string 2)))
	  (set-buffer cmd-buffer)
	  (goto-char (point-max))
	  (insert "--  refactor " ;; must match wisi-ada.ads Refactor_Label
		  (cl-ecase label
		    (0 "Method_Object_To_Object_Method ")
		    (1 "Object_Method_To_Method_Object ")
		    (2 "Element_Object_To_Object_Index ")
		    (3 "Object_Index_To_Element_Object ")
		    (4 "Format_Parameter_List "))
		  pos "\n\n")
	  (set-buffer log-buffer)))

       (t
	;; other command or other file - ignore
	))

      )
    (with-current-buffer cmd-buffer
      (when (buffer-file-name)
	(save-buffer)))
    (goto-char log-buffer-point)))

(defun wisi-process-log-to-cmd-1 ()
  "Convert parse_partial process command at point to run_* command line."
  (interactive)
  (forward-line 0)
  (unless (looking-at "parse 0 \\([-0-9]+\\) \"\\([^\"]*\\)\" \\([-0-9]+\\) \\([-0-9]+\\) \\([-0-9]+\\) \\([-0-9]+\\) \\([-0-9]+\\) \\([-0-9]+\\) \\([-0-9]+\\) \\([-0-9]+\\) \"\\([^\"]*\\)\" \\([-0-9]+\\) \\([-0-9]+\\) \\([-0-9]+\\) \\([-0-9]+\\) \\([-0-9]+\\) \"\\([^\"]*\\)\"")
    (user-error "not on partial_parse command"))

  (let ((parse-action (string-to-number (match-string 1)))
	(source-file (match-string 2))
	(begin-byte-pos (match-string 3))
	(end-byte-pos (match-string 4))
	(goal-byte-pos (match-string 5))
	(begin-char-pos (match-string 6))
	(end-char-pos (match-string 7))
	(begin-line (match-string 8))
	(begin-indent (match-string 9))
	(_partial-parse-active (match-string 10))
	(verbosity (match-string 11))
	(mckenzie_task_count (string-to-number (match-string 12)))
	(mckenzie_zombie_limit (string-to-number (match-string 13)))
	(mckenzie_enqueue_limit (string-to-number (match-string 14)))
	(parse_max_parallel (string-to-number (match-string 15)))
	(_byte-count (match-string 16))
	(language_param (match-string 17))
	cmd)

    (setq cmd
	  (concat "parse_partial "
		  (cl-ecase parse-action
		    (0 "navigate ")
		    (1 "face ")
		    (2 "indent "))
		  source-file " "
		  begin-byte-pos " "
		  end-byte-pos " "
		  goal-byte-pos " "
		  begin-char-pos " "
		  end-char-pos " "
		  begin-line " "
		  begin-indent " "
		  (when (not (string-equal "" verbosity)) (format "--verbosity \"%s\" " verbosity))
		  (when (not (= -1 mckenzie_task_count)) (format "--mckenzie_task_count %d " mckenzie_task_count))
		  (when (not (= -1 mckenzie_zombie_limit)) (format "--mckenzie_zombie_limit %d" mckenzie_zombie_limit))
		  " "
		  (when (not (= -1 mckenzie_enqueue_limit))
		    (format "--mckenzie_enqueue_limit %d" mckenzie_enqueue_limit))
		  " "
		  (when (not (= -1 parse_max_parallel)) (format "--parse_max_parallel %d" parse_max_parallel)) " "
		  (when (not (string-equal "" language_param)) (format "--lang_params \"%s\" " language_param))
		  ))
    (message cmd)))

(defun wisi-time (func count &optional report-wait-time)
  "call FUNC COUNT times, show total time"
  (interactive "afunction \nncount ")

  (let ((start-time (float-time))
	(start-gcs gcs-done)
	(cum-wait-time 0.0)
        (i 0)
        diff-time
	diff-gcs)
    (while (not (eq (1+ count) (setq i (1+ i))))
      (save-excursion
        (funcall func))
      (when report-wait-time
	(setq cum-wait-time (+ cum-wait-time (wisi-process--parser-total-wait-time wisi--parser)))))
    (setq diff-time (- (float-time) start-time))
    (setq diff-gcs (- gcs-done start-gcs))
    (if report-wait-time
	(progn
	  (message "Total %f seconds, %d gcs; per iteration %f seconds %d gcs %d responses %f wait"
		   diff-time
		   diff-gcs
		   (/ diff-time count)
		   (/ (float diff-gcs) count)
		   (wisi-process--parser-response-count wisi--parser)
		   (/ cum-wait-time count)))

      (message "Total %f seconds, %d gcs; per iteration %f seconds %d gcs"
	       diff-time
	       diff-gcs
	       (/ diff-time count)
	       (/ (float diff-gcs) count))
      ))
  nil)

(defun wisi-time-indent-middle-line-cold-cache (count &optional report-wait-time)
  (goto-char (point-min))
  (forward-line (1- (/ (count-lines (point-min) (point-max)) 2)))
  (let ((cum-wait-time 0.0))
    (wisi-time
     (lambda ()
       (wisi-set-parse-try t 'indent)
       (wisi-invalidate-cache 'indent (point-min))
       (wisi-indent-line)
       (when (wisi-process--parser-p wisi--parser)
	 (setq cum-wait-time (+ cum-wait-time (wisi-process--parser-total-wait-time wisi--parser)))))
     count
     report-wait-time)
    ))

(defun wisi-time-indent-middle-line-warm-cache (count)
  (wisi-set-parse-try t 'indent)
  (wisi-invalidate-cache 'indent (point-min))
  (goto-char (point-min))
  (forward-line (/ (count-lines (point-min) (point-max)) 2))
  (wisi-indent-line)
  (wisi-time #'wisi-indent-line count))

(provide 'wisi-process-parse)
