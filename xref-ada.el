;; xref-ada.el --- ada-mode backend for xref.el -*-lexical-binding:t-*-
;;
;; requires emacs 25

(eval-and-compile
  (when (> emacs-major-version 24)
    (require 'xref)))

(defun xref-ada-find-backend ()
  'xref-ada)

(cl-defmethod xref-backend-definitions ((_backend (eql xref-ada)) identifier)
  (let* ((t-prop (get-text-property 0 'xref-ada-identifier identifier))
	 (identifier (substring-no-properties identifier 0 nil))
	 (file (plist-get t-prop ':file))
	 (line (plist-get t-prop ':line))
	 (column (plist-get t-prop ':column))
	 )

    ;; t-prop is nil when identifier is from prompt

    (unless file
      ;; WORKARUND: gpr-query-other requires a non-nil file arg.
      ;; Should prompt for file with identifier, or improve gpr-query to handle nil file.
      ;; For now, use current buffer file; will search spec and body (a common use case).
      (setq file (buffer-file-name)))

    (ada-check-current-project file)

    (when (null ada-xref-other-function)
      (error "no cross reference information available"))

    (let ((target
	   (funcall
	    ada-xref-other-function
	    identifier file line column)))
      ;; IMPROVEME: change ada-xref-other-function to return xref-file-location
      (list
       (xref-make
	identifier
	(xref-make-file-location
	 (nth 0 target) ;; file
	 (nth 1 target) ;; line
	 (nth 2 target)) ;; column
	)))
    ))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql xref-ada)))
  (save-excursion
    (condition-case nil
	(let ((ident (ada-identifier-at-point))) ;; moves point to start of ident
	  (put-text-property
	   0 1
	   'xref-ada-identifier
	   (list ':file (buffer-file-name)
		 ':line (line-number-at-pos)
		 ':column (current-column))
	   ident)
	  ident)
	(error
	 ;; from ada-identifier-at-point; no identifier
	 nil))))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql xref-ada)))
  (wisi-validate-cache (point-max) t)
  (save-excursion
    (let ((table nil)
	  cache)
      (goto-char (point-min))
      (while (not (eobp))
	(setq cache (wisi-forward-find-cache-token '(IDENTIFIER name )(point-max)))
	(cond
	 ((null cache)
	  ;; eob
	  )

	 ((memq (wisi-cache-nonterm cache)
		'(abstract_subprogram_declaration
		  exception_declaration
		  function_specification
		  full_type_declaration
		  generic_declaration
		  generic_instantiation
		  null_procedure_declaration
		  object_declaration
		  procedure_specification
		  package_specification
		  subtype_declaration
		  type_declaration))
	  (push (wisi-cache-text cache) table))
	 ))
      table)))

(define-minor-mode xref-ada-mode ()
  "Use xref-ada functions."
  :init-value t
  ;; The macro code sets the mode variable to the new value before we get here.
  (if xref-ada-mode
      (add-hook 'xref-backend-functions #'xref-ada-find-backend nil t)

    (setq xref-backend-functions (remq #'xref-ada-find-backend xref-backend-functions))))

(add-hook 'ada-mode-hook 'xref-ada-mode)

(provide 'xref-ada)
