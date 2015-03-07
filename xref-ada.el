;; xref-ada.el --- ada-mode backend for xref.el -*-lexical-binding:t-*-
;;
;; not loaded from ada-mode.el yet; xref not in released emacs.

(require 'xref)

(defun xref-ada-find (action arg)
  (cl-ecase action
    (definitions ;; arg is an identifer (see xref-ada-identifier-at-point)
      (xref-ada-find-definitions arg))
    (references  ;; arg is an identifer
     (error "FIXME: not implemented"))
    (apropos      ;; arg is a rexexp
     (error "FIXME: not implemented"))
    )
  )

(defun xref-ada-find-definitions (identifier)
  "For `xref-find-function' 'definitions case."
  (let* ((t-prop (get-text-property 0 'xref-ada-identifier identifier))
	 (identifier (substring-no-properties identifier 0 nil))
	 (file (plist-get t-prop ':file))
	 (line (plist-get t-prop ':line))
	 (column (plist-get t-prop ':column))
	 )

    (ada-check-current-project file)

    (when (null ada-xref-other-function)
      (error "no cross reference information available"))

    (let ((target
	   (funcall
	    ada-xref-other-function
	    identifier file line column)))
      ;; FIXME: ada-xref-other-function return xref-file-location
      (list
       (xref-make
	identifier
	(xref-make-file-location
	 (nth 0 target) ;; file
	 (nth 1 target) ;; line
	 (nth 2 target)) ;; column
	)))
    ))

(defun xref-ada-identifier-at-point ()
  (save-excursion
    (condition-case nil
	(let ((ident (ada-identifier-at-point))) ;; moves point to start of ident
	  (put-text-property
	   0 1
	   'xref-ada-identifier
	   (list ':file (buffer-file-name)
		 ':line (line-number-at-pos)
		 ':column (1+ (current-column)))
	   ident)
	  ident)
	(error
	 ;; from ada-identifier-at-point; no identifier
	 nil))))

;; (defun xref-ada-identifer-completion-table (identifer)
;;   "For `xref-identifier-completion-table-function'."
;;   ;; FIXME: implement gpr or asis backend
;;    nil)

(defun xref-ada-setup ()
  (setq-local xref-find-function #'xref-ada-find)
  (setq-local xref-identifier-at-point-function #'xref-ada-identifier-at-point)
  ;; (setq-local xref-identifier-completion-table-function
  ;;             #'xref--ada-identifier-completion-table)
  ;; (add-hook 'completion-at-point-functions
  ;;           #'xref-ada-completion-at-point nil 'local)
  )

;; FIXME: add gpr-query backend for C++, C?

(provide 'xref-ada)
