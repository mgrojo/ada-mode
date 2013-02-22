;; Emacs settings and extentions for OpenToken

(gnat-7.1)
(require 'ada-smie-opentoken)

;; extensions to ada-fix-error

(defun opentoken-gnat-fix-error (msg source-buffer source-window)
  "For `ada-gnat-fix-error-hook'."

  (let ((start-pos (point))
	result)
    ;; Move to start of error message text
    (skip-syntax-forward "^-")
    (forward-char 1)

    ;; recognize it, handle it
    (setq
     result
     (unwind-protect
	 (cond
	  ;; OpenToken access type naming convention
	  ((looking-at (concat "expected \\(private \\)?type " ada-gnat-quoted-name-regexp))
	   (let ((type (match-string 2)))
	     (next-line 1)
	     (when (looking-at "found type .*_Ptr")
	       ;; assume just need '.all'
	       (progn
		 (pop-to-buffer source-buffer)
		 (forward-word 1)
		 (insert ".all")
		 t)
	       )))
	  ;; Ada hidden in wisi packages
	  ((looking-at "package \"Ada\" is hidden by declaration")
	   (pop-to-buffer source-buffer)
	   (backward-word 1)
	   (insert "Standard.")
	   t)

	  )));; end of setq unwind-protect cond
    (if result
	t
      (goto-char start-pos)
      nil)
    ))

(add-hook 'ada-gnat-fix-error-hook 'opentoken-gnat-fix-error)

;; end of file
