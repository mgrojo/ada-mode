;; Project definitions

(require 'ada-project)
(require 'xref-ada)

(add-to-list 'project-find-functions 'project-menu-prj)

(let* ((prj-file "wisitoken.prj")
       (prj-name "wisitoken main")
       (prj (make-ada-project
	     :env-vars nil
	     :ada-prj prj-file)))

  (project-menu-add-project prj prj-name default-directory)

  (project-menu-select-by-name prj-name)
  )

;; extensions to ada-fix-error

(defun wisitoken-gnat-fix-error (msg source-buffer source-window)
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
	  ;; Wisitoken access type naming convention
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

(add-hook 'ada-gnat-fix-error-hook 'wisitoken-gnat-fix-error)

;; end of file
