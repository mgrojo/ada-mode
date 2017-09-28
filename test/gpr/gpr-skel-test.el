;;; Test indentation of all gpr skeletons
;;
;; We expand each entry in gpr-skel-token-alist, indent the buffer,
;; and save it; the makefile then diffs it with a known good result.

;; useful while testing indentation for new skeletons:
;; in gpr buffer: (setq skeleton-end-hook nil)
;; (setq wisi-debug 1)

(package-initialize) ;; for queue

(require 'gpr-mode)

(defun skeleton-expand-all (skel-token-alist exclude)
  "Expand all skeletons in SKEL-TOKEN-ALIST at point in the current buffer.
Preserves text after point."
  (let (token-skel
	(post-marker (save-excursion (newline) (copy-marker (point))))
	)
    ;; expand all skeletons before post-marker
    (while (setq token-skel (pop skel-token-alist))
      (when (not (member (car token-skel) exclude))
	(if (functionp (cdr token-skel))
	    (progn
	      ;; Provide a "str" arg in case it is needed
	      (funcall (cdr token-skel) "foo")
	      (goto-char (1- post-marker))
	      (newline))
	  (skeleton-expand-all (cdr token-skel) exclude)))
      )))

(defun gpr-skel-test ()
  (let ((filename "gpr-skel.gpr.tmp")
	(user-full-name "gpr-skel") ;; reproducible)
	(wisi-debug 1);; report parse errors
	)
    (with-temp-buffer
      filename
      (gpr-mode)

      ;; `skeleton-insert' may recenter, which fails if the buffer is
      ;; not mapped to the selected window.
      (pop-to-buffer (current-buffer))

      ;; gpr-skel-initial-string inserts 'header project' tokens;
      ;; expand those, then expand all other tokens within the project
      ;; declaration, so they are accepted by the parser.
      (goto-char (point-min))
      (end-of-line)
      (skeleton-expand) ;; header
      (goto-char (point-max))
      (skeleton-expand "Project_1") ;; project
      (skeleton-expand-all gpr-skel-token-alist '("header""project"))
      (indent-region (point-min) (point-max))
      (delete-trailing-whitespace (point-min) (point-max))
      (let ((buffer-file-coding-system 'undecided-unix))
	(write-file filename))
      )))

;; end of file
