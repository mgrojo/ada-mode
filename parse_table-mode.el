;; For navigating in a parse_table as output by wisi-generate

(defun parse_table--xref-backend () 'parse_table)

(cl-defgeneric xref-backend-identifier-completion-table ((_backend (eql parse_table)))
  ;; could complete on nonterms, find productions
  nil)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql parse_table)))
  ;; assume we are on one of:
  ;; - ’goto state nnn’ in a state action
  ;; - ’=> State nnn’ in the debug kernels list
  ;; - ’( nnn)’ in the unknown conflicts list
  (save-excursion
    (end-of-line)
    (when (or (looking-back "[Ss]tate \\([0-9]+\\),?")
	      (looking-back "( \\([0-9]+\\))"))
      (match-string 1))))

(cl-defgeneric xref-backend-definitions ((_backend (eql parse_table)) identifier)
  ;; state tables are self-contained; IDENTIFIER must be a state number
  (save-excursion
    (goto-char (point-min))
    (search-forward-regexp (concat "^State " identifier ":$"))
    (list (xref-make identifier (xref-make-buffer-location (current-buffer) (match-beginning 0))))))

(define-minor-mode parse_table-mode
  "Provides navigation in wisi-generate parse table output."
  nil ":parse_table" nil
  (add-hook 'xref-backend-functions #'parse_table--xref-backend nil t)

  (if parse_table-mode
      (read-only-mode 0)
    (read-only-mode 1)
  ))

(provide 'parse_table-mode)
;; end of file
