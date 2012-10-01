(defconst debug-grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
    '(
      (name)

      (statements
       (statement)
       (statement ";" statement))

      (case_statement_alternative
       (statement)
       (statement "when" statement))

      (statement
       ("case" name "is" case_statement_alternative "end")
       (name "=" name))
      ))))

(defun debug-show-keyword-forward ()
  (interactive)
  (message "%s" (assoc (smie-default-forward-token) smie-grammar)))

(defun debug-show-keyword-backward ()
  (interactive)
  (message "%s" (assoc (smie-default-backward-token) smie-grammar)))

(defun debug-show-parent ()
  (interactive)
  (let ((toklevels (smie-backward-sexp (save-excursion (smie-default-forward-token)))))
    (message "%s; %s" toklevels (assoc (save-excursion (smie-default-forward-token)) smie-grammar))))

(defvar debug-map (make-sparse-keymap "debug"))
(defun debug-setup ()
  (interactive)
  (smie-setup debug-grammar nil
	      :forward-token #'smie-default-forward-token
	      :backward-token #'smie-default-backward-token)

  (use-local-map debug-map)

  (define-key debug-map "\M-i" 'debug-show-keyword-backward)
  (define-key debug-map "\M-k" 'debug-show-keyword-forward)
  (define-key debug-map "\M-o" 'debug-show-parent)

  )
