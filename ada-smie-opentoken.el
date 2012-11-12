;;; An indentation function for ada-smie that indents OpenToken
;;; grammar statements nicely.
;;
;; To be added on a per-file basis in Local Variables:
;;
;; Local Variables:
;; eval: (require 'ada-smie-opentoken)
;; eval: (add-to-list 'smie-indent-functions 'ada-smie-opentoken)
;; End:

(defcustom ada-indent-opentoken nil
  "If non-nil, apply `ada-smie-opentoken' indentation rule."
  :type 'boolean :group 'ada-indentation)
(make-variable-buffer-local 'ada-indent-opentoken)
(put 'ada-indent-opentoken 'safe-local-variable 'booleanp)

(defun ada-smie-opentoken ()
  "Return appropriate indentation (an integer column) for continuation lines in an OpenToken grammar statement."
  ;; We don't do any checking to see if we actually are in an
  ;; OpenToken grammar statement, since this rule should only be
  ;; included in package specs that exist solely to define OpenToken
  ;; grammar fragments.
  (when ada-indent-opentoken
    (save-excursion
      (let ((token (smie-default-backward-token)))
	(cond
	 ((equal token "<=")
	  (back-to-indentation)
	  (+ (current-column) ada-indent-broken))

	 ((member token '("+" "&"))
	  (back-to-indentation)
	  (current-column))
	 )))))

;; This must be last on ada-mode-hook, so ada-smie-opentoken is first
;; on smie-indent-functions
(add-hook 'ada-mode-hook
	  (lambda () (add-to-list 'smie-indent-functions 'ada-smie-opentoken))
	  t)

(provide 'ada-smie-opentoken)
;; end of file
