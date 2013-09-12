;;; An indentation function for ada-smie that indents OpenToken
;;; grammar statements nicely.
;;
;; In ~/.emacs:
;; (require 'ada-smie-opentoken)
;;
;; In each file that declares OpenToken grammars:
;;
;; Local Variables:
;; ada-indent-opentoken: t
;; End:

(require 'ada-mode)
(require 'ada-smie)

(defcustom ada-indent-opentoken nil
  "If non-nil, apply `ada-smie-opentoken' indentation rule."
  :type 'boolean
  :group 'ada-indentation
  :safe 'booleanp)
(make-variable-buffer-local 'ada-indent-opentoken)

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
	  (ada-smie-backward-tokens-unrefined "<=")
	  (back-to-indentation)
	  (+ (current-column) ada-indent-broken))
	 )))))

(defun ada-smie-opentoken-setup ()
  (add-to-list 'smie-indent-functions 'ada-smie-opentoken))

;; This must be last on ada-mode-hook, so ada-smie-opentoken is first
;; on smie-indent-functions
(add-hook 'ada-mode-hook 'ada-smie-opentoken-setup t)

(add-to-list 'ada-align-rules
	     '(ada-opentoken
	       (regexp  . "[^=]\\(\\s-*\\)<=")
	       (valid   . (lambda() (not (ada-in-comment-p))))
	       (modes   . '(ada-mode))))

(provide 'ada-smie-opentoken)
;; end of file
