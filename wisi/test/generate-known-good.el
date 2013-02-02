;; Generate a wisent parser table, output as elisp to compare with
;; OpenToken generated version.

;; set up for calling `semantic-grammar-create-package'
(add-to-list 'load-path "../../../org.emacs.ada-mode.smie"); wisi.el
(require 'cl)

(defun wisi-generate (&optional file)
  (let* ((file (or file (car command-line-args-left))))
    (unless (and file (file-exists-p file))
        (error "Argument %s is not a valid file name" file))

    (condition-case err
	(with-current-buffer (find-file-noselect file)
	  (semantic-mode)
	  (semantic-grammar-create-package))
      (error
       (message "%s" (error-message-string err))
       nil))))

;;;; end of file
