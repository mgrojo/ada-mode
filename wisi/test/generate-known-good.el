;; Generate a wisent parser table, output as elisp to compare with
;; OpenToken generated version.

;; set up for calling `semantic-grammar-create-package'
(add-to-list 'load-path "../../../org.emacs.ada-mode.smie"); wisi.el
(add-to-list 'load-path "."); *-wy.el
(require 'cl)
(require 'semantic/wisent/comp)

(defun wisi-generate (&optional file)
  (let* ((file (or file (car command-line-args-left))))
    (unless (and file (file-exists-p file))
        (error "Argument %s is not a valid file name" file))

    (condition-case err
	(with-current-buffer (find-file-noselect file)
	  (semantic-mode)
	  (let ((packagename (semantic-grammar-create-package))
		(wisent-verbose-flag t))

	    (when (file-exists-p "wisent.output")
	      (delete-file "wisent.output"))

	    (load packagename);; generates wisent.output
	    ))
      (error
       (message "%s" (error-message-string err))
       nil))))

;;;; end of file
