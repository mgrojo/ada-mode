;; Disable installed ELPA packages except mmm-mode
(defvar package-load-list '(all))
(push '(ada-mode . nil) package-load-list)
(push '(wisi . nil) package-load-list)

;; end of file
