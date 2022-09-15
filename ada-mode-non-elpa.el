;;; Activate ada-mode outside the Emacs package system -*- no-byte-compile: t -*-

(let ((ada-mode-dir "path/to/ada-mode")
      (wisi-dir "path/to/wisi"))

  (add-to-list 'load-path ada-mode-dir)
  (add-to-list 'load-path wisi-dir)

  (load-file (concat ada-mode-dir "/autoloads.el"))
  (load-file (concat wisi-dir "/autoloads.el")))

;;; ada-mode-non-elpa.el ends here
