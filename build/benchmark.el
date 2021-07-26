;; emacs command line arg: Ada source file to parse.
(package-initialize)

(setq benchmark-source-file (pop command-line-args-left))

(require 'ada-mode)

(setq ada-mode-dir (file-name-directory (locate-file "ada-mode.el" load-path)))

(set-default 'wisi-size-threshold most-positive-fixnum); before open buffer
(setq-default wisi-incremental-parse-enable nil)

(setq ada-process-parse-exec (expand-file-name "ada_mode_wisi_lr1_parse.exe" ada-mode-dir))

(find-file benchmark-source-file)
(wisi-parse-buffer 'indent) ; time with warm caches.
(message "partial parse:")
(message "navigate")
(wisi-time (lambda () (wisi-parse-buffer 'navigate)) 4 t); t for process stats
(message "face")
(wisi-time (lambda () (wisi-parse-buffer 'face)) 4 t); t for process stats
(message "indent")
(wisi-time (lambda () (wisi-parse-buffer 'indent)) 4 t); t for process stats
(message "re-indent")
(goto-char (/ (point-max) 2))
(goto-char (line-beginning-position))
(wisi-time (lambda () (indent-rigidly (point)(line-beginning-position 2) -1)(indent-for-tab-command)) 4 t)

(setq-default wisi-incremental-parse-enable t)
(message "incremental parse:")
(message "initial")
(wisi-time (lambda () (wisi-parse-incremental wisi--parser t)) 4 t)
(message "re-indent")
(goto-char (/ (point-max) 2))
(goto-char (line-beginning-position))
(wisi-time (lambda () (indent-rigidly (point)(line-beginning-position 2) -1)(indent-for-tab-command)) 4 t)

;; FIXME: find a fontified word, time re-fontify

(split-window-vertically)
(pop-to-buffer "*Messages*")

;; end of file
