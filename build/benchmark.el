;; emacs command line arg: Ada source file to parse.
(package-initialize)

(setq benchmark-source-file (pop command-line-args-left))

(require 'ada-mode)

(setq ada-mode-dir (file-name-directory (locate-file "ada-mode.el" load-path)))

(set-default 'wisi-size-threshold most-positive-fixnum); before open buffer
(setq-default wisi-incremental-parse-enable nil)

(setq ada-process-parse-exec (expand-file-name "ada_mode_wisi_lr1_parse.exe" ada-mode-dir))
(let ((parser (ada-parse-require-process)))
  (wisi-parse-enable-memory-report parser)
  (wisi-parse-memory-report parser))

(setq-default wisi-incremental-parse-enable nil)
(find-file benchmark-source-file) ;; does not do initial parse, since incremental not enabled
(wisi-parse-buffer 'indent) ; time with warm caches.
(wisi-parse-memory-report wisi--parser)

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
(wisi-parse-memory-report wisi--parser)

(setq-default wisi-incremental-parse-enable t)
(message "incremental parse:")
(message "initial")
(wisi-time (lambda () (wisi-parse-incremental wisi--parser 'none :full t)) 4 t)
(wisi-parse-memory-report wisi--parser)

(message "re-indent")
(goto-char (/ (point-max) 2))
(goto-char (line-beginning-position))
(wisi-time (lambda () (indent-rigidly (point)(line-beginning-position 2) -1)(indent-for-tab-command)) 4 t)
(wisi-parse-memory-report wisi--parser)

;; FIXME: find a fontified word, time re-fontify

(split-window-vertically)
(pop-to-buffer "*Messages*")

;; end of file
