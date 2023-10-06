;; emacs command line arg: Ada source file to parse.
(package-initialize)

(setq debug-on-error t)
(setq benchmark-source-file (pop command-line-args-left))

(require 'ada-mode)

(setq ada-mode-dir (file-name-directory (locate-file "ada-mode.el" load-path)))

(set-default 'wisi-size-threshold most-positive-fixnum); before open buffer
(setq-default wisi-incremental-parse-enable nil)
(setq-default wisi-process-time-out 60.0);; should not be this slow, but this allows better debugging

(setq ada-process-parse-exec (expand-file-name "bin/ada_mode_wisi_lr1_parse" ada-mode-dir))
(message "creating parser ...")
(let ((parser (ada-parse-require-process :wait t)))
  (wisi-parse-enable-memory-report parser)
  (wisi-parse-memory-report parser)) ;; 0
(message "... done")

(find-file benchmark-source-file) ;; does not do initial parse, since incremental not enabled
(wisi-parse-buffer 'indent) ; time with warm caches.
(wisi-parse-memory-report wisi-parser-shared)

(message "partial parse:")

(message "navigate")
(wisi-time (lambda () (wisi-parse-buffer 'navigate)) 4 :report-wait-time t)

(message "face")
(wisi-time (lambda () (wisi-parse-buffer 'face)) 4 :report-wait-time t)

(message "indent")
(wisi-time (lambda () (wisi-parse-buffer 'indent)) 4 :report-wait-time t)
(wisi-parse-memory-report wisi-parser-shared) ;; memory for post-parse caches

(message "re-indent")
(goto-char (/ (point-max) 3))
(goto-char (line-beginning-position))
(wisi-time (lambda () (indent-rigidly (point)(line-beginning-position 2) -1)(indent-for-tab-command)) 4 :report-wait-time t)
(wisi-parse-memory-report wisi-parser-shared)

(message "recover")
(goto-char (/ (point-max) 2))
(goto-char (line-beginning-position))
(forward-comment (point-max))
(kill-line 2)
(wisi-time 'indent-for-tab-command 4 :report-wait-time t)
(undo)
(wisi-parse-memory-report wisi-parser-shared)

(message "reset parser")
(wisi-reset-parser) ;; delete syntax tree before reset memory counts
(wisi-parse-memory-report wisi-parser-shared) ;; current should be near 0; just parse table

(setq-default wisi-incremental-parse-enable t)
(message "incremental parse:")

(message "initial")
(wisi-time (lambda () (wisi-parse-incremental wisi-parser-shared 'none :full t)) 4 :report-wait-time t)
(wisi-parse-memory-report wisi-parser-shared)

(message "re-indent")
(goto-char (/ (point-max) 3))
(goto-char (line-beginning-position))
(wisi-time (lambda () (indent-rigidly (point)(line-beginning-position 2) -1)(indent-for-tab-command)) 4 :report-wait-time t)
(wisi-parse-memory-report wisi-parser-shared)

(defun time-recover ()
  (goto-char (line-beginning-position))
  (forward-comment (point-max))
  (kill-line 2)
  (wisi-time 'indent-for-tab-command 4 :report-wait-time t)
  (undo)
  (indent-for-tab-command);; parse with no errors for next test
  )

(message "recover near start of file")
(goto-char (+ (point-min) 10000))
(time-recover)

(message "recover near end of file")
(goto-char (- (point-max) 10000))
(time-recover)

(wisi-parse-memory-report wisi-parser-shared)

(split-window-vertically)
(pop-to-buffer "*Messages*")

;; end of file
