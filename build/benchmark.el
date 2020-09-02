(package-initialize)

(require 'ada-mode)

(setq ada-mode-dir (file-name-directory (locate-file "ada-mode.el" load-path)))

(set-default 'wisi-size-threshold most-positive-fixnum); before open buffer
(setq ada-process-parse-exec (expand-file-name "ada_mode_wisi_lr1_parse.exe" ada-mode-dir))
(find-file "c:/eurocontrol/gnatcoll-xref.adb")
(wisi-parse-buffer 'indent) ; time with warm caches.
(message "navigate")
(wisi-time (lambda () (wisi-parse-buffer 'navigate)) 4 t); t for process stats
(message "face")
(wisi-time (lambda () (wisi-parse-buffer 'face)) 4 t); t for process stats
(message "indent")
(wisi-time (lambda () (wisi-parse-buffer 'indent)) 4 t); t for process stats
(split-window-vertically)
(pop-to-buffer "*Messages*")
(kill-buffer "gnatcoll-xref.adb")
