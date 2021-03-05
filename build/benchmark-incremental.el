(package-initialize)

(require 'ada-mode)

(setq ada-mode-dir (file-name-directory (locate-file "ada-mode.el" load-path)))

(setq ada-process-parse-exec (expand-file-name "ada_mode_wisi_lr1_parse.exe" ada-mode-dir))

(setq-default wisi-incremental-parse-enable t)
(find-file "c:/eurocontrol/gnatcoll-xref.adb")

(goto-line 3000)
(wisi-time (lambda () (indent-rigidly (point)(line-beginning-position 2) -1)(indent-for-tab-command)) 4 t); t for process stats
(split-window-vertically)
(pop-to-buffer "*Messages*")
(kill-buffer "gnatcoll-xref.adb")
