;; project settings for ada-mode with eglot -*- no-byte-compile : t -*-

;; FIXME: use ada-eglot
(setq eglot-extend-to-xref t)

(let ((dir (file-name-directory (or load-file-name (buffer-file-name)))))
  (setenv "GPR_PROJECT_PATH"
	  (concat (expand-file-name "." dir)
		path-separator (expand-file-name "../org.emacs.wisi" dir)
		path-separator (expand-file-name "../org.wisitoken/build" dir)
		path-separator (expand-file-name "../org.stephe_leake.sal/build" dir)
		path-separator (expand-file-name "../org.stephe_leake.makerules" dir)
		path-separator (expand-file-name "../org.stephe_leake.aunit_ext/build" dir))
	))
;; end of file
