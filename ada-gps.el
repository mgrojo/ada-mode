;; Emacs ada-mode indentation engine, using GPS code in a subprocess.
;;
;; GNAT is provided by AdaCore; see http://libre.adacore.com/
;;
;;; Copyright (C) 2014  Free Software Foundation, Inc.
;;
;; Author: Stephen Leake <stephen_leake@member.fsf.org>
;; Maintainer: Stephen Leake <stephen_leake@member.fsf.org>
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;;;; sessions

;; ada_mode_gps_indent runs a loop, waiting for indentation requests.
;;
;; We only need one process; there is no persistent state.

(cl-defstruct (ada-gps--session)
  (process nil) ;; running gpr_query
  (buffer nil))  ;; receives output of gpr_query

(defvar ada-gps-session
  (make-ada-gps--session)
  "The single instance of ada-gps--session")

(defconst ada-gps-buffer-name " *ada_gps*")

(defcustom ada-gps-exec "ada_mode_gps_indent"
  "Name of executable to use for ada_mode_gps_indent,"
  :type 'string
  :group 'ada-indentation)

(defun ada-gps--start-process ()
  "Start the session process running ada_gps."
  (unless (buffer-live-p (ada-gps--session-buffer ada-gps-session))
    ;; user may have killed buffer
    (setf (ada-gps--session-buffer ada-gps-session) (get-buffer-create ada-gps-buffer-name)))

  (let ((exec-file (locate-file ada-gps-exec exec-path '(".exe"))))
    (unless exec-file
      (error "%s not found on `exec-path'" ada-gps-exec))

    (with-current-buffer (ada-gps--session-buffer ada-gps-session)
      (erase-buffer); delete any previous messages, prompt
      (setf (ada-gps--session-process ada-gps-session)
	    (start-process ada-gps-buffer-name (current-buffer) exec-file))
      (set-process-query-on-exit-flag (ada-gps--session-process ada-gps-session) nil)
      (ada-gps-session-wait)

      ;; check for warnings about invalid directories etc
      (goto-char (point-min))
      (when (search-forward "warning:" nil t)
	(error "ada_gps warnings"))
      )))

(defconst ada-gps-prompt "^GPS_Indent> $"
  "Regexp matching ada_mode_gps_indent prompt; indicates previous command is complete.")

(defun ada-gps-session-wait ()
  "Wait for the current command to complete."
  (unless (process-live-p (ada-gps--session-process ada-gps-session))
    (ada-gps-show-buffer ada-gps-session)
    (error "ada-gps process died"))

  (with-current-buffer (ada-gps--session-buffer ada-gps-session)
    (let ((process (ada-gps--session-process ada-gps-session))
	  (search-start (point-min)))
      (while (and (process-live-p process)
		  (progn
		    ;; process output is inserted before point, so move back over it to search it
		    (goto-char search-start)
		    (not (re-search-forward ada-gps-prompt (point-max) 1))))
	(setq search-start (point));; don't search same text again
	(accept-process-output process 1.0))
      (unless (process-live-p process)
	(ada-gps-show-buffer)
	(error "ada_gps process died"))
      )))

(defun ada-gps-session-send (cmd wait)
  "Send CMD to ada_gps session, prepended with CMD byte count.
If WAIT is non-nil, wait for command to complete."
  ;; always wait for previous command to complete; also checks for
  ;; dead process.
  (ada-gps-session-wait)
  (let ((byte-count-img (format "%02d" (length cmd))))
    (with-current-buffer (ada-gps--session-buffer ada-gps-session)
      (erase-buffer)
      (process-send-string
       (ada-gps--session-process ada-gps-session)
       (concat byte-count-img cmd))
      (when wait
	(ada-gps-session-wait))
      )))

(defun ada-gps-kill-session ()
  (interactive)
  (when (process-live-p (ada-gps--session-process ada-gps-session))
    (process-send-string (ada-gps--session-process ada-gps-session) "04exit")
    ))

(defun ada-gps-show-buffer (&optional session)
  "Show ada-gps buffer."
  (interactive)
  (pop-to-buffer (ada-gps--session-buffer ada-gps-session)))

;;;;; indenting

(defun ada-gps-indent-line ()
  "Indent current line using the ada-gps indentation engine."
  (interactive)
  (let ((savep (point))
	indent)
    (save-excursion
      (back-to-indentation)
      (when (>= (point) savep) (setq savep nil))

      ;; send complete current line
      (end-of-line)
      (ada-gps-session-send (format "compute_indent %d %d" (line-number-at-pos) (point)) nil)
      (ada-gps-session-send (buffer-substring-no-properties (point-min) (point)) t)
      )
    (with-current-buffer (ada-gps--session-buffer ada-gps-session)
      (goto-char (point-min))
      (looking-at (concat ada-gps-prompt "\\([0-9+]\\)$"))
      (setq indent (string-to-number (match-string 1)))
      )

    (if savep
	;; point was inside line text; leave it there
	(save-excursion (indent-line-to indent))
      ;; point was before line text; move to start of text
      (indent-line-to indent))
    ))


(defun ada-gps-setup ()
  "Set up a buffer for indenting with ada-gps."
  (set (make-local-variable 'indent-line-function) 'ada-gps-indent-line)
  (unless (ada-gps--session-process ada-gps-session)
    ;; just loaded; create buffer, process
   (ada-gps--start-process)
   (ada-gps-session-wait))

  ;; FIXME: need this?
  ;; (set (make-local-variable 'comment-indent-function) 'wisi-comment-indent)

  ;; FIXME: fontify 'return' (see ada-wisi-post-local-vars)
  )

;; FIXME: make this per-buffer, depending on size threshold
(add-hook 'ada-mode-hook 'ada-gps-setup)

(provide 'ada-gps)
(provide 'ada-indent-engine)

;; end of file
