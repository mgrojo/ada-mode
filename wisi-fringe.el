;;; wisi-fringe.el --- show approximate error locations in the fringe
;;
;; Copyright (C) 2018  Free Software Foundation, Inc.
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
;;
;; Design:
;;
;; Bitmaps are displayed in the fringe by putting a 'display property
;; on buffer text. However, just doing that also hides the buffer
;; text. To avoid that, we put the ’display property on a string, and
;; then an overlay containing that string as ’before-string on the
;; newline of a buffer line.

(defun wisi-fringe-create-bitmaps ()
  "Return an array of bitmap symbols containing the fringe bitmaps."
  ;; In condensing the entire buffer to the current window height, we
  ;; assume a 10 point font, which allows 6 distinct line positions
  ;; each one pixel high, with one blank pixel between.

  (let ((result (make-vector 64 nil))
	(i 1)) ;; bitmap 0 is never used
    (while (< i (length result))
      (aset result i
	    (define-fringe-bitmap (intern (format "wisi-fringe--line-%d-bmp" i))
	      (vector
	       (if (>= i 32) #b11111111 #b00000000)
	       #b00000000
	       (if (>= (% i 32) 16) #b11111111 #b00000000)
	       #b00000000
	       (if (>= (% i 16) 8) #b11111111 #b00000000)
	       #b00000000
	       (if (>= (% i 8) 4) #b11111111 #b00000000)
	       #b00000000
	       (if (>= (% i 4) 2) #b11111111 #b00000000)
	       #b00000000
	       (if (>= (% i 2) 1) #b11111111 #b00000000)
	       )))
      (setq i (1+ i)))
    result))

(defconst wisi-fringe-bitmaps (wisi-fringe-create-bitmaps)
  "Array of 64 bitmap symbols.")

(defun wisi-fringe--put (line bitmap-index)
  (save-excursion
    (goto-char (point-min))
    (forward-line line)
    (let* ((endpos (line-end-position))
	   (ov (make-overlay endpos (1+ endpos)))
	   (bmp (aref wisi-fringe-bitmaps bitmap-index)))
      (overlay-put ov 'before-string (propertize "-" 'display (list 'right-fringe bmp 'compilation-error)))
      (overlay-put ov 'wisi-fringe t)
      )))

(defun wisi-fringe--scale (error-line buffer-lines window-line-first window-lines)
  "Return a cons (LINE . BIN) for ERROR-LINE,
where LINE is the line to display the error bar on, and BIN is a
6-bit bit vector giving the relative position in that line.
BUFFER-LINES is the count of lines in the buffer.
WINDOW-LINE-FIRST is the first and last lines of the buffer
visible in the window. WINDOW-LINES is the count of lines visible
in the window."
  ;; FIXME: handle end of buffer inside window

  ;; partial-lines / window-line = 6
  ;; buffer-lines / window-line = 1/scale
  ;; buffer-lines / partial-line  = (window-line / partial-lines) * (buffer-lines / window-line) = 1/6 * 1/scale
  (let* ((scale (/ window-lines (float buffer-lines)))
	 (line (floor (* scale error-line)))
	 (rem (- error-line (floor (/ line scale)))))
    (cons (+ window-line-first line) (lsh 1 (floor (* rem (* 6 scale)))))))

(defun wisi-fringe-display-errors (positions)
  "Display a bar in the right fringe for each buffer position in POSITIONS.
The buffer containing POSITIONS must be current, and the window
displaying that buffer must be current."
  ;; FIXME: recompute fringe display on scroll!
  (let (scaled-posns
	(buffer-lines (line-number-at-pos (point-max)))
	(window-lines (window-height))
	(window-line-first (line-number-at-pos (window-start))))
    (dolist (pos positions)
      (let ((scaled-pos (wisi-fringe--scale (line-number-at-pos pos) buffer-lines window-line-first window-lines)))
	(if (and scaled-posns
		 (= (caar scaled-posns) (car scaled-pos)))
	    (setcdr (car scaled-posns) (logior (cdar scaled-posns) (cdr scaled-pos)))
	  (push scaled-pos scaled-posns))
	))

    (remove-overlays (point-min) (point-max) 'wisi-fringe t)
    (dolist (pos scaled-posns)
      (wisi-fringe--put (car pos) (cdr pos)))
    ))

(provide 'wisi-fringe)
