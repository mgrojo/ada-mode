;;; wisi-parse-common.el --- declarations used by wisi-parse.el, wisi-ada-parse.el, and wisi.el
;;
;; Copyright (C) 2014  Free Software Foundation, Inc.
;;
;; Author: Stephen Leake <stephen_leake@member.fsf.org>
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

(defvar wisi-debug 0
  "wisi debug mode:
0 : normal - ignore parse errors, for indenting new code
1 : report parse errors (for running tests)
2 : show parse states, position point at parse errors, debug-on-error works in parser
3 : also show top 10 items of parser stack.")

(defvar-local wisi-cache-max 0
  "Maximimum position in buffer where wisi-cache text properties are valid.")

(defun wisi-error-msg (message &rest args)
  (let ((line (line-number-at-pos))
	(col (- (point) (line-beginning-position))))
    (format
     "%s:%d:%d: %s"
       (buffer-name) ;; buffer-file-name is sometimes nil here!?
       line col
       (apply 'format message args))))

(defvar wisi-parse-error nil)
(put 'wisi-parse-error
     'error-conditions
     '(error wisi-parse-error))
(put 'wisi-parse-error
     'error-message
     "wisi parse error")


(provide 'wisi-parse-common)
