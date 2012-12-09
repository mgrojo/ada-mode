;;; user options shared by Ada mode indentation engines
;;
;; Copyright (C) 2012  Free Software Foundation, Inc.
;;
;; Author: Stephen Leake <stephen_leake@member.fsf.org>
;; Contributors: Simon Wright <simon.j.wright@mac.com>
;;
;; Keywords: languages ada
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
;;; History: see ada_mode.el

;;;; code

(defgroup ada-indentation nil
  "Indentation options for Ada source."
  :group 'ada)

(defcustom ada-indent 3
  "*Size of Ada default indentation, when no other indentation is used.

Example :
procedure Foo is
begin
>>>null;"
  :type 'integer  :group 'ada-indentation)
(put 'ada-indent 'safe-local-variable 'integerp)

(defvar ada-broken-indent nil)
(make-obsolete-variable
 'ada-broken-indent
 'ada-indent-broken
 "Emacs 24.4, Ada mode 5.0")
;; FIXME (later): this doesn't warn user at runtime, but at least they should
;; notice something broke, and the help will be useful.

(defcustom ada-indent-broken 2
  "*Indentation for the continuation of a broken line.

Example :
   My_Var : My_Type :=
   >>(Field1 => Value);"
  :type 'integer :group 'ada-indentation)
(put 'ada-indent-broken 'safe-local-variable 'integerp)

(defcustom ada-indent-comment-col-0 nil
  "If non-nil, comments currently starting in column 0 are left in column 0.
Otherwise, they are indented with previous comments or code."
  :type 'boolena :group 'ada-indentation)
(put 'ada-indent-comment-col-0 'safe-local-variable 'booleanp)

(define-obsolete-variable-alias
 'ada-label-indent
 'ada-indent-label
 "Emacs 24.4, Ada mode 5.0")
;; FIXME (later): this doesn't warn user at runtime, and they won't notice
;; something is wrong until we delete it, and then there won't be any
;; useful help.

(defcustom ada-indent-label -3
  ;; Ada mode 4.01 and earlier default this to -4. But that is
  ;; incompatible with the default gnat indentation style check, which
  ;; wants all indentations to be a multiple of 3 (with some
  ;; exceptions). So we default this to -3.
  "*Indentation for a loop, block, or statement label, relative to the item it labels.

Example :
   Label_1 :
   <<<<declare

   <<Label_2>>
   <<<<Foo := 0;"
  :type 'integer :group 'ada-indentation)
(put 'ada-indent-label 'safe-local-variable 'integerp)

(defcustom ada-indent-record-rel-type 3
  "*Indentation for 'record' relative to 'type' or 'use'.

An example is:
   type A is
   >>>record"
  :type 'integer :group 'ada-indent)
(put 'ada-indent-record-rel-type 'safe-local-variable 'integerp)

(defcustom ada-indent-renames 2
  "*Indentation for 'renames' relative to the matching subprogram keyword.

If `ada-indent-renames' is zero or less, then
- if the subprogram has parameters, the indentation is done
  relative to the open parenthesis;
- if not, `ada-indent-broken' is used relative to the keyword.

An example is:
   function A (B : Integer)
              return C;
   >>renames Foo;"
:type 'integer :group 'ada-indent)
(put 'ada-indent-renames 'safe-local-variable 'integerp)

(defcustom ada-indent-return 0
  "*Indentation for 'return' relative to the matching 'function' keyword.

If `ada-indent-return' is zero or less, then
- if the function has parameters, the indentation is done
  relative to the open parenthesis;
- if not, `ada-indent-broken' is used relative to 'function'.

An example is:
   function A (B : Integer)
   >>>>>>>>>>>return C;"
:type 'integer :group 'ada-indent)
(put 'ada-indent-return 'safe-local-variable 'integerp)

(defvar ada-use-indent nil)
(make-obsolete-variable
 'ada-use-indent
 'ada-indent-use
 "Emacs 24.4, Ada mode 5.0")
(defcustom ada-indent-use ada-indent-broken
  "*Indentation for the lines in a 'use' statement.

An example is:
   use Ada.Text_IO,
   >>Ada.Numerics;"
  :type 'integer :group 'ada)
(put 'ada-indent-use 'safe-local-variable 'integerp)

(defvar ada-when-indent nil)
(make-obsolete-variable
 'ada-when-indent
 'ada-indent-when
 "Emacs 24.4, Ada mode 5.0")

(defcustom ada-indent-when (or ada-when-indent 3)
  "*Indentation for 'when' relative to 'exception', 'case', 'or' in select.

An example is:
   case A is
   >>>when B =>"
  :type 'integer :group 'ada-indent)
(put 'ada-indent-when 'safe-local-variable 'integerp)

(defvar ada-with-indent nil)
(make-obsolete-variable
 'ada-with-indent
 'ada-indent-with
 "Emacs 24.4, Ada mode 5.0")

(defcustom ada-indent-with ada-indent-broken
  "*Indentation for the lines in a 'with' statement.

An example is:
   with Ada.Text_IO,
   >>Ada.Numerics;"
  :type 'integer :group 'ada)
(put 'ada-indent-with 'safe-local-variable 'integerp)

(provide 'ada-indent-user-options)

;; end file
