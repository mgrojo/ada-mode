;;; wisitoken_grammar_1-process.el --- Generated parser support file  -*- lexical-binding:t -*-
;;  command line: wisitoken-bnf-generate.exe  --generate LR1 Ada_Emacs re2c PROCESS wisitoken_grammar_1.wy

;;  Copyright (C) 2017, 2018 Free Software Foundation, Inc.
;;
;;  Author: Stephen Leake <stephe-leake@stephe-leake.org>
;;
;;  This file is part of GNU Emacs.
;;
;;  GNU Emacs is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;
;;  GNU Emacs is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.
;;
;;  You should have received a copy of the GNU General Public License
;;  along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

(require 'wisi-process-parse)

(defconst wisitoken_grammar_1-process-token-table
  [WHITESPACE
   NEW_LINE
   COMMENT
   CODE
   END
   IF
   KEYWORD
   NON_GRAMMAR
   TOKEN
   RAW_CODE
   REGEXP
   ACTION
   BAR
   COLON
   COMMA
   EQUAL
   GREATER
   LEFT_BRACE
   LEFT_BRACKET
   LEFT_PAREN
   LESS
   PERCENT
   PLUS
   QUESTION
   RIGHT_BRACE
   RIGHT_BRACKET
   RIGHT_PAREN
   SEMICOLON
   SLASH
   STAR
   NUMERIC_LITERAL
   IDENTIFIER
   STRING_LITERAL_1
   STRING_LITERAL_2
   Wisi_EOI
   wisitoken_accept
   declaration
   token_keyword_non_grammar
   identifier_list
   declaration_item_list
   declaration_item
   nonterminal
   semicolon_opt
   rhs_list
   rhs
   token_item
   token_list
   compilation_unit
   compilation_unit_list
   ])

(defconst wisitoken_grammar_1-process-face-table
  [
   font-lock-constant-face
   font-lock-function-name-face
   font-lock-keyword-face
   font-lock-string-face
   font-lock-type-face
   nil
   ])

(provide 'wisitoken_grammar_1-process)
