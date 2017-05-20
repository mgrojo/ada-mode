;;; gpr-grammar-elisp.el --- Generated parser support file  -*- lexical-binding:t -*-
;;; with command line: wisi-generate.exe -v 1 gpr-grammar.wy

;; Copyright (C) 2013 - 2015 Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.
;;
;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

(require 'wisi)
(require 'semantic/lex)
(require 'wisi-compile)

(defconst gpr-grammar-elisp-keyword-table
  (semantic-lex-make-keyword-table
   '(
    ("abstract" . ABSTRACT)
    ("aggregate" . AGGREGATE)
    ("case" . CASE)
    ("configuration" . CONFIGURATION)
    ("end" . END)
    ("extends" . EXTENDS)
    ("external" . EXTERNAL)
    ("external_as_list" . EXTERNAL_AS_LIST)
    ("for" . FOR)
    ("is" . IS)
    ("(" . LEFT_PAREN)
    ("library" . LIBRARY)
    ("null" . NULL)
    ("others" . OTHERS)
    ("package" . PACKAGE)
    ("project" . PROJECT)
    ("renames" . RENAMES)
    (")" . RIGHT_PAREN)
    ("standard" . STANDARD)
    ("type" . TYPE)
    ("use" . USE)
    ("when" . WHEN)
    ("with" . WITH)
    )
   nil)
  "Table of language keywords.")

(defconst gpr-grammar-elisp-token-table
  (semantic-lex-make-type-table
   '(
     ("punctuation"
      (AMPERSAND . "&")
      (COLON . ":")
      (COLON_EQUALS . ":=")
      (COMMA . ",")
      (DOT . ".")
      (EQUAL_GREATER . "=>")
      (QUOTE . "'")
      (SEMICOLON . ";")
      (VERTICAL_BAR . "|")
     )
     ("symbol"
      (IDENTIFIER . "[a-zA-Z][0-9a-zA-Z_]*")
     )
     ("string-double"
      (STRING_LITERAL . "\"[^\"]*\"")
     )
    )
   nil)
  "Table of language tokens.")

(defconst gpr-grammar-elisp-parse-table
   (wisi-compile-grammar
   '((AMPERSAND COLON COLON_EQUALS COMMA DOT EQUAL_GREATER QUOTE SEMICOLON VERTICAL_BAR IDENTIFIER STRING_LITERAL ABSTRACT AGGREGATE CASE CONFIGURATION END EXTENDS EXTERNAL EXTERNAL_AS_LIST FOR IS LEFT_PAREN LIBRARY NULL OTHERS PACKAGE PROJECT RENAMES RIGHT_PAREN STANDARD TYPE USE WHEN WITH )
     ((aggregate
       ((LEFT_PAREN string_list RIGHT_PAREN )
        (wisi-indent-action [0 (wisi-anchored 1 1) (wisi-anchored 1 0)])))
      (attribute_declaration
       ((FOR IDENTIFIER USE expression SEMICOLON )
        (progn
        (wisi-statement-action [1 statement-start 5 statement-end])
        (wisi-face-apply-action [2 font-lock-function-name-face])
        (wisi-indent-action [0 gpr-indent-broken 0 gpr-indent-broken 0])))
       ((FOR IDENTIFIER LEFT_PAREN discrete_choice RIGHT_PAREN USE expression SEMICOLON )
        (progn
        (wisi-statement-action [1 statement-start 8 statement-end])
        (wisi-face-apply-action [2 font-lock-function-name-face])
        (wisi-indent-action [0 gpr-indent-broken (1- gpr-indent-broken) gpr-indent-broken (1- gpr-indent-broken) 0
        gpr-indent-broken 0])))
       ((FOR EXTERNAL LEFT_PAREN STRING_LITERAL RIGHT_PAREN USE expression SEMICOLON )
        (progn
        (wisi-statement-action [1 statement-start 8 statement-end])
        (wisi-indent-action [0 gpr-indent-broken (1- gpr-indent-broken) gpr-indent-broken (1- gpr-indent-broken) 0
        gpr-indent-broken 0]))))
      (attribute_prefix
       ((PROJECT ))
       ((name )))
      (attribute_reference
       ((attribute_prefix QUOTE IDENTIFIER ))
       ((attribute_prefix QUOTE IDENTIFIER LEFT_PAREN STRING_LITERAL RIGHT_PAREN )))
      (case_statement
       ((CASE name IS case_items END CASE SEMICOLON )
        (progn
        (wisi-statement-action [1 statement-start 7 statement-end])
        (wisi-indent-action [0 gpr-indent-broken 0 [gpr-indent-when gpr-indent-when] 0 0 0]))))
      (case_item
       ((WHEN discrete_choice_list EQUAL_GREATER declarative_items_opt )
        (progn
        (wisi-statement-action [1 motion])
        (wisi-indent-action [0 gpr-indent-broken gpr-indent gpr-indent]))))
      (case_items
       (())
       ((case_item ))
       ((case_items case_item )))
      (compilation_unit
       ((context_clause_opt project_qualifier_opt project_declaration_opt )))
      (context_clause
       ((with_clause ))
       ((context_clause with_clause )))
      (context_clause_opt
       (())
       ((context_clause )))
      (declarative_item
       ((simple_declarative_item ))
       ((typed_string_declaration ))
       ((package_declaration )))
      (declarative_items
       ((declarative_item ))
       ((declarative_items declarative_item )))
      (declarative_items_opt
       (())
       ((declarative_items )))
      (discrete_choice
       (())
       ((STRING_LITERAL ))
       ((OTHERS )))
      (discrete_choice_list
       ((discrete_choice ))
       ((discrete_choice_list VERTICAL_BAR discrete_choice )))
      (expression
       ((term ))
       ((expression AMPERSAND term )))
      (external_value
       ((EXTERNAL aggregate ))
       ((EXTERNAL_AS_LIST aggregate )))
      (identifier_opt
       (())
       ((IDENTIFIER )))
      (name
       ((identifier_opt ))
       ((name DOT IDENTIFIER )))
      (package_declaration
       ((package_spec ))
       ((package_extension ))
       ((package_renaming )))
      (package_spec
       ((PACKAGE identifier_opt IS declarative_items_opt END identifier_opt SEMICOLON )
        (progn
        (wisi-statement-action [1 statement-start 7 statement-end])
        (wisi-containing-action 1 4)
        (wisi-face-apply-action [2 font-lock-function-name-face 6 font-lock-function-name-face])
        (wisi-indent-action [0 gpr-indent-broken 0 [gpr-indent gpr-indent] 0 0 0]))))
      (package_extension
       ((PACKAGE identifier_opt EXTENDS name IS declarative_items_opt END identifier_opt SEMICOLON )
        (progn
        (wisi-statement-action [1 statement-start 9 statement-end])
        (wisi-containing-action 1 6)
        (wisi-face-apply-action [2 font-lock-function-name-face 8 font-lock-function-name-face])
        (wisi-indent-action [0 gpr-indent-broken 0 gpr-indent-broken 0 [gpr-indent gpr-indent] 0 0 0]))))
      (package_renaming
       ((PACKAGE identifier_opt RENAMES name SEMICOLON )
        (progn
        (wisi-statement-action [1 statement-start 5 statement-end])
        (wisi-face-apply-action [2 font-lock-function-name-face 4 font-lock-function-name-face]))))
      (project_declaration_opt
       (())
       ((simple_project_declaration ))
       ((project_extension )))
      (project_extension
       ((PROJECT identifier_opt EXTENDS STRING_LITERAL IS declarative_items_opt END identifier_opt SEMICOLON )
        (progn
        (wisi-statement-action [1 statement-start 9 statement-end])
        (wisi-containing-action 1 6)
        (wisi-face-apply-action [2 font-lock-function-name-face 8 font-lock-function-name-face])
        (wisi-indent-action [0 gpr-indent-broken 0 gpr-indent-broken 0 [gpr-indent gpr-indent] 0 0 0]))))
      (project_qualifier_opt
       (())
       ((ABSTRACT ))
       ((STANDARD ))
       ((AGGREGATE ))
       ((AGGREGATE LIBRARY ))
       ((LIBRARY ))
       ((CONFIGURATION )))
      (simple_declarative_item
       ((IDENTIFIER COLON_EQUALS expression SEMICOLON )
        (progn
        (wisi-statement-action [1 statement-start 4 statement-end])
        (wisi-indent-action [0 gpr-indent-broken gpr-indent-broken 0])))
       ((IDENTIFIER COLON IDENTIFIER COLON_EQUALS expression SEMICOLON )
        (progn
        (wisi-statement-action [1 statement-start 6 statement-end])
        (wisi-indent-action [0 gpr-indent-broken gpr-indent-broken gpr-indent-broken gpr-indent-broken 0])))
       ((attribute_declaration ))
       ((case_statement ))
       ((NULL SEMICOLON )
        (wisi-statement-action [1 statement-start 2 statement-end])))
      (simple_project_declaration
       ((PROJECT identifier_opt IS declarative_items_opt END identifier_opt SEMICOLON )
        (progn
        (wisi-statement-action [1 statement-start 7 statement-end])
        (wisi-containing-action 1 4)
        (wisi-face-apply-action [2 font-lock-function-name-face 6 font-lock-function-name-face])
        (wisi-indent-action [0 gpr-indent-broken 0 [gpr-indent gpr-indent] 0 0 0]))))
      (string_primary
       ((STRING_LITERAL ))
       ((name ))
       ((external_value ))
       ((attribute_reference )))
      (string_list
       ((expression ))
       ((string_list COMMA expression )))
      (term
       ((string_primary ))
       ((LEFT_PAREN RIGHT_PAREN ))
       ((aggregate )))
      (typed_string_declaration
       ((TYPE IDENTIFIER IS aggregate SEMICOLON )
        (progn
        (wisi-statement-action [1 statement-start 5 statement-end])
        (wisi-indent-action [0 gpr-indent-broken gpr-indent-broken gpr-indent-broken 0]))))
      (with_clause
       ((WITH string_list SEMICOLON ))))
     [((default . error) (ABSTRACT . (context_clause_opt . 0)) (AGGREGATE . (context_clause_opt . 0)) (CONFIGURATION . (context_clause_opt . 0)) (LIBRARY . (context_clause_opt . 0)) (PROJECT . (context_clause_opt . 0)) (STANDARD . (context_clause_opt . 0)) (WITH .  7) (Wisi_EOI . (context_clause_opt . 0)))
      ((default . error) (PROJECT . (project_qualifier_opt . 1)) (Wisi_EOI . (project_qualifier_opt . 1)))
      ((default . error) (LIBRARY .  33) (PROJECT . (project_qualifier_opt . 3)) (Wisi_EOI . (project_qualifier_opt . 3)))
      ((default . error) (PROJECT . (project_qualifier_opt . 6)) (Wisi_EOI . (project_qualifier_opt . 6)))
      ((default . error) (PROJECT . (project_qualifier_opt . 5)) (Wisi_EOI . (project_qualifier_opt . 5)))
      ((default . error) (EXTENDS . (identifier_opt . 0)) (IS . (identifier_opt . 0)) (IDENTIFIER .  20))
      ((default . error) (PROJECT . (project_qualifier_opt . 2)) (Wisi_EOI . (project_qualifier_opt . 2)))
      ((default . error) (EXTERNAL .  16) (EXTERNAL_AS_LIST .  17) (LEFT_PAREN .  18) (PROJECT .  19) (AMPERSAND . (identifier_opt . 0)) (COMMA . (identifier_opt . 0)) (DOT . (identifier_opt . 0)) (QUOTE . (identifier_opt . 0)) (SEMICOLON . (identifier_opt . 0)) (IDENTIFIER .  20) (STRING_LITERAL .  21))
      ((default . error) (Wisi_EOI . accept))
      ((default . error) (ABSTRACT . (context_clause_opt . 1)) (AGGREGATE . (context_clause_opt . 1)) (CONFIGURATION . (context_clause_opt . 1)) (LIBRARY . (context_clause_opt . 1)) (PROJECT . (context_clause_opt . 1)) (STANDARD . (context_clause_opt . 1)) (WITH .  7) (Wisi_EOI . (context_clause_opt . 1)))
      ((default . error) (ABSTRACT .  1) (AGGREGATE .  2) (CONFIGURATION .  3) (LIBRARY .  4) (PROJECT . (project_qualifier_opt . 0)) (STANDARD .  6) (Wisi_EOI . (project_qualifier_opt . 0)))
      ((default . error) (Wisi_EOI . (project_declaration_opt . 2)))
      ((default . error) (Wisi_EOI . (project_declaration_opt . 1)))
      ((default . error) (ABSTRACT . (context_clause . 0)) (AGGREGATE . (context_clause . 0)) (CONFIGURATION . (context_clause . 0)) (LIBRARY . (context_clause . 0)) (PROJECT . (context_clause . 0)) (STANDARD . (context_clause . 0)) (WITH . (context_clause . 0)) (Wisi_EOI . (context_clause . 0)))
      ((default . error) (PROJECT .  5) (Wisi_EOI . (project_declaration_opt . 0)))
      ((default . error) (ABSTRACT . (context_clause . 1)) (AGGREGATE . (context_clause . 1)) (CONFIGURATION . (context_clause . 1)) (LIBRARY . (context_clause . 1)) (PROJECT . (context_clause . 1)) (STANDARD . (context_clause . 1)) (WITH . (context_clause . 1)) (Wisi_EOI . (context_clause . 1)))
      ((default . error) (LEFT_PAREN .  43))
      ((default . error) (LEFT_PAREN .  43))
      ((default . error) (EXTERNAL .  16) (EXTERNAL_AS_LIST .  17) (LEFT_PAREN .  18) (PROJECT .  19) (RIGHT_PAREN . ( 41 (identifier_opt . 0))) (AMPERSAND . (identifier_opt . 0)) (COMMA . (identifier_opt . 0)) (DOT . (identifier_opt . 0)) (QUOTE . (identifier_opt . 0)) (IDENTIFIER .  20) (STRING_LITERAL .  21))
      ((default . error) (QUOTE . (attribute_prefix . 0)))
      ((default . error) (EXTENDS . (identifier_opt . 1)) (IS . (identifier_opt . 1)) (RENAMES . (identifier_opt . 1)) (RIGHT_PAREN . (identifier_opt . 1)) (AMPERSAND . (identifier_opt . 1)) (COMMA . (identifier_opt . 1)) (DOT . (identifier_opt . 1)) (QUOTE . (identifier_opt . 1)) (SEMICOLON . (identifier_opt . 1)))
      ((default . error) (RIGHT_PAREN . (string_primary . 0)) (AMPERSAND . (string_primary . 0)) (COMMA . (string_primary . 0)) (SEMICOLON . (string_primary . 0)))
      ((default . error) (RIGHT_PAREN . (term . 2)) (AMPERSAND . (term . 2)) (COMMA . (term . 2)) (SEMICOLON . (term . 2)))
      ((default . error) (QUOTE .  40))
      ((default . error) (RIGHT_PAREN . (string_primary . 3)) (AMPERSAND . (string_primary . 3)) (COMMA . (string_primary . 3)) (SEMICOLON . (string_primary . 3)))
      ((default . error) (RIGHT_PAREN . (string_list . 0)) (AMPERSAND .  39) (COMMA . (string_list . 0)) (SEMICOLON . (string_list . 0)))
      ((default . error) (RIGHT_PAREN . (string_primary . 2)) (AMPERSAND . (string_primary . 2)) (COMMA . (string_primary . 2)) (SEMICOLON . (string_primary . 2)))
      ((default . error) (IS . (name . 0)) (RIGHT_PAREN . (name . 0)) (AMPERSAND . (name . 0)) (COMMA . (name . 0)) (DOT . (name . 0)) (QUOTE . (name . 0)) (SEMICOLON . (name . 0)))
      ((default . error) (RIGHT_PAREN . (string_primary . 1)) (AMPERSAND . (string_primary . 1)) (COMMA . (string_primary . 1)) (DOT .  38) (QUOTE . (attribute_prefix . 1)) (SEMICOLON . (string_primary . 1)))
      ((default . error) (RIGHT_PAREN . (term . 0)) (AMPERSAND . (term . 0)) (COMMA . (term . 0)) (SEMICOLON . (term . 0)))
      ((default . error) (COMMA .  36) (SEMICOLON .  37))
      ((default . error) (RIGHT_PAREN . (expression . 0)) (AMPERSAND . (expression . 0)) (COMMA . (expression . 0)) (SEMICOLON . (expression . 0)))
      ((default . error) (EXTENDS .  34) (IS .  35))
      ((default . error) (PROJECT . (project_qualifier_opt . 4)) (Wisi_EOI . (project_qualifier_opt . 4)))
      ((default . error) (STRING_LITERAL .  69))
      ((default . error) (CASE .  52) (END . (declarative_items_opt . 0)) (FOR .  53) (NULL .  54) (PACKAGE .  55) (TYPE .  56) (IDENTIFIER .  57))
      ((default . error) (EXTERNAL .  16) (EXTERNAL_AS_LIST .  17) (LEFT_PAREN .  18) (PROJECT .  19) (RIGHT_PAREN . (identifier_opt . 0)) (AMPERSAND . (identifier_opt . 0)) (COMMA . (identifier_opt . 0)) (DOT . (identifier_opt . 0)) (QUOTE . (identifier_opt . 0)) (SEMICOLON . (identifier_opt . 0)) (IDENTIFIER .  20) (STRING_LITERAL .  21))
      ((default . error) (ABSTRACT . (with_clause . 0)) (AGGREGATE . (with_clause . 0)) (CONFIGURATION . (with_clause . 0)) (LIBRARY . (with_clause . 0)) (PROJECT . (with_clause . 0)) (STANDARD . (with_clause . 0)) (WITH . (with_clause . 0)) (Wisi_EOI . (with_clause . 0)))
      ((default . error) (IDENTIFIER .  50))
      ((default . error) (EXTERNAL .  16) (EXTERNAL_AS_LIST .  17) (LEFT_PAREN .  18) (PROJECT .  19) (RIGHT_PAREN . (identifier_opt . 0)) (AMPERSAND . (identifier_opt . 0)) (COMMA . (identifier_opt . 0)) (DOT . (identifier_opt . 0)) (QUOTE . (identifier_opt . 0)) (SEMICOLON . (identifier_opt . 0)) (IDENTIFIER .  20) (STRING_LITERAL .  21))
      ((default . error) (IDENTIFIER .  48))
      ((default . error) (RIGHT_PAREN . (term . 1)) (AMPERSAND . (term . 1)) (COMMA . (term . 1)) (SEMICOLON . (term . 1)))
      ((default . error) (RIGHT_PAREN .  47) (COMMA .  36))
      ((default . error) (EXTERNAL .  16) (EXTERNAL_AS_LIST .  17) (LEFT_PAREN .  18) (PROJECT .  19) (RIGHT_PAREN . (identifier_opt . 0)) (AMPERSAND . (identifier_opt . 0)) (COMMA . (identifier_opt . 0)) (DOT . (identifier_opt . 0)) (QUOTE . (identifier_opt . 0)) (IDENTIFIER .  20) (STRING_LITERAL .  21))
      ((default . error) (RIGHT_PAREN . (external_value . 1)) (AMPERSAND . (external_value . 1)) (COMMA . (external_value . 1)) (SEMICOLON . (external_value . 1)))
      ((default . error) (RIGHT_PAREN . (external_value . 0)) (AMPERSAND . (external_value . 0)) (COMMA . (external_value . 0)) (SEMICOLON . (external_value . 0)))
      ((default . error) (Wisi_EOI . (compilation_unit . 0)))
      ((default . error) (RIGHT_PAREN . (aggregate . 0)) (AMPERSAND . (aggregate . 0)) (COMMA . (aggregate . 0)) (SEMICOLON . (aggregate . 0)))
      ((default . error) (LEFT_PAREN .  81) (RIGHT_PAREN . (attribute_reference . 0)) (AMPERSAND . (attribute_reference . 0)) (COMMA . (attribute_reference . 0)) (SEMICOLON . (attribute_reference . 0)))
      ((default . error) (RIGHT_PAREN . (expression . 1)) (AMPERSAND . (expression . 1)) (COMMA . (expression . 1)) (SEMICOLON . (expression . 1)))
      ((default . error) (IS . (name . 1)) (RIGHT_PAREN . (name . 1)) (AMPERSAND . (name . 1)) (COMMA . (name . 1)) (DOT . (name . 1)) (QUOTE . (name . 1)) (SEMICOLON . (name . 1)))
      ((default . error) (RIGHT_PAREN . (string_list . 1)) (AMPERSAND .  39) (COMMA . (string_list . 1)) (SEMICOLON . (string_list . 1)))
      ((default . error) (IS . (identifier_opt . 0)) (DOT . (identifier_opt . 0)) (IDENTIFIER .  20))
      ((default . error) (EXTERNAL .  78) (IDENTIFIER .  79))
      ((default . error) (SEMICOLON .  77))
      ((default . error) (EXTENDS . (identifier_opt . 0)) (IS . (identifier_opt . 0)) (RENAMES . (identifier_opt . 0)) (IDENTIFIER .  20))
      ((default . error) (IDENTIFIER .  75))
      ((default . error) (COLON .  73) (COLON_EQUALS .  74))
      ((default . error) (CASE . (simple_declarative_item . 2)) (END . (simple_declarative_item . 2)) (FOR . (simple_declarative_item . 2)) (NULL . (simple_declarative_item . 2)) (PACKAGE . (simple_declarative_item . 2)) (TYPE . (simple_declarative_item . 2)) (WHEN . (simple_declarative_item . 2)) (IDENTIFIER . (simple_declarative_item . 2)))
      ((default . error) (CASE . (simple_declarative_item . 3)) (END . (simple_declarative_item . 3)) (FOR . (simple_declarative_item . 3)) (NULL . (simple_declarative_item . 3)) (PACKAGE . (simple_declarative_item . 3)) (TYPE . (simple_declarative_item . 3)) (WHEN . (simple_declarative_item . 3)) (IDENTIFIER . (simple_declarative_item . 3)))
      ((default . error) (CASE . (declarative_items . 0)) (END . (declarative_items . 0)) (FOR . (declarative_items . 0)) (NULL . (declarative_items . 0)) (PACKAGE . (declarative_items . 0)) (TYPE . (declarative_items . 0)) (WHEN . (declarative_items . 0)) (IDENTIFIER . (declarative_items . 0)))
      ((default . error) (CASE .  52) (END . (declarative_items_opt . 1)) (FOR .  53) (NULL .  54) (PACKAGE .  55) (TYPE .  56) (WHEN . (declarative_items_opt . 1)) (IDENTIFIER .  57))
      ((default . error) (END .  71))
      ((default . error) (CASE . (declarative_item . 2)) (END . (declarative_item . 2)) (FOR . (declarative_item . 2)) (NULL . (declarative_item . 2)) (PACKAGE . (declarative_item . 2)) (TYPE . (declarative_item . 2)) (WHEN . (declarative_item . 2)) (IDENTIFIER . (declarative_item . 2)))
      ((default . error) (CASE . (package_declaration . 0)) (END . (package_declaration . 0)) (FOR . (package_declaration . 0)) (NULL . (package_declaration . 0)) (PACKAGE . (package_declaration . 0)) (TYPE . (package_declaration . 0)) (WHEN . (package_declaration . 0)) (IDENTIFIER . (package_declaration . 0)))
      ((default . error) (CASE . (package_declaration . 1)) (END . (package_declaration . 1)) (FOR . (package_declaration . 1)) (NULL . (package_declaration . 1)) (PACKAGE . (package_declaration . 1)) (TYPE . (package_declaration . 1)) (WHEN . (package_declaration . 1)) (IDENTIFIER . (package_declaration . 1)))
      ((default . error) (CASE . (package_declaration . 2)) (END . (package_declaration . 2)) (FOR . (package_declaration . 2)) (NULL . (package_declaration . 2)) (PACKAGE . (package_declaration . 2)) (TYPE . (package_declaration . 2)) (WHEN . (package_declaration . 2)) (IDENTIFIER . (package_declaration . 2)))
      ((default . error) (CASE . (declarative_item . 0)) (END . (declarative_item . 0)) (FOR . (declarative_item . 0)) (NULL . (declarative_item . 0)) (PACKAGE . (declarative_item . 0)) (TYPE . (declarative_item . 0)) (WHEN . (declarative_item . 0)) (IDENTIFIER . (declarative_item . 0)))
      ((default . error) (CASE . (declarative_item . 1)) (END . (declarative_item . 1)) (FOR . (declarative_item . 1)) (NULL . (declarative_item . 1)) (PACKAGE . (declarative_item . 1)) (TYPE . (declarative_item . 1)) (WHEN . (declarative_item . 1)) (IDENTIFIER . (declarative_item . 1)))
      ((default . error) (IS .  70))
      ((default . error) (CASE .  52) (END . (declarative_items_opt . 0)) (FOR .  53) (NULL .  54) (PACKAGE .  55) (TYPE .  56) (IDENTIFIER .  57))
      ((default . error) (SEMICOLON . (identifier_opt . 0)) (IDENTIFIER .  20))
      ((default . error) (CASE . (declarative_items . 1)) (END . (declarative_items . 1)) (FOR . (declarative_items . 1)) (NULL . (declarative_items . 1)) (PACKAGE . (declarative_items . 1)) (TYPE . (declarative_items . 1)) (WHEN . (declarative_items . 1)) (IDENTIFIER . (declarative_items . 1)))
      ((default . error) (IDENTIFIER .  92))
      ((default . error) (EXTERNAL .  16) (EXTERNAL_AS_LIST .  17) (LEFT_PAREN .  18) (PROJECT .  19) (AMPERSAND . (identifier_opt . 0)) (DOT . (identifier_opt . 0)) (QUOTE . (identifier_opt . 0)) (SEMICOLON . (identifier_opt . 0)) (IDENTIFIER .  20) (STRING_LITERAL .  21))
      ((default . error) (IS .  90))
      ((default . error) (EXTENDS .  87) (IS .  88) (RENAMES .  89))
      ((default . error) (CASE . (simple_declarative_item . 4)) (END . (simple_declarative_item . 4)) (FOR . (simple_declarative_item . 4)) (NULL . (simple_declarative_item . 4)) (PACKAGE . (simple_declarative_item . 4)) (TYPE . (simple_declarative_item . 4)) (WHEN . (simple_declarative_item . 4)) (IDENTIFIER . (simple_declarative_item . 4)))
      ((default . error) (LEFT_PAREN .  86))
      ((default . error) (LEFT_PAREN .  84) (USE .  85))
      ((default . error) (IS .  83) (DOT .  38))
      ((default . error) (STRING_LITERAL .  82))
      ((default . error) (RIGHT_PAREN .  111))
      ((default . error) (END . (case_items . 0)) (WHEN . ( 108 (case_items . 0))))
      ((default . error) (OTHERS .  105) (RIGHT_PAREN . (discrete_choice . 0)) (STRING_LITERAL .  106))
      ((default . error) (EXTERNAL .  16) (EXTERNAL_AS_LIST .  17) (LEFT_PAREN .  18) (PROJECT .  19) (AMPERSAND . (identifier_opt . 0)) (DOT . (identifier_opt . 0)) (QUOTE . (identifier_opt . 0)) (SEMICOLON . (identifier_opt . 0)) (IDENTIFIER .  20) (STRING_LITERAL .  21))
      ((default . error) (STRING_LITERAL .  103))
      ((default . error) (IS . (identifier_opt . 0)) (DOT . (identifier_opt . 0)) (IDENTIFIER .  20))
      ((default . error) (CASE .  52) (END . (declarative_items_opt . 0)) (FOR .  53) (NULL .  54) (PACKAGE .  55) (TYPE .  56) (IDENTIFIER .  57))
      ((default . error) (DOT . (identifier_opt . 0)) (SEMICOLON . (identifier_opt . 0)) (IDENTIFIER .  20))
      ((default . error) (LEFT_PAREN .  43))
      ((default . error) (AMPERSAND .  39) (SEMICOLON .  98))
      ((default . error) (COLON_EQUALS .  97))
      ((default . error) (SEMICOLON .  96))
      ((default . error) (END .  95))
      ((default . error) (SEMICOLON . (identifier_opt . 0)) (IDENTIFIER .  20))
      ((default . error) (Wisi_EOI . (simple_project_declaration . 0)))
      ((default . error) (EXTERNAL .  16) (EXTERNAL_AS_LIST .  17) (LEFT_PAREN .  18) (PROJECT .  19) (AMPERSAND . (identifier_opt . 0)) (DOT . (identifier_opt . 0)) (QUOTE . (identifier_opt . 0)) (SEMICOLON . (identifier_opt . 0)) (IDENTIFIER .  20) (STRING_LITERAL .  21))
      ((default . error) (CASE . (simple_declarative_item . 0)) (END . (simple_declarative_item . 0)) (FOR . (simple_declarative_item . 0)) (NULL . (simple_declarative_item . 0)) (PACKAGE . (simple_declarative_item . 0)) (TYPE . (simple_declarative_item . 0)) (WHEN . (simple_declarative_item . 0)) (IDENTIFIER . (simple_declarative_item . 0)))
      ((default . error) (SEMICOLON .  122))
      ((default . error) (DOT .  38) (SEMICOLON .  121))
      ((default . error) (END .  120))
      ((default . error) (IS .  119) (DOT .  38))
      ((default . error) (RIGHT_PAREN .  118))
      ((default . error) (AMPERSAND .  39) (SEMICOLON .  117))
      ((default . error) (RIGHT_PAREN . (discrete_choice . 2)) (EQUAL_GREATER . (discrete_choice . 2)) (VERTICAL_BAR . (discrete_choice . 2)))
      ((default . error) (RIGHT_PAREN . (discrete_choice . 1)) (EQUAL_GREATER . (discrete_choice . 1)) (VERTICAL_BAR . (discrete_choice . 1)))
      ((default . error) (RIGHT_PAREN .  116))
      ((default . error) (OTHERS .  105) (EQUAL_GREATER . (discrete_choice . 0)) (VERTICAL_BAR . (discrete_choice . 0)) (STRING_LITERAL .  106))
      ((default . error) (END . (case_items . 1)) (WHEN . (case_items . 1)))
      ((default . error) (END .  112) (WHEN .  108))
      ((default . error) (RIGHT_PAREN . (attribute_reference . 1)) (AMPERSAND . (attribute_reference . 1)) (COMMA . (attribute_reference . 1)) (SEMICOLON . (attribute_reference . 1)))
      ((default . error) (CASE .  133))
      ((default . error) (END . (case_items . 2)) (WHEN . (case_items . 2)))
      ((default . error) (EQUAL_GREATER . (discrete_choice_list . 0)) (VERTICAL_BAR . (discrete_choice_list . 0)))
      ((default . error) (EQUAL_GREATER .  131) (VERTICAL_BAR .  132))
      ((default . error) (USE .  130))
      ((default . error) (CASE . (attribute_declaration . 0)) (END . (attribute_declaration . 0)) (FOR . (attribute_declaration . 0)) (NULL . (attribute_declaration . 0)) (PACKAGE . (attribute_declaration . 0)) (TYPE . (attribute_declaration . 0)) (WHEN . (attribute_declaration . 0)) (IDENTIFIER . (attribute_declaration . 0)))
      ((default . error) (USE .  129))
      ((default . error) (CASE .  52) (END . (declarative_items_opt . 0)) (FOR .  53) (NULL .  54) (PACKAGE .  55) (TYPE .  56) (IDENTIFIER .  57))
      ((default . error) (SEMICOLON . (identifier_opt . 0)) (IDENTIFIER .  20))
      ((default . error) (CASE . (package_renaming . 0)) (END . (package_renaming . 0)) (FOR . (package_renaming . 0)) (NULL . (package_renaming . 0)) (PACKAGE . (package_renaming . 0)) (TYPE . (package_renaming . 0)) (WHEN . (package_renaming . 0)) (IDENTIFIER . (package_renaming . 0)))
      ((default . error) (CASE . (typed_string_declaration . 0)) (END . (typed_string_declaration . 0)) (FOR . (typed_string_declaration . 0)) (NULL . (typed_string_declaration . 0)) (PACKAGE . (typed_string_declaration . 0)) (TYPE . (typed_string_declaration . 0)) (WHEN . (typed_string_declaration . 0)) (IDENTIFIER . (typed_string_declaration . 0)))
      ((default . error) (AMPERSAND .  39) (SEMICOLON .  126))
      ((default . error) (SEMICOLON .  125))
      ((default . error) (Wisi_EOI . (project_extension . 0)))
      ((default . error) (CASE . (simple_declarative_item . 1)) (END . (simple_declarative_item . 1)) (FOR . (simple_declarative_item . 1)) (NULL . (simple_declarative_item . 1)) (PACKAGE . (simple_declarative_item . 1)) (TYPE . (simple_declarative_item . 1)) (WHEN . (simple_declarative_item . 1)) (IDENTIFIER . (simple_declarative_item . 1)))
      ((default . error) (SEMICOLON .  140))
      ((default . error) (END .  139))
      ((default . error) (EXTERNAL .  16) (EXTERNAL_AS_LIST .  17) (LEFT_PAREN .  18) (PROJECT .  19) (AMPERSAND . (identifier_opt . 0)) (DOT . (identifier_opt . 0)) (QUOTE . (identifier_opt . 0)) (SEMICOLON . (identifier_opt . 0)) (IDENTIFIER .  20) (STRING_LITERAL .  21))
      ((default . error) (EXTERNAL .  16) (EXTERNAL_AS_LIST .  17) (LEFT_PAREN .  18) (PROJECT .  19) (AMPERSAND . (identifier_opt . 0)) (DOT . (identifier_opt . 0)) (QUOTE . (identifier_opt . 0)) (SEMICOLON . (identifier_opt . 0)) (IDENTIFIER .  20) (STRING_LITERAL .  21))
      ((default . error) (CASE .  52) (END . (declarative_items_opt . 0)) (FOR .  53) (NULL .  54) (PACKAGE .  55) (TYPE .  56) (WHEN . (declarative_items_opt . 0)) (IDENTIFIER .  57))
      ((default . error) (OTHERS .  105) (EQUAL_GREATER . (discrete_choice . 0)) (VERTICAL_BAR . (discrete_choice . 0)) (STRING_LITERAL .  106))
      ((default . error) (SEMICOLON .  134))
      ((default . error) (CASE . (case_statement . 0)) (END . (case_statement . 0)) (FOR . (case_statement . 0)) (NULL . (case_statement . 0)) (PACKAGE . (case_statement . 0)) (TYPE . (case_statement . 0)) (WHEN . (case_statement . 0)) (IDENTIFIER . (case_statement . 0)))
      ((default . error) (EQUAL_GREATER . (discrete_choice_list . 1)) (VERTICAL_BAR . (discrete_choice_list . 1)))
      ((default . error) (END . (case_item . 0)) (WHEN . (case_item . 0)))
      ((default . error) (AMPERSAND .  39) (SEMICOLON .  143))
      ((default . error) (AMPERSAND .  39) (SEMICOLON .  142))
      ((default . error) (SEMICOLON . (identifier_opt . 0)) (IDENTIFIER .  20))
      ((default . error) (CASE . (package_spec . 0)) (END . (package_spec . 0)) (FOR . (package_spec . 0)) (NULL . (package_spec . 0)) (PACKAGE . (package_spec . 0)) (TYPE . (package_spec . 0)) (WHEN . (package_spec . 0)) (IDENTIFIER . (package_spec . 0)))
      ((default . error) (SEMICOLON .  144))
      ((default . error) (CASE . (attribute_declaration . 2)) (END . (attribute_declaration . 2)) (FOR . (attribute_declaration . 2)) (NULL . (attribute_declaration . 2)) (PACKAGE . (attribute_declaration . 2)) (TYPE . (attribute_declaration . 2)) (WHEN . (attribute_declaration . 2)) (IDENTIFIER . (attribute_declaration . 2)))
      ((default . error) (CASE . (attribute_declaration . 1)) (END . (attribute_declaration . 1)) (FOR . (attribute_declaration . 1)) (NULL . (attribute_declaration . 1)) (PACKAGE . (attribute_declaration . 1)) (TYPE . (attribute_declaration . 1)) (WHEN . (attribute_declaration . 1)) (IDENTIFIER . (attribute_declaration . 1)))
      ((default . error) (CASE . (package_extension . 0)) (END . (package_extension . 0)) (FOR . (package_extension . 0)) (NULL . (package_extension . 0)) (PACKAGE . (package_extension . 0)) (TYPE . (package_extension . 0)) (WHEN . (package_extension . 0)) (IDENTIFIER . (package_extension . 0)))]
     [((compilation_unit . 8)(context_clause . 9)(context_clause_opt . 10)(project_extension . 11)(simple_project_declaration . 12)(with_clause . 13))
      nil
      nil
      nil
      nil
      ((identifier_opt . 32))
      nil
      ((aggregate . 22)(attribute_prefix . 23)(attribute_reference . 24)(expression . 25)(external_value . 26)(identifier_opt . 27)(name . 28)(string_primary . 29)(string_list . 30)(term . 31))
      nil
      ((with_clause . 15))
      ((project_qualifier_opt . 14))
      nil
      nil
      nil
      ((project_declaration_opt . 46)(project_extension . 11)(simple_project_declaration . 12))
      nil
      ((aggregate . 45))
      ((aggregate . 44))
      ((aggregate . 22)(attribute_prefix . 23)(attribute_reference . 24)(expression . 25)(external_value . 26)(identifier_opt . 27)(name . 28)(string_primary . 29)(string_list . 42)(term . 31))
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      ((attribute_declaration . 58)(case_statement . 59)(declarative_item . 60)(declarative_items . 61)(declarative_items_opt . 62)(package_declaration . 63)(package_spec . 64)(package_extension . 65)(package_renaming . 66)(simple_declarative_item . 67)(typed_string_declaration . 68))
      ((aggregate . 22)(attribute_prefix . 23)(attribute_reference . 24)(expression . 51)(external_value . 26)(identifier_opt . 27)(name . 28)(string_primary . 29)(term . 31))
      nil
      nil
      ((aggregate . 22)(attribute_prefix . 23)(attribute_reference . 24)(external_value . 26)(identifier_opt . 27)(name . 28)(string_primary . 29)(term . 49))
      nil
      nil
      nil
      ((aggregate . 22)(attribute_prefix . 23)(attribute_reference . 24)(expression . 25)(external_value . 26)(identifier_opt . 27)(name . 28)(string_primary . 29)(string_list . 42)(term . 31))
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      ((identifier_opt . 27)(name . 80))
      nil
      nil
      ((identifier_opt . 76))
      nil
      nil
      nil
      nil
      nil
      ((attribute_declaration . 58)(case_statement . 59)(declarative_item . 72)(package_declaration . 63)(package_spec . 64)(package_extension . 65)(package_renaming . 66)(simple_declarative_item . 67)(typed_string_declaration . 68))
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      ((attribute_declaration . 58)(case_statement . 59)(declarative_item . 60)(declarative_items . 61)(declarative_items_opt . 94)(package_declaration . 63)(package_spec . 64)(package_extension . 65)(package_renaming . 66)(simple_declarative_item . 67)(typed_string_declaration . 68))
      ((identifier_opt . 93))
      nil
      nil
      ((aggregate . 22)(attribute_prefix . 23)(attribute_reference . 24)(expression . 91)(external_value . 26)(identifier_opt . 27)(name . 28)(string_primary . 29)(term . 31))
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      ((case_item . 109)(case_items . 110))
      ((discrete_choice . 107))
      ((aggregate . 22)(attribute_prefix . 23)(attribute_reference . 24)(expression . 104)(external_value . 26)(identifier_opt . 27)(name . 28)(string_primary . 29)(term . 31))
      nil
      ((identifier_opt . 27)(name . 102))
      ((attribute_declaration . 58)(case_statement . 59)(declarative_item . 60)(declarative_items . 61)(declarative_items_opt . 101)(package_declaration . 63)(package_spec . 64)(package_extension . 65)(package_renaming . 66)(simple_declarative_item . 67)(typed_string_declaration . 68))
      ((identifier_opt . 27)(name . 100))
      ((aggregate . 99))
      nil
      nil
      nil
      nil
      ((identifier_opt . 124))
      nil
      ((aggregate . 22)(attribute_prefix . 23)(attribute_reference . 24)(expression . 123)(external_value . 26)(identifier_opt . 27)(name . 28)(string_primary . 29)(term . 31))
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      ((discrete_choice . 114)(discrete_choice_list . 115))
      nil
      ((case_item . 113))
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      ((attribute_declaration . 58)(case_statement . 59)(declarative_item . 60)(declarative_items . 61)(declarative_items_opt . 128)(package_declaration . 63)(package_spec . 64)(package_extension . 65)(package_renaming . 66)(simple_declarative_item . 67)(typed_string_declaration . 68))
      ((identifier_opt . 127))
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      ((aggregate . 22)(attribute_prefix . 23)(attribute_reference . 24)(expression . 138)(external_value . 26)(identifier_opt . 27)(name . 28)(string_primary . 29)(term . 31))
      ((aggregate . 22)(attribute_prefix . 23)(attribute_reference . 24)(expression . 137)(external_value . 26)(identifier_opt . 27)(name . 28)(string_primary . 29)(term . 31))
      ((attribute_declaration . 58)(case_statement . 59)(declarative_item . 60)(declarative_items . 61)(declarative_items_opt . 136)(package_declaration . 63)(package_spec . 64)(package_extension . 65)(package_renaming . 66)(simple_declarative_item . 67)(typed_string_declaration . 68))
      ((discrete_choice . 135))
      nil
      nil
      nil
      nil
      nil
      nil
      ((identifier_opt . 141))
      nil
      nil
      nil
      nil
      nil]))
  "Parser table.")

(provide 'gpr-grammar-elisp)

;; end of file
