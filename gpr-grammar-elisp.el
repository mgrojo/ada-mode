;;; gpr-grammar-elisp.el --- Generated parser support file  -*- lexical-binding:t -*-
;;; with command line: wisi-generate.exe -v 1 gpr-grammar.wy LR1 Elisp

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
      (IDENTIFIER)
     )
     ("string-double"
      (STRING_LITERAL)
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
     [((default . error) (ABSTRACT . (context_clause_opt . 0)) (AGGREGATE . (context_clause_opt . 0)) (CONFIGURATION . (context_clause_opt . 0)) (LIBRARY . (context_clause_opt . 0)) (PROJECT . (context_clause_opt . 0)) (STANDARD . (context_clause_opt . 0)) (WITH .  1) (Wisi_EOI . (context_clause_opt . 0)))
      ((default . error) (EXTERNAL .  13) (EXTERNAL_AS_LIST .  14) (LEFT_PAREN .  15) (PROJECT .  16) (AMPERSAND . (identifier_opt . 0)) (COMMA . (identifier_opt . 0)) (DOT . (identifier_opt . 0)) (QUOTE . (identifier_opt . 0)) (SEMICOLON . (identifier_opt . 0)) (IDENTIFIER .  17) (STRING_LITERAL .  18))
      ((default . error) (Wisi_EOI . accept))
      ((default . error) (ABSTRACT . (context_clause_opt . 1)) (AGGREGATE . (context_clause_opt . 1)) (CONFIGURATION . (context_clause_opt . 1)) (LIBRARY . (context_clause_opt . 1)) (PROJECT . (context_clause_opt . 1)) (STANDARD . (context_clause_opt . 1)) (WITH .  1) (Wisi_EOI . (context_clause_opt . 1)))
      ((default . error) (ABSTRACT .  6) (AGGREGATE .  7) (CONFIGURATION .  8) (LIBRARY .  9) (PROJECT . (project_qualifier_opt . 0)) (STANDARD .  10) (Wisi_EOI . (project_qualifier_opt . 0)))
      ((default . error) (ABSTRACT . (context_clause . 0)) (AGGREGATE . (context_clause . 0)) (CONFIGURATION . (context_clause . 0)) (LIBRARY . (context_clause . 0)) (PROJECT . (context_clause . 0)) (STANDARD . (context_clause . 0)) (WITH . (context_clause . 0)) (Wisi_EOI . (context_clause . 0)))
      ((default . error) (PROJECT . (project_qualifier_opt . 1)) (Wisi_EOI . (project_qualifier_opt . 1)))
      ((default . error) (LIBRARY .  57) (PROJECT . (project_qualifier_opt . 3)) (Wisi_EOI . (project_qualifier_opt . 3)))
      ((default . error) (PROJECT . (project_qualifier_opt . 6)) (Wisi_EOI . (project_qualifier_opt . 6)))
      ((default . error) (PROJECT . (project_qualifier_opt . 5)) (Wisi_EOI . (project_qualifier_opt . 5)))
      ((default . error) (PROJECT . (project_qualifier_opt . 2)) (Wisi_EOI . (project_qualifier_opt . 2)))
      ((default . error) (PROJECT .  53) (Wisi_EOI . (project_declaration_opt . 0)))
      ((default . error) (ABSTRACT . (context_clause . 1)) (AGGREGATE . (context_clause . 1)) (CONFIGURATION . (context_clause . 1)) (LIBRARY . (context_clause . 1)) (PROJECT . (context_clause . 1)) (STANDARD . (context_clause . 1)) (WITH . (context_clause . 1)) (Wisi_EOI . (context_clause . 1)))
      ((default . error) (LEFT_PAREN .  50))
      ((default . error) (LEFT_PAREN .  50))
      ((default . error) (EXTERNAL .  34) (EXTERNAL_AS_LIST .  35) (LEFT_PAREN .  36) (PROJECT .  16) (RIGHT_PAREN . ( 37 (identifier_opt . 0))) (AMPERSAND . (identifier_opt . 0)) (COMMA . (identifier_opt . 0)) (DOT . (identifier_opt . 0)) (QUOTE . (identifier_opt . 0)) (IDENTIFIER .  38) (STRING_LITERAL .  39))
      ((default . error) (QUOTE . (attribute_prefix . 0)))
      ((default . error) (AMPERSAND . (identifier_opt . 1)) (COMMA . (identifier_opt . 1)) (DOT . (identifier_opt . 1)) (QUOTE . (identifier_opt . 1)) (SEMICOLON . (identifier_opt . 1)))
      ((default . error) (AMPERSAND . (string_primary . 0)) (COMMA . (string_primary . 0)) (SEMICOLON . (string_primary . 0)))
      ((default . error) (AMPERSAND . (term . 2)) (COMMA . (term . 2)) (SEMICOLON . (term . 2)))
      ((default . error) (QUOTE .  33))
      ((default . error) (AMPERSAND . (string_primary . 3)) (COMMA . (string_primary . 3)) (SEMICOLON . (string_primary . 3)))
      ((default . error) (AMPERSAND .  32) (COMMA . (string_list . 0)) (SEMICOLON . (string_list . 0)))
      ((default . error) (AMPERSAND . (string_primary . 2)) (COMMA . (string_primary . 2)) (SEMICOLON . (string_primary . 2)))
      ((default . error) (AMPERSAND . (name . 0)) (COMMA . (name . 0)) (DOT . (name . 0)) (QUOTE . (name . 0)) (SEMICOLON . (name . 0)))
      ((default . error) (AMPERSAND . (string_primary . 1)) (COMMA . (string_primary . 1)) (DOT .  31) (QUOTE . (attribute_prefix . 1)) (SEMICOLON . (string_primary . 1)))
      ((default . error) (AMPERSAND . (term . 0)) (COMMA . (term . 0)) (SEMICOLON . (term . 0)))
      ((default . error) (COMMA .  29) (SEMICOLON .  30))
      ((default . error) (AMPERSAND . (expression . 0)) (COMMA . (expression . 0)) (SEMICOLON . (expression . 0)))
      ((default . error) (EXTERNAL .  13) (EXTERNAL_AS_LIST .  14) (LEFT_PAREN .  15) (PROJECT .  16) (AMPERSAND . (identifier_opt . 0)) (COMMA . (identifier_opt . 0)) (DOT . (identifier_opt . 0)) (QUOTE . (identifier_opt . 0)) (SEMICOLON . (identifier_opt . 0)) (IDENTIFIER .  17) (STRING_LITERAL .  18))
      ((default . error) (ABSTRACT . (with_clause . 0)) (AGGREGATE . (with_clause . 0)) (CONFIGURATION . (with_clause . 0)) (LIBRARY . (with_clause . 0)) (PROJECT . (with_clause . 0)) (STANDARD . (with_clause . 0)) (WITH . (with_clause . 0)) (Wisi_EOI . (with_clause . 0)))
      ((default . error) (IDENTIFIER .  72))
      ((default . error) (EXTERNAL .  13) (EXTERNAL_AS_LIST .  14) (LEFT_PAREN .  15) (PROJECT .  16) (AMPERSAND . (identifier_opt . 0)) (COMMA . (identifier_opt . 0)) (DOT . (identifier_opt . 0)) (QUOTE . (identifier_opt . 0)) (SEMICOLON . (identifier_opt . 0)) (IDENTIFIER .  17) (STRING_LITERAL .  18))
      ((default . error) (IDENTIFIER .  70))
      ((default . error) (LEFT_PAREN .  67))
      ((default . error) (LEFT_PAREN .  67))
      ((default . error) (EXTERNAL .  34) (EXTERNAL_AS_LIST .  35) (LEFT_PAREN .  36) (PROJECT .  16) (RIGHT_PAREN . ( 65 (identifier_opt . 0))) (AMPERSAND . (identifier_opt . 0)) (COMMA . (identifier_opt . 0)) (DOT . (identifier_opt . 0)) (QUOTE . (identifier_opt . 0)) (IDENTIFIER .  38) (STRING_LITERAL .  39))
      ((default . error) (AMPERSAND . (term . 1)) (COMMA . (term . 1)) (SEMICOLON . (term . 1)))
      ((default . error) (RIGHT_PAREN . (identifier_opt . 1)) (AMPERSAND . (identifier_opt . 1)) (COMMA . (identifier_opt . 1)) (DOT . (identifier_opt . 1)) (QUOTE . (identifier_opt . 1)))
      ((default . error) (RIGHT_PAREN . (string_primary . 0)) (AMPERSAND . (string_primary . 0)) (COMMA . (string_primary . 0)))
      ((default . error) (RIGHT_PAREN . (term . 2)) (AMPERSAND . (term . 2)) (COMMA . (term . 2)))
      ((default . error) (QUOTE .  64))
      ((default . error) (RIGHT_PAREN . (string_primary . 3)) (AMPERSAND . (string_primary . 3)) (COMMA . (string_primary . 3)))
      ((default . error) (RIGHT_PAREN . (string_list . 0)) (AMPERSAND .  63) (COMMA . (string_list . 0)))
      ((default . error) (RIGHT_PAREN . (string_primary . 2)) (AMPERSAND . (string_primary . 2)) (COMMA . (string_primary . 2)))
      ((default . error) (RIGHT_PAREN . (name . 0)) (AMPERSAND . (name . 0)) (COMMA . (name . 0)) (DOT . (name . 0)) (QUOTE . (name . 0)))
      ((default . error) (RIGHT_PAREN . (string_primary . 1)) (AMPERSAND . (string_primary . 1)) (COMMA . (string_primary . 1)) (DOT .  62) (QUOTE . (attribute_prefix . 1)))
      ((default . error) (RIGHT_PAREN . (term . 0)) (AMPERSAND . (term . 0)) (COMMA . (term . 0)))
      ((default . error) (RIGHT_PAREN .  60) (COMMA .  61))
      ((default . error) (RIGHT_PAREN . (expression . 0)) (AMPERSAND . (expression . 0)) (COMMA . (expression . 0)))
      ((default . error) (EXTERNAL .  34) (EXTERNAL_AS_LIST .  35) (LEFT_PAREN .  36) (PROJECT .  16) (RIGHT_PAREN . (identifier_opt . 0)) (AMPERSAND . (identifier_opt . 0)) (COMMA . (identifier_opt . 0)) (DOT . (identifier_opt . 0)) (QUOTE . (identifier_opt . 0)) (IDENTIFIER .  38) (STRING_LITERAL .  39))
      ((default . error) (AMPERSAND . (external_value . 1)) (COMMA . (external_value . 1)) (SEMICOLON . (external_value . 1)))
      ((default . error) (AMPERSAND . (external_value . 0)) (COMMA . (external_value . 0)) (SEMICOLON . (external_value . 0)))
      ((default . error) (EXTENDS . (identifier_opt . 0)) (IS . (identifier_opt . 0)) (IDENTIFIER .  58))
      ((default . error) (Wisi_EOI . (compilation_unit . 0)))
      ((default . error) (Wisi_EOI . (project_declaration_opt . 2)))
      ((default . error) (Wisi_EOI . (project_declaration_opt . 1)))
      ((default . error) (PROJECT . (project_qualifier_opt . 4)) (Wisi_EOI . (project_qualifier_opt . 4)))
      ((default . error) (EXTENDS . (identifier_opt . 1)) (IS . (identifier_opt . 1)))
      ((default . error) (EXTENDS .  80) (IS .  81))
      ((default . error) (AMPERSAND . (aggregate . 0)) (COMMA . (aggregate . 0)) (SEMICOLON . (aggregate . 0)))
      ((default . error) (EXTERNAL .  34) (EXTERNAL_AS_LIST .  35) (LEFT_PAREN .  36) (PROJECT .  16) (RIGHT_PAREN . (identifier_opt . 0)) (AMPERSAND . (identifier_opt . 0)) (COMMA . (identifier_opt . 0)) (DOT . (identifier_opt . 0)) (QUOTE . (identifier_opt . 0)) (IDENTIFIER .  38) (STRING_LITERAL .  39))
      ((default . error) (IDENTIFIER .  78))
      ((default . error) (EXTERNAL .  34) (EXTERNAL_AS_LIST .  35) (LEFT_PAREN .  36) (PROJECT .  16) (RIGHT_PAREN . (identifier_opt . 0)) (AMPERSAND . (identifier_opt . 0)) (COMMA . (identifier_opt . 0)) (DOT . (identifier_opt . 0)) (QUOTE . (identifier_opt . 0)) (IDENTIFIER .  38) (STRING_LITERAL .  39))
      ((default . error) (IDENTIFIER .  76))
      ((default . error) (RIGHT_PAREN . (term . 1)) (AMPERSAND . (term . 1)) (COMMA . (term . 1)))
      ((default . error) (RIGHT_PAREN .  75) (COMMA .  61))
      ((default . error) (EXTERNAL .  34) (EXTERNAL_AS_LIST .  35) (LEFT_PAREN .  36) (PROJECT .  16) (RIGHT_PAREN . (identifier_opt . 0)) (AMPERSAND . (identifier_opt . 0)) (COMMA . (identifier_opt . 0)) (DOT . (identifier_opt . 0)) (QUOTE . (identifier_opt . 0)) (IDENTIFIER .  38) (STRING_LITERAL .  39))
      ((default . error) (RIGHT_PAREN . (external_value . 1)) (AMPERSAND . (external_value . 1)) (COMMA . (external_value . 1)))
      ((default . error) (RIGHT_PAREN . (external_value . 0)) (AMPERSAND . (external_value . 0)) (COMMA . (external_value . 0)))
      ((default . error) (LEFT_PAREN .  74) (AMPERSAND . (attribute_reference . 0)) (COMMA . (attribute_reference . 0)) (SEMICOLON . (attribute_reference . 0)))
      ((default . error) (AMPERSAND . (expression . 1)) (COMMA . (expression . 1)) (SEMICOLON . (expression . 1)))
      ((default . error) (AMPERSAND . (name . 1)) (COMMA . (name . 1)) (DOT . (name . 1)) (QUOTE . (name . 1)) (SEMICOLON . (name . 1)))
      ((default . error) (AMPERSAND .  32) (COMMA . (string_list . 1)) (SEMICOLON . (string_list . 1)))
      ((default . error) (STRING_LITERAL .  101))
      ((default . error) (RIGHT_PAREN . (aggregate . 0)) (AMPERSAND . (aggregate . 0)) (COMMA . (aggregate . 0)))
      ((default . error) (LEFT_PAREN .  100) (RIGHT_PAREN . (attribute_reference . 0)) (AMPERSAND . (attribute_reference . 0)) (COMMA . (attribute_reference . 0)))
      ((default . error) (RIGHT_PAREN . (expression . 1)) (AMPERSAND . (expression . 1)) (COMMA . (expression . 1)))
      ((default . error) (RIGHT_PAREN . (name . 1)) (AMPERSAND . (name . 1)) (COMMA . (name . 1)) (DOT . (name . 1)) (QUOTE . (name . 1)))
      ((default . error) (RIGHT_PAREN . (string_list . 1)) (AMPERSAND .  63) (COMMA . (string_list . 1)))
      ((default . error) (STRING_LITERAL .  99))
      ((default . error) (CASE .  82) (END . (declarative_items_opt . 0)) (FOR .  83) (NULL .  84) (PACKAGE .  85) (TYPE .  86) (IDENTIFIER .  87))
      ((default . error) (IS . (identifier_opt . 0)) (DOT . (identifier_opt . 0)) (IDENTIFIER .  115))
      ((default . error) (EXTERNAL .  113) (IDENTIFIER .  114))
      ((default . error) (SEMICOLON .  112))
      ((default . error) (EXTENDS . (identifier_opt . 0)) (IS . (identifier_opt . 0)) (RENAMES . (identifier_opt . 0)) (IDENTIFIER .  110))
      ((default . error) (IDENTIFIER .  109))
      ((default . error) (COLON .  107) (COLON_EQUALS .  108))
      ((default . error) (CASE . (simple_declarative_item . 2)) (END . (simple_declarative_item . 2)) (FOR . (simple_declarative_item . 2)) (NULL . (simple_declarative_item . 2)) (PACKAGE . (simple_declarative_item . 2)) (TYPE . (simple_declarative_item . 2)) (IDENTIFIER . (simple_declarative_item . 2)))
      ((default . error) (CASE . (simple_declarative_item . 3)) (END . (simple_declarative_item . 3)) (FOR . (simple_declarative_item . 3)) (NULL . (simple_declarative_item . 3)) (PACKAGE . (simple_declarative_item . 3)) (TYPE . (simple_declarative_item . 3)) (IDENTIFIER . (simple_declarative_item . 3)))
      ((default . error) (CASE . (declarative_items . 0)) (END . (declarative_items . 0)) (FOR . (declarative_items . 0)) (NULL . (declarative_items . 0)) (PACKAGE . (declarative_items . 0)) (TYPE . (declarative_items . 0)) (IDENTIFIER . (declarative_items . 0)))
      ((default . error) (CASE .  82) (END . (declarative_items_opt . 1)) (FOR .  83) (NULL .  84) (PACKAGE .  85) (TYPE .  86) (IDENTIFIER .  87))
      ((default . error) (END .  105))
      ((default . error) (CASE . (declarative_item . 2)) (END . (declarative_item . 2)) (FOR . (declarative_item . 2)) (NULL . (declarative_item . 2)) (PACKAGE . (declarative_item . 2)) (TYPE . (declarative_item . 2)) (IDENTIFIER . (declarative_item . 2)))
      ((default . error) (CASE . (package_declaration . 0)) (END . (package_declaration . 0)) (FOR . (package_declaration . 0)) (NULL . (package_declaration . 0)) (PACKAGE . (package_declaration . 0)) (TYPE . (package_declaration . 0)) (IDENTIFIER . (package_declaration . 0)))
      ((default . error) (CASE . (package_declaration . 1)) (END . (package_declaration . 1)) (FOR . (package_declaration . 1)) (NULL . (package_declaration . 1)) (PACKAGE . (package_declaration . 1)) (TYPE . (package_declaration . 1)) (IDENTIFIER . (package_declaration . 1)))
      ((default . error) (CASE . (package_declaration . 2)) (END . (package_declaration . 2)) (FOR . (package_declaration . 2)) (NULL . (package_declaration . 2)) (PACKAGE . (package_declaration . 2)) (TYPE . (package_declaration . 2)) (IDENTIFIER . (package_declaration . 2)))
      ((default . error) (CASE . (declarative_item . 0)) (END . (declarative_item . 0)) (FOR . (declarative_item . 0)) (NULL . (declarative_item . 0)) (PACKAGE . (declarative_item . 0)) (TYPE . (declarative_item . 0)) (IDENTIFIER . (declarative_item . 0)))
      ((default . error) (CASE . (declarative_item . 1)) (END . (declarative_item . 1)) (FOR . (declarative_item . 1)) (NULL . (declarative_item . 1)) (PACKAGE . (declarative_item . 1)) (TYPE . (declarative_item . 1)) (IDENTIFIER . (declarative_item . 1)))
      ((default . error) (IS .  104))
      ((default . error) (STRING_LITERAL .  103))
      ((default . error) (RIGHT_PAREN .  102))
      ((default . error) (AMPERSAND . (attribute_reference . 1)) (COMMA . (attribute_reference . 1)) (SEMICOLON . (attribute_reference . 1)))
      ((default . error) (RIGHT_PAREN .  145))
      ((default . error) (CASE .  82) (END . (declarative_items_opt . 0)) (FOR .  83) (NULL .  84) (PACKAGE .  85) (TYPE .  86) (IDENTIFIER .  87))
      ((default . error) (SEMICOLON . (identifier_opt . 0)) (IDENTIFIER .  142))
      ((default . error) (CASE . (declarative_items . 1)) (END . (declarative_items . 1)) (FOR . (declarative_items . 1)) (NULL . (declarative_items . 1)) (PACKAGE . (declarative_items . 1)) (TYPE . (declarative_items . 1)) (IDENTIFIER . (declarative_items . 1)))
      ((default . error) (IDENTIFIER .  141))
      ((default . error) (EXTERNAL .  127) (EXTERNAL_AS_LIST .  128) (LEFT_PAREN .  129) (PROJECT .  16) (AMPERSAND . (identifier_opt . 0)) (DOT . (identifier_opt . 0)) (QUOTE . (identifier_opt . 0)) (SEMICOLON . (identifier_opt . 0)) (IDENTIFIER .  130) (STRING_LITERAL .  131))
      ((default . error) (IS .  126))
      ((default . error) (EXTENDS . (identifier_opt . 1)) (IS . (identifier_opt . 1)) (RENAMES . (identifier_opt . 1)))
      ((default . error) (EXTENDS .  123) (IS .  124) (RENAMES .  125))
      ((default . error) (CASE . (simple_declarative_item . 4)) (END . (simple_declarative_item . 4)) (FOR . (simple_declarative_item . 4)) (NULL . (simple_declarative_item . 4)) (PACKAGE . (simple_declarative_item . 4)) (TYPE . (simple_declarative_item . 4)) (IDENTIFIER . (simple_declarative_item . 4)))
      ((default . error) (LEFT_PAREN .  122))
      ((default . error) (LEFT_PAREN .  120) (USE .  121))
      ((default . error) (IS . (identifier_opt . 1)) (DOT . (identifier_opt . 1)))
      ((default . error) (IS . (name . 0)) (DOT . (name . 0)))
      ((default . error) (IS .  118) (DOT .  119))
      ((default . error) (END . (case_items . 0)) (WHEN . ( 171 (case_items . 0))))
      ((default . error) (IDENTIFIER .  170))
      ((default . error) (OTHERS .  167) (RIGHT_PAREN . (discrete_choice . 0)) (STRING_LITERAL .  168))
      ((default . error) (EXTERNAL .  127) (EXTERNAL_AS_LIST .  128) (LEFT_PAREN .  129) (PROJECT .  16) (AMPERSAND . (identifier_opt . 0)) (DOT . (identifier_opt . 0)) (QUOTE . (identifier_opt . 0)) (SEMICOLON . (identifier_opt . 0)) (IDENTIFIER .  130) (STRING_LITERAL .  131))
      ((default . error) (STRING_LITERAL .  165))
      ((default . error) (IS . (identifier_opt . 0)) (DOT . (identifier_opt . 0)) (IDENTIFIER .  115))
      ((default . error) (CASE .  82) (END . (declarative_items_opt . 0)) (FOR .  83) (NULL .  84) (PACKAGE .  85) (TYPE .  86) (IDENTIFIER .  87))
      ((default . error) (DOT . (identifier_opt . 0)) (SEMICOLON . (identifier_opt . 0)) (IDENTIFIER .  160))
      ((default . error) (LEFT_PAREN .  158))
      ((default . error) (LEFT_PAREN .  155))
      ((default . error) (LEFT_PAREN .  155))
      ((default . error) (EXTERNAL .  34) (EXTERNAL_AS_LIST .  35) (LEFT_PAREN .  36) (PROJECT .  16) (RIGHT_PAREN . ( 153 (identifier_opt . 0))) (AMPERSAND . (identifier_opt . 0)) (COMMA . (identifier_opt . 0)) (DOT . (identifier_opt . 0)) (QUOTE . (identifier_opt . 0)) (IDENTIFIER .  38) (STRING_LITERAL .  39))
      ((default . error) (AMPERSAND . (identifier_opt . 1)) (DOT . (identifier_opt . 1)) (QUOTE . (identifier_opt . 1)) (SEMICOLON . (identifier_opt . 1)))
      ((default . error) (AMPERSAND . (string_primary . 0)) (SEMICOLON . (string_primary . 0)))
      ((default . error) (AMPERSAND . (term . 2)) (SEMICOLON . (term . 2)))
      ((default . error) (QUOTE .  152))
      ((default . error) (AMPERSAND . (string_primary . 3)) (SEMICOLON . (string_primary . 3)))
      ((default . error) (AMPERSAND .  150) (SEMICOLON .  151))
      ((default . error) (AMPERSAND . (string_primary . 2)) (SEMICOLON . (string_primary . 2)))
      ((default . error) (AMPERSAND . (name . 0)) (DOT . (name . 0)) (QUOTE . (name . 0)) (SEMICOLON . (name . 0)))
      ((default . error) (AMPERSAND . (string_primary . 1)) (DOT .  149) (QUOTE . (attribute_prefix . 1)) (SEMICOLON . (string_primary . 1)))
      ((default . error) (AMPERSAND . (term . 0)) (SEMICOLON . (term . 0)))
      ((default . error) (AMPERSAND . (expression . 0)) (SEMICOLON . (expression . 0)))
      ((default . error) (COLON_EQUALS .  148))
      ((default . error) (SEMICOLON . (identifier_opt . 1)))
      ((default . error) (SEMICOLON .  147))
      ((default . error) (END .  146))
      ((default . error) (RIGHT_PAREN . (attribute_reference . 1)) (AMPERSAND . (attribute_reference . 1)) (COMMA . (attribute_reference . 1)))
      ((default . error) (SEMICOLON . (identifier_opt . 0)) (IDENTIFIER .  142))
      ((default . error) (Wisi_EOI . (simple_project_declaration . 0)))
      ((default . error) (EXTERNAL .  127) (EXTERNAL_AS_LIST .  128) (LEFT_PAREN .  129) (PROJECT .  16) (AMPERSAND . (identifier_opt . 0)) (DOT . (identifier_opt . 0)) (QUOTE . (identifier_opt . 0)) (SEMICOLON . (identifier_opt . 0)) (IDENTIFIER .  130) (STRING_LITERAL .  131))
      ((default . error) (IDENTIFIER .  192))
      ((default . error) (EXTERNAL .  127) (EXTERNAL_AS_LIST .  128) (LEFT_PAREN .  129) (PROJECT .  16) (AMPERSAND . (identifier_opt . 0)) (DOT . (identifier_opt . 0)) (QUOTE . (identifier_opt . 0)) (SEMICOLON . (identifier_opt . 0)) (IDENTIFIER .  130) (STRING_LITERAL .  131))
      ((default . error) (CASE . (simple_declarative_item . 0)) (END . (simple_declarative_item . 0)) (FOR . (simple_declarative_item . 0)) (NULL . (simple_declarative_item . 0)) (PACKAGE . (simple_declarative_item . 0)) (TYPE . (simple_declarative_item . 0)) (IDENTIFIER . (simple_declarative_item . 0)))
      ((default . error) (IDENTIFIER .  190))
      ((default . error) (AMPERSAND . (term . 1)) (SEMICOLON . (term . 1)))
      ((default . error) (RIGHT_PAREN .  189) (COMMA .  61))
      ((default . error) (EXTERNAL .  34) (EXTERNAL_AS_LIST .  35) (LEFT_PAREN .  36) (PROJECT .  16) (RIGHT_PAREN . (identifier_opt . 0)) (AMPERSAND . (identifier_opt . 0)) (COMMA . (identifier_opt . 0)) (DOT . (identifier_opt . 0)) (QUOTE . (identifier_opt . 0)) (IDENTIFIER .  38) (STRING_LITERAL .  39))
      ((default . error) (AMPERSAND . (external_value . 1)) (SEMICOLON . (external_value . 1)))
      ((default . error) (AMPERSAND . (external_value . 0)) (SEMICOLON . (external_value . 0)))
      ((default . error) (EXTERNAL .  34) (EXTERNAL_AS_LIST .  35) (LEFT_PAREN .  36) (PROJECT .  16) (RIGHT_PAREN . (identifier_opt . 0)) (AMPERSAND . (identifier_opt . 0)) (COMMA . (identifier_opt . 0)) (DOT . (identifier_opt . 0)) (QUOTE . (identifier_opt . 0)) (IDENTIFIER .  38) (STRING_LITERAL .  39))
      ((default . error) (SEMICOLON .  187))
      ((default . error) (DOT . (identifier_opt . 1)) (SEMICOLON . (identifier_opt . 1)))
      ((default . error) (DOT . (name . 0)) (SEMICOLON . (name . 0)))
      ((default . error) (DOT .  185) (SEMICOLON .  186))
      ((default . error) (END .  184))
      ((default . error) (IS .  183) (DOT .  119))
      ((default . error) (RIGHT_PAREN .  182))
      ((default . error) (AMPERSAND .  150) (SEMICOLON .  181))
      ((default . error) (RIGHT_PAREN . (discrete_choice . 2)))
      ((default . error) (RIGHT_PAREN . (discrete_choice . 1)))
      ((default . error) (RIGHT_PAREN .  180))
      ((default . error) (IS . (name . 1)) (DOT . (name . 1)))
      ((default . error) (OTHERS .  176) (EQUAL_GREATER . (discrete_choice . 0)) (VERTICAL_BAR . (discrete_choice . 0)) (STRING_LITERAL .  177))
      ((default . error) (END . (case_items . 1)) (WHEN . (case_items . 1)))
      ((default . error) (END .  174) (WHEN .  171))
      ((default . error) (CASE .  206))
      ((default . error) (END . (case_items . 2)) (WHEN . (case_items . 2)))
      ((default . error) (EQUAL_GREATER . (discrete_choice . 2)) (VERTICAL_BAR . (discrete_choice . 2)))
      ((default . error) (EQUAL_GREATER . (discrete_choice . 1)) (VERTICAL_BAR . (discrete_choice . 1)))
      ((default . error) (EQUAL_GREATER . (discrete_choice_list . 0)) (VERTICAL_BAR . (discrete_choice_list . 0)))
      ((default . error) (EQUAL_GREATER .  204) (VERTICAL_BAR .  205))
      ((default . error) (USE .  203))
      ((default . error) (CASE . (attribute_declaration . 0)) (END . (attribute_declaration . 0)) (FOR . (attribute_declaration . 0)) (NULL . (attribute_declaration . 0)) (PACKAGE . (attribute_declaration . 0)) (TYPE . (attribute_declaration . 0)) (IDENTIFIER . (attribute_declaration . 0)))
      ((default . error) (USE .  202))
      ((default . error) (CASE .  82) (END . (declarative_items_opt . 0)) (FOR .  83) (NULL .  84) (PACKAGE .  85) (TYPE .  86) (IDENTIFIER .  87))
      ((default . error) (SEMICOLON . (identifier_opt . 0)) (IDENTIFIER .  142))
      ((default . error) (IDENTIFIER .  199))
      ((default . error) (CASE . (package_renaming . 0)) (END . (package_renaming . 0)) (FOR . (package_renaming . 0)) (NULL . (package_renaming . 0)) (PACKAGE . (package_renaming . 0)) (TYPE . (package_renaming . 0)) (IDENTIFIER . (package_renaming . 0)))
      ((default . error) (CASE . (typed_string_declaration . 0)) (END . (typed_string_declaration . 0)) (FOR . (typed_string_declaration . 0)) (NULL . (typed_string_declaration . 0)) (PACKAGE . (typed_string_declaration . 0)) (TYPE . (typed_string_declaration . 0)) (IDENTIFIER . (typed_string_declaration . 0)))
      ((default . error) (RIGHT_PAREN .  198) (COMMA .  61))
      ((default . error) (AMPERSAND . (aggregate . 0)) (SEMICOLON . (aggregate . 0)))
      ((default . error) (LEFT_PAREN .  197) (AMPERSAND . (attribute_reference . 0)) (SEMICOLON . (attribute_reference . 0)))
      ((default . error) (AMPERSAND . (expression . 1)) (SEMICOLON . (expression . 1)))
      ((default . error) (AMPERSAND . (name . 1)) (DOT . (name . 1)) (QUOTE . (name . 1)) (SEMICOLON . (name . 1)))
      ((default . error) (AMPERSAND .  150) (SEMICOLON .  196))
      ((default . error) (SEMICOLON .  195))
      ((default . error) (Wisi_EOI . (project_extension . 0)))
      ((default . error) (CASE . (simple_declarative_item . 1)) (END . (simple_declarative_item . 1)) (FOR . (simple_declarative_item . 1)) (NULL . (simple_declarative_item . 1)) (PACKAGE . (simple_declarative_item . 1)) (TYPE . (simple_declarative_item . 1)) (IDENTIFIER . (simple_declarative_item . 1)))
      ((default . error) (STRING_LITERAL .  230))
      ((default . error) (SEMICOLON . (aggregate . 0)))
      ((default . error) (DOT . (name . 1)) (SEMICOLON . (name . 1)))
      ((default . error) (SEMICOLON .  229))
      ((default . error) (END .  228))
      ((default . error) (EXTERNAL .  127) (EXTERNAL_AS_LIST .  128) (LEFT_PAREN .  129) (PROJECT .  16) (AMPERSAND . (identifier_opt . 0)) (DOT . (identifier_opt . 0)) (QUOTE . (identifier_opt . 0)) (SEMICOLON . (identifier_opt . 0)) (IDENTIFIER .  130) (STRING_LITERAL .  131))
      ((default . error) (EXTERNAL .  127) (EXTERNAL_AS_LIST .  128) (LEFT_PAREN .  129) (PROJECT .  16) (AMPERSAND . (identifier_opt . 0)) (DOT . (identifier_opt . 0)) (QUOTE . (identifier_opt . 0)) (SEMICOLON . (identifier_opt . 0)) (IDENTIFIER .  130) (STRING_LITERAL .  131))
      ((default . error) (CASE .  209) (END . (declarative_items_opt . 0)) (FOR .  210) (NULL .  211) (PACKAGE .  212) (TYPE .  213) (WHEN . (declarative_items_opt . 0)) (IDENTIFIER .  214))
      ((default . error) (OTHERS .  176) (EQUAL_GREATER . (discrete_choice . 0)) (VERTICAL_BAR . (discrete_choice . 0)) (STRING_LITERAL .  177))
      ((default . error) (SEMICOLON .  207))
      ((default . error) (CASE . (case_statement . 0)) (END . (case_statement . 0)) (FOR . (case_statement . 0)) (NULL . (case_statement . 0)) (PACKAGE . (case_statement . 0)) (TYPE . (case_statement . 0)) (IDENTIFIER . (case_statement . 0)))
      ((default . error) (EQUAL_GREATER . (discrete_choice_list . 1)) (VERTICAL_BAR . (discrete_choice_list . 1)))
      ((default . error) (IS . (identifier_opt . 0)) (DOT . (identifier_opt . 0)) (IDENTIFIER .  115))
      ((default . error) (EXTERNAL .  241) (IDENTIFIER .  242))
      ((default . error) (SEMICOLON .  240))
      ((default . error) (EXTENDS . (identifier_opt . 0)) (IS . (identifier_opt . 0)) (RENAMES . (identifier_opt . 0)) (IDENTIFIER .  110))
      ((default . error) (IDENTIFIER .  238))
      ((default . error) (COLON .  236) (COLON_EQUALS .  237))
      ((default . error) (CASE . (simple_declarative_item . 2)) (END . (simple_declarative_item . 2)) (FOR . (simple_declarative_item . 2)) (NULL . (simple_declarative_item . 2)) (PACKAGE . (simple_declarative_item . 2)) (TYPE . (simple_declarative_item . 2)) (WHEN . (simple_declarative_item . 2)) (IDENTIFIER . (simple_declarative_item . 2)))
      ((default . error) (CASE . (simple_declarative_item . 3)) (END . (simple_declarative_item . 3)) (FOR . (simple_declarative_item . 3)) (NULL . (simple_declarative_item . 3)) (PACKAGE . (simple_declarative_item . 3)) (TYPE . (simple_declarative_item . 3)) (WHEN . (simple_declarative_item . 3)) (IDENTIFIER . (simple_declarative_item . 3)))
      ((default . error) (CASE . (declarative_items . 0)) (END . (declarative_items . 0)) (FOR . (declarative_items . 0)) (NULL . (declarative_items . 0)) (PACKAGE . (declarative_items . 0)) (TYPE . (declarative_items . 0)) (WHEN . (declarative_items . 0)) (IDENTIFIER . (declarative_items . 0)))
      ((default . error) (CASE .  209) (END . (declarative_items_opt . 1)) (FOR .  210) (NULL .  211) (PACKAGE .  212) (TYPE .  213) (WHEN . (declarative_items_opt . 1)) (IDENTIFIER .  214))
      ((default . error) (END . (case_item . 0)) (WHEN . (case_item . 0)))
      ((default . error) (CASE . (declarative_item . 2)) (END . (declarative_item . 2)) (FOR . (declarative_item . 2)) (NULL . (declarative_item . 2)) (PACKAGE . (declarative_item . 2)) (TYPE . (declarative_item . 2)) (WHEN . (declarative_item . 2)) (IDENTIFIER . (declarative_item . 2)))
      ((default . error) (CASE . (package_declaration . 0)) (END . (package_declaration . 0)) (FOR . (package_declaration . 0)) (NULL . (package_declaration . 0)) (PACKAGE . (package_declaration . 0)) (TYPE . (package_declaration . 0)) (WHEN . (package_declaration . 0)) (IDENTIFIER . (package_declaration . 0)))
      ((default . error) (CASE . (package_declaration . 1)) (END . (package_declaration . 1)) (FOR . (package_declaration . 1)) (NULL . (package_declaration . 1)) (PACKAGE . (package_declaration . 1)) (TYPE . (package_declaration . 1)) (WHEN . (package_declaration . 1)) (IDENTIFIER . (package_declaration . 1)))
      ((default . error) (CASE . (package_declaration . 2)) (END . (package_declaration . 2)) (FOR . (package_declaration . 2)) (NULL . (package_declaration . 2)) (PACKAGE . (package_declaration . 2)) (TYPE . (package_declaration . 2)) (WHEN . (package_declaration . 2)) (IDENTIFIER . (package_declaration . 2)))
      ((default . error) (CASE . (declarative_item . 0)) (END . (declarative_item . 0)) (FOR . (declarative_item . 0)) (NULL . (declarative_item . 0)) (PACKAGE . (declarative_item . 0)) (TYPE . (declarative_item . 0)) (WHEN . (declarative_item . 0)) (IDENTIFIER . (declarative_item . 0)))
      ((default . error) (CASE . (declarative_item . 1)) (END . (declarative_item . 1)) (FOR . (declarative_item . 1)) (NULL . (declarative_item . 1)) (PACKAGE . (declarative_item . 1)) (TYPE . (declarative_item . 1)) (WHEN . (declarative_item . 1)) (IDENTIFIER . (declarative_item . 1)))
      ((default . error) (AMPERSAND .  150) (SEMICOLON .  234))
      ((default . error) (AMPERSAND .  150) (SEMICOLON .  233))
      ((default . error) (SEMICOLON . (identifier_opt . 0)) (IDENTIFIER .  142))
      ((default . error) (CASE . (package_spec . 0)) (END . (package_spec . 0)) (FOR . (package_spec . 0)) (NULL . (package_spec . 0)) (PACKAGE . (package_spec . 0)) (TYPE . (package_spec . 0)) (IDENTIFIER . (package_spec . 0)))
      ((default . error) (RIGHT_PAREN .  231))
      ((default . error) (AMPERSAND . (attribute_reference . 1)) (SEMICOLON . (attribute_reference . 1)))
      ((default . error) (SEMICOLON .  254))
      ((default . error) (CASE . (attribute_declaration . 2)) (END . (attribute_declaration . 2)) (FOR . (attribute_declaration . 2)) (NULL . (attribute_declaration . 2)) (PACKAGE . (attribute_declaration . 2)) (TYPE . (attribute_declaration . 2)) (IDENTIFIER . (attribute_declaration . 2)))
      ((default . error) (CASE . (attribute_declaration . 1)) (END . (attribute_declaration . 1)) (FOR . (attribute_declaration . 1)) (NULL . (attribute_declaration . 1)) (PACKAGE . (attribute_declaration . 1)) (TYPE . (attribute_declaration . 1)) (IDENTIFIER . (attribute_declaration . 1)))
      ((default . error) (CASE . (declarative_items . 1)) (END . (declarative_items . 1)) (FOR . (declarative_items . 1)) (NULL . (declarative_items . 1)) (PACKAGE . (declarative_items . 1)) (TYPE . (declarative_items . 1)) (WHEN . (declarative_items . 1)) (IDENTIFIER . (declarative_items . 1)))
      ((default . error) (IDENTIFIER .  253))
      ((default . error) (EXTERNAL .  127) (EXTERNAL_AS_LIST .  128) (LEFT_PAREN .  129) (PROJECT .  16) (AMPERSAND . (identifier_opt . 0)) (DOT . (identifier_opt . 0)) (QUOTE . (identifier_opt . 0)) (SEMICOLON . (identifier_opt . 0)) (IDENTIFIER .  130) (STRING_LITERAL .  131))
      ((default . error) (IS .  251))
      ((default . error) (EXTENDS .  248) (IS .  249) (RENAMES .  250))
      ((default . error) (CASE . (simple_declarative_item . 4)) (END . (simple_declarative_item . 4)) (FOR . (simple_declarative_item . 4)) (NULL . (simple_declarative_item . 4)) (PACKAGE . (simple_declarative_item . 4)) (TYPE . (simple_declarative_item . 4)) (WHEN . (simple_declarative_item . 4)) (IDENTIFIER . (simple_declarative_item . 4)))
      ((default . error) (LEFT_PAREN .  247))
      ((default . error) (LEFT_PAREN .  245) (USE .  246))
      ((default . error) (IS .  244) (DOT .  119))
      ((default . error) (END . (case_items . 0)) (WHEN . ( 171 (case_items . 0))))
      ((default . error) (OTHERS .  167) (RIGHT_PAREN . (discrete_choice . 0)) (STRING_LITERAL .  168))
      ((default . error) (EXTERNAL .  127) (EXTERNAL_AS_LIST .  128) (LEFT_PAREN .  129) (PROJECT .  16) (AMPERSAND . (identifier_opt . 0)) (DOT . (identifier_opt . 0)) (QUOTE . (identifier_opt . 0)) (SEMICOLON . (identifier_opt . 0)) (IDENTIFIER .  130) (STRING_LITERAL .  131))
      ((default . error) (STRING_LITERAL .  261))
      ((default . error) (IS . (identifier_opt . 0)) (DOT . (identifier_opt . 0)) (IDENTIFIER .  115))
      ((default . error) (CASE .  82) (END . (declarative_items_opt . 0)) (FOR .  83) (NULL .  84) (PACKAGE .  85) (TYPE .  86) (IDENTIFIER .  87))
      ((default . error) (DOT . (identifier_opt . 0)) (SEMICOLON . (identifier_opt . 0)) (IDENTIFIER .  160))
      ((default . error) (LEFT_PAREN .  158))
      ((default . error) (AMPERSAND .  150) (SEMICOLON .  256))
      ((default . error) (COLON_EQUALS .  255))
      ((default . error) (CASE . (package_extension . 0)) (END . (package_extension . 0)) (FOR . (package_extension . 0)) (NULL . (package_extension . 0)) (PACKAGE . (package_extension . 0)) (TYPE . (package_extension . 0)) (IDENTIFIER . (package_extension . 0)))
      ((default . error) (EXTERNAL .  127) (EXTERNAL_AS_LIST .  128) (LEFT_PAREN .  129) (PROJECT .  16) (AMPERSAND . (identifier_opt . 0)) (DOT . (identifier_opt . 0)) (QUOTE . (identifier_opt . 0)) (SEMICOLON . (identifier_opt . 0)) (IDENTIFIER .  130) (STRING_LITERAL .  131))
      ((default . error) (CASE . (simple_declarative_item . 0)) (END . (simple_declarative_item . 0)) (FOR . (simple_declarative_item . 0)) (NULL . (simple_declarative_item . 0)) (PACKAGE . (simple_declarative_item . 0)) (TYPE . (simple_declarative_item . 0)) (WHEN . (simple_declarative_item . 0)) (IDENTIFIER . (simple_declarative_item . 0)))
      ((default . error) (SEMICOLON .  272))
      ((default . error) (DOT .  185) (SEMICOLON .  271))
      ((default . error) (END .  270))
      ((default . error) (IS .  269) (DOT .  119))
      ((default . error) (RIGHT_PAREN .  268))
      ((default . error) (AMPERSAND .  150) (SEMICOLON .  267))
      ((default . error) (RIGHT_PAREN .  266))
      ((default . error) (END .  265) (WHEN .  171))
      ((default . error) (CASE .  279))
      ((default . error) (USE .  278))
      ((default . error) (CASE . (attribute_declaration . 0)) (END . (attribute_declaration . 0)) (FOR . (attribute_declaration . 0)) (NULL . (attribute_declaration . 0)) (PACKAGE . (attribute_declaration . 0)) (TYPE . (attribute_declaration . 0)) (WHEN . (attribute_declaration . 0)) (IDENTIFIER . (attribute_declaration . 0)))
      ((default . error) (USE .  277))
      ((default . error) (CASE .  82) (END . (declarative_items_opt . 0)) (FOR .  83) (NULL .  84) (PACKAGE .  85) (TYPE .  86) (IDENTIFIER .  87))
      ((default . error) (SEMICOLON . (identifier_opt . 0)) (IDENTIFIER .  142))
      ((default . error) (CASE . (package_renaming . 0)) (END . (package_renaming . 0)) (FOR . (package_renaming . 0)) (NULL . (package_renaming . 0)) (PACKAGE . (package_renaming . 0)) (TYPE . (package_renaming . 0)) (WHEN . (package_renaming . 0)) (IDENTIFIER . (package_renaming . 0)))
      ((default . error) (CASE . (typed_string_declaration . 0)) (END . (typed_string_declaration . 0)) (FOR . (typed_string_declaration . 0)) (NULL . (typed_string_declaration . 0)) (PACKAGE . (typed_string_declaration . 0)) (TYPE . (typed_string_declaration . 0)) (WHEN . (typed_string_declaration . 0)) (IDENTIFIER . (typed_string_declaration . 0)))
      ((default . error) (AMPERSAND .  150) (SEMICOLON .  274))
      ((default . error) (CASE . (simple_declarative_item . 1)) (END . (simple_declarative_item . 1)) (FOR . (simple_declarative_item . 1)) (NULL . (simple_declarative_item . 1)) (PACKAGE . (simple_declarative_item . 1)) (TYPE . (simple_declarative_item . 1)) (WHEN . (simple_declarative_item . 1)) (IDENTIFIER . (simple_declarative_item . 1)))
      ((default . error) (SEMICOLON .  284))
      ((default . error) (END .  283))
      ((default . error) (EXTERNAL .  127) (EXTERNAL_AS_LIST .  128) (LEFT_PAREN .  129) (PROJECT .  16) (AMPERSAND . (identifier_opt . 0)) (DOT . (identifier_opt . 0)) (QUOTE . (identifier_opt . 0)) (SEMICOLON . (identifier_opt . 0)) (IDENTIFIER .  130) (STRING_LITERAL .  131))
      ((default . error) (EXTERNAL .  127) (EXTERNAL_AS_LIST .  128) (LEFT_PAREN .  129) (PROJECT .  16) (AMPERSAND . (identifier_opt . 0)) (DOT . (identifier_opt . 0)) (QUOTE . (identifier_opt . 0)) (SEMICOLON . (identifier_opt . 0)) (IDENTIFIER .  130) (STRING_LITERAL .  131))
      ((default . error) (SEMICOLON .  280))
      ((default . error) (CASE . (case_statement . 0)) (END . (case_statement . 0)) (FOR . (case_statement . 0)) (NULL . (case_statement . 0)) (PACKAGE . (case_statement . 0)) (TYPE . (case_statement . 0)) (WHEN . (case_statement . 0)) (IDENTIFIER . (case_statement . 0)))
      ((default . error) (AMPERSAND .  150) (SEMICOLON .  287))
      ((default . error) (AMPERSAND .  150) (SEMICOLON .  286))
      ((default . error) (SEMICOLON . (identifier_opt . 0)) (IDENTIFIER .  142))
      ((default . error) (CASE . (package_spec . 0)) (END . (package_spec . 0)) (FOR . (package_spec . 0)) (NULL . (package_spec . 0)) (PACKAGE . (package_spec . 0)) (TYPE . (package_spec . 0)) (WHEN . (package_spec . 0)) (IDENTIFIER . (package_spec . 0)))
      ((default . error) (SEMICOLON .  288))
      ((default . error) (CASE . (attribute_declaration . 2)) (END . (attribute_declaration . 2)) (FOR . (attribute_declaration . 2)) (NULL . (attribute_declaration . 2)) (PACKAGE . (attribute_declaration . 2)) (TYPE . (attribute_declaration . 2)) (WHEN . (attribute_declaration . 2)) (IDENTIFIER . (attribute_declaration . 2)))
      ((default . error) (CASE . (attribute_declaration . 1)) (END . (attribute_declaration . 1)) (FOR . (attribute_declaration . 1)) (NULL . (attribute_declaration . 1)) (PACKAGE . (attribute_declaration . 1)) (TYPE . (attribute_declaration . 1)) (WHEN . (attribute_declaration . 1)) (IDENTIFIER . (attribute_declaration . 1)))
      ((default . error) (CASE . (package_extension . 0)) (END . (package_extension . 0)) (FOR . (package_extension . 0)) (NULL . (package_extension . 0)) (PACKAGE . (package_extension . 0)) (TYPE . (package_extension . 0)) (WHEN . (package_extension . 0)) (IDENTIFIER . (package_extension . 0)))]
     [((compilation_unit . 2)(context_clause . 3)(context_clause_opt . 4)(with_clause . 5))
      ((aggregate . 19)(attribute_prefix . 20)(attribute_reference . 21)(expression . 22)(external_value . 23)(identifier_opt . 24)(name . 25)(string_primary . 26)(string_list . 27)(term . 28))
      nil
      ((with_clause . 12))
      ((project_qualifier_opt . 11))
      nil
      nil
      nil
      nil
      nil
      nil
      ((project_declaration_opt . 54)(project_extension . 55)(simple_project_declaration . 56))
      nil
      ((aggregate . 52))
      ((aggregate . 51))
      ((aggregate . 40)(attribute_prefix . 41)(attribute_reference . 42)(expression . 43)(external_value . 44)(identifier_opt . 45)(name . 46)(string_primary . 47)(string_list . 48)(term . 49))
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
      ((aggregate . 19)(attribute_prefix . 20)(attribute_reference . 21)(expression . 73)(external_value . 23)(identifier_opt . 24)(name . 25)(string_primary . 26)(term . 28))
      nil
      nil
      ((aggregate . 19)(attribute_prefix . 20)(attribute_reference . 21)(external_value . 23)(identifier_opt . 24)(name . 25)(string_primary . 26)(term . 71))
      nil
      ((aggregate . 69))
      ((aggregate . 68))
      ((aggregate . 40)(attribute_prefix . 41)(attribute_reference . 42)(expression . 43)(external_value . 44)(identifier_opt . 45)(name . 46)(string_primary . 47)(string_list . 66)(term . 49))
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
      ((aggregate . 40)(attribute_prefix . 41)(attribute_reference . 42)(expression . 43)(external_value . 44)(identifier_opt . 45)(name . 46)(string_primary . 47)(string_list . 48)(term . 49))
      nil
      nil
      ((identifier_opt . 59))
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      ((aggregate . 40)(attribute_prefix . 41)(attribute_reference . 42)(expression . 79)(external_value . 44)(identifier_opt . 45)(name . 46)(string_primary . 47)(term . 49))
      nil
      ((aggregate . 40)(attribute_prefix . 41)(attribute_reference . 42)(external_value . 44)(identifier_opt . 45)(name . 46)(string_primary . 47)(term . 77))
      nil
      nil
      nil
      ((aggregate . 40)(attribute_prefix . 41)(attribute_reference . 42)(expression . 43)(external_value . 44)(identifier_opt . 45)(name . 46)(string_primary . 47)(string_list . 66)(term . 49))
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
      ((attribute_declaration . 88)(case_statement . 89)(declarative_item . 90)(declarative_items . 91)(declarative_items_opt . 92)(package_declaration . 93)(package_spec . 94)(package_extension . 95)(package_renaming . 96)(simple_declarative_item . 97)(typed_string_declaration . 98))
      ((identifier_opt . 116)(name . 117))
      nil
      nil
      ((identifier_opt . 111))
      nil
      nil
      nil
      nil
      nil
      ((attribute_declaration . 88)(case_statement . 89)(declarative_item . 106)(package_declaration . 93)(package_spec . 94)(package_extension . 95)(package_renaming . 96)(simple_declarative_item . 97)(typed_string_declaration . 98))
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
      ((attribute_declaration . 88)(case_statement . 89)(declarative_item . 90)(declarative_items . 91)(declarative_items_opt . 144)(package_declaration . 93)(package_spec . 94)(package_extension . 95)(package_renaming . 96)(simple_declarative_item . 97)(typed_string_declaration . 98))
      ((identifier_opt . 143))
      nil
      nil
      ((aggregate . 132)(attribute_prefix . 133)(attribute_reference . 134)(expression . 135)(external_value . 136)(identifier_opt . 137)(name . 138)(string_primary . 139)(term . 140))
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      ((case_item . 172)(case_items . 173))
      nil
      ((discrete_choice . 169))
      ((aggregate . 132)(attribute_prefix . 133)(attribute_reference . 134)(expression . 166)(external_value . 136)(identifier_opt . 137)(name . 138)(string_primary . 139)(term . 140))
      nil
      ((identifier_opt . 116)(name . 164))
      ((attribute_declaration . 88)(case_statement . 89)(declarative_item . 90)(declarative_items . 91)(declarative_items_opt . 163)(package_declaration . 93)(package_spec . 94)(package_extension . 95)(package_renaming . 96)(simple_declarative_item . 97)(typed_string_declaration . 98))
      ((identifier_opt . 161)(name . 162))
      ((aggregate . 159))
      ((aggregate . 157))
      ((aggregate . 156))
      ((aggregate . 40)(attribute_prefix . 41)(attribute_reference . 42)(expression . 43)(external_value . 44)(identifier_opt . 45)(name . 46)(string_primary . 47)(string_list . 154)(term . 49))
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
      ((identifier_opt . 194))
      nil
      ((aggregate . 132)(attribute_prefix . 133)(attribute_reference . 134)(expression . 193)(external_value . 136)(identifier_opt . 137)(name . 138)(string_primary . 139)(term . 140))
      nil
      ((aggregate . 132)(attribute_prefix . 133)(attribute_reference . 134)(external_value . 136)(identifier_opt . 137)(name . 138)(string_primary . 139)(term . 191))
      nil
      nil
      nil
      nil
      ((aggregate . 40)(attribute_prefix . 41)(attribute_reference . 42)(expression . 43)(external_value . 44)(identifier_opt . 45)(name . 46)(string_primary . 47)(string_list . 154)(term . 49))
      nil
      nil
      ((aggregate . 40)(attribute_prefix . 41)(attribute_reference . 42)(expression . 43)(external_value . 44)(identifier_opt . 45)(name . 46)(string_primary . 47)(string_list . 188)(term . 49))
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
      ((discrete_choice . 178)(discrete_choice_list . 179))
      nil
      ((case_item . 175))
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      ((attribute_declaration . 88)(case_statement . 89)(declarative_item . 90)(declarative_items . 91)(declarative_items_opt . 201)(package_declaration . 93)(package_spec . 94)(package_extension . 95)(package_renaming . 96)(simple_declarative_item . 97)(typed_string_declaration . 98))
      ((identifier_opt . 200))
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
      nil
      ((aggregate . 132)(attribute_prefix . 133)(attribute_reference . 134)(expression . 227)(external_value . 136)(identifier_opt . 137)(name . 138)(string_primary . 139)(term . 140))
      ((aggregate . 132)(attribute_prefix . 133)(attribute_reference . 134)(expression . 226)(external_value . 136)(identifier_opt . 137)(name . 138)(string_primary . 139)(term . 140))
      ((attribute_declaration . 215)(case_statement . 216)(declarative_item . 217)(declarative_items . 218)(declarative_items_opt . 219)(package_declaration . 220)(package_spec . 221)(package_extension . 222)(package_renaming . 223)(simple_declarative_item . 224)(typed_string_declaration . 225))
      ((discrete_choice . 208))
      nil
      nil
      nil
      ((identifier_opt . 116)(name . 243))
      nil
      nil
      ((identifier_opt . 239))
      nil
      nil
      nil
      nil
      nil
      ((attribute_declaration . 215)(case_statement . 216)(declarative_item . 235)(package_declaration . 220)(package_spec . 221)(package_extension . 222)(package_renaming . 223)(simple_declarative_item . 224)(typed_string_declaration . 225))
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      ((identifier_opt . 232))
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      ((aggregate . 132)(attribute_prefix . 133)(attribute_reference . 134)(expression . 252)(external_value . 136)(identifier_opt . 137)(name . 138)(string_primary . 139)(term . 140))
      nil
      nil
      nil
      nil
      nil
      nil
      ((case_item . 172)(case_items . 264))
      ((discrete_choice . 263))
      ((aggregate . 132)(attribute_prefix . 133)(attribute_reference . 134)(expression . 262)(external_value . 136)(identifier_opt . 137)(name . 138)(string_primary . 139)(term . 140))
      nil
      ((identifier_opt . 116)(name . 260))
      ((attribute_declaration . 88)(case_statement . 89)(declarative_item . 90)(declarative_items . 91)(declarative_items_opt . 259)(package_declaration . 93)(package_spec . 94)(package_extension . 95)(package_renaming . 96)(simple_declarative_item . 97)(typed_string_declaration . 98))
      ((identifier_opt . 161)(name . 258))
      ((aggregate . 257))
      nil
      nil
      nil
      ((aggregate . 132)(attribute_prefix . 133)(attribute_reference . 134)(expression . 273)(external_value . 136)(identifier_opt . 137)(name . 138)(string_primary . 139)(term . 140))
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      ((case_item . 175))
      nil
      nil
      nil
      nil
      ((attribute_declaration . 88)(case_statement . 89)(declarative_item . 90)(declarative_items . 91)(declarative_items_opt . 276)(package_declaration . 93)(package_spec . 94)(package_extension . 95)(package_renaming . 96)(simple_declarative_item . 97)(typed_string_declaration . 98))
      ((identifier_opt . 275))
      nil
      nil
      nil
      nil
      nil
      nil
      ((aggregate . 132)(attribute_prefix . 133)(attribute_reference . 134)(expression . 282)(external_value . 136)(identifier_opt . 137)(name . 138)(string_primary . 139)(term . 140))
      ((aggregate . 132)(attribute_prefix . 133)(attribute_reference . 134)(expression . 281)(external_value . 136)(identifier_opt . 137)(name . 138)(string_primary . 139)(term . 140))
      nil
      nil
      nil
      nil
      ((identifier_opt . 285))
      nil
      nil
      nil
      nil
      nil]))
  "Parser table.")

(provide 'gpr-grammar-elisp)

;; end of file
