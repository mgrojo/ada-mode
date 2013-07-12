;;; gpr-grammar-wy.el --- Generated parser support file

;; Copyright (C) 2013 Stephen Leake.  All Rights Reserved.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; PLEASE DO NOT MANUALLY EDIT THIS FILE!  It is automatically
;; generated from the grammar file gpr-grammar.wy

(require 'wisi)
(require 'semantic/lex)
(require 'wisi-compile)

(defconst gpr-grammar-wy--keyword-table
  (semantic-lex-make-keyword-table
   '(
    ("abstract" . ABSTRACT)
    ("aggregate" . AGGREGATE)
    ("case" . CASE)
    ("configuration" . CONFIGURATION)
    ("end" . END)
    ("external" . EXTERNAL)
    ("for" . FOR)
    ("is" . IS)
    ("(" . LEFT_PAREN)
    ("library" . LIBRARY)
    ("null" . NULL)
    ("others" . OTHERS)
    ("package" . PACKAGE)
    ("project" . PROJECT)
    (")" . RIGHT_PAREN)
    ("standard" . STANDARD)
    ("type" . TYPE)
    ("use" . USE)
    ("when" . WHEN)
    ("with" . WITH)
    )
   nil)
  "Table of language keywords.")

(defconst gpr-grammar-wy--token-table
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
     ("string"
      (STRING_LITERAL)
     )
    )
   nil)
  "Table of language tokens.")

(defconst gpr-grammar-wy--parse-table
   (wisi-compile-grammar
   '((AMPERSAND COLON COLON_EQUALS COMMA DOT EQUAL_GREATER QUOTE SEMICOLON VERTICAL_BAR IDENTIFIER STRING_LITERAL ABSTRACT AGGREGATE CASE CONFIGURATION END EXTERNAL FOR IS LEFT_PAREN LIBRARY NULL OTHERS PACKAGE PROJECT RIGHT_PAREN STANDARD TYPE USE WHEN WITH )
     ((attribute_declaration
       ((FOR IDENTIFIER USE expression SEMICOLON )
        (progn
        `,(wisi-statement-action 1 'statement-start 5 'statement-end)
        `,(wisi-start-action 1 4)))
       ((FOR IDENTIFIER LEFT_PAREN STRING_LITERAL RIGHT_PAREN USE expression SEMICOLON )
        (progn
        `,(wisi-statement-action 1 'statement-start 3 'open-paren 6 'close-paren 8 'statement-end)
        `,(wisi-start-action 1 7))))
      (attribute_prefix
       ((PROJECT ))
       ((name )))
      (attribute_reference
       ((attribute_prefix QUOTE IDENTIFIER ))
       ((attribute_prefix QUOTE IDENTIFIER LEFT_PAREN STRING_LITERAL RIGHT_PAREN )
        `,(wisi-statement-action 4 'open-paren 6 'close-paren)))
      (case_statement
       ((CASE name IS case_items END CASE SEMICOLON )
        (progn
        `,(wisi-statement-action 1 'statement-start 5 'statement-middle 7 'statement-end)
        `,(wisi-start-action 1 4))))
      (case_item
       ((WHEN discrete_choice_list EQUAL_GREATER declarative_items )
        (progn
        `,(wisi-statement-action 1 'statement-middle)
        `,(wisi-start-action 1 4))))
      (case_items
       ((case_item ))
       ((case_items case_item )))
      (compilation_unit
       ((context_clause project_declaration ))
       ((project_declaration )))
      (context_clause
       ((with_clause ))
       ((project_qualifier ))
       ((context_clause with_clause )))
      (declarative_item
       ((simple_declarative_item ))
       ((typed_string_declaration ))
       ((package_declaration )))
      (declarative_items
       ((declarative_item ))
       ((declarative_items declarative_item )))
      (discrete_choice
       ((STRING_LITERAL ))
       ((OTHERS )))
      (discrete_choice_list
       ((discrete_choice ))
       ((discrete_choice_list VERTICAL_BAR discrete_choice )))
      (expression
       ((term ))
       ((expression AMPERSAND term )))
      (external_value
       ((EXTERNAL LEFT_PAREN string_list RIGHT_PAREN )
        `,(wisi-statement-action 2 'open-paren 4 'close-paren)))
      (name
       ((IDENTIFIER ))
       ((name DOT IDENTIFIER )))
      (project_declaration
       ((simple_project_declaration )))
      (package_declaration
       ((package_spec )))
      (package_spec
       ((PACKAGE IDENTIFIER IS simple_declarative_items END IDENTIFIER SEMICOLON )
        (progn
        `,(wisi-statement-action 1 'statement-start 3 'block-start 5 'block-end 7 'statement-end)
        `,(wisi-start-action 1 4))))
      (project_qualifier
       ((ABSTRACT ))
       ((STANDARD ))
       ((AGGREGATE ))
       ((AGGREGATE LIBRARY ))
       ((LIBRARY ))
       ((CONFIGURATION )))
      (simple_declarative_item
       ((IDENTIFIER COLON_EQUALS expression SEMICOLON )
        (progn
        `,(wisi-statement-action 1 'statement-start 4 'statement-end)
        `,(wisi-start-action 1 3)))
       ((IDENTIFIER COLON IDENTIFIER COLON_EQUALS expression SEMICOLON )
        (progn
        `,(wisi-statement-action 1 'statement-start 6 'statement-end)
        `,(wisi-start-action 1 5)))
       ((attribute_declaration ))
       ((case_statement ))
       ((NULL SEMICOLON )
        `,(wisi-statement-action 1 'statement-start 2 'statement-end)))
      (simple_declarative_items
       ((simple_declarative_item ))
       ((simple_declarative_items simple_declarative_item )))
      (simple_project_declaration
       ((PROJECT IDENTIFIER IS declarative_items END IDENTIFIER SEMICOLON )
        (progn
        `,(wisi-statement-action
        1 'statement-start
        3 'block-start
        5 'block-end
        7 'statement-end)
        `,(wisi-start-action 1 4))))
      (string_expression
       ((STRING_LITERAL ))
       ((name ))
       ((external_value ))
       ((attribute_reference )))
      (string_list
       ((string_expression ))
       ((string_list COMMA string_expression )))
      (term
       ((string_expression ))
       ((LEFT_PAREN RIGHT_PAREN ))
       ((LEFT_PAREN string_list RIGHT_PAREN )
        `,(wisi-statement-action
        1 'open-paren
        3 'close-paren)))
      (typed_string_declaration
       ((TYPE IDENTIFIER IS LEFT_PAREN string_list RIGHT_PAREN SEMICOLON )
        `,(wisi-statement-action
        1 'statement-start
        4 'open-paren
        6 'close-paren
        7 'statement-end)))
      (with_clause
       ((WITH string_list SEMICOLON ))))
     [((default . error) (ABSTRACT .  1) (STANDARD .  6) (AGGREGATE .  2) (LIBRARY .  4) (CONFIGURATION .  3) (WITH .  7) (PROJECT .  5))
      ((default . error) (WITH . (project_qualifier . 0)) (PROJECT . (project_qualifier . 0)))
      ((default . error) (LIBRARY .  28) (WITH . (project_qualifier . 2)) (PROJECT . (project_qualifier . 2)))
      ((default . error) (WITH . (project_qualifier . 5)) (PROJECT . (project_qualifier . 5)))
      ((default . error) (WITH . (project_qualifier . 4)) (PROJECT . (project_qualifier . 4)))
      ((default . error) (IDENTIFIER .  27))
      ((default . error) (WITH . (project_qualifier . 1)) (PROJECT . (project_qualifier . 1)))
      ((default . error) (STRING_LITERAL .  18) (EXTERNAL .  19) (IDENTIFIER .  17) (PROJECT .  20))
      ((default . error) ($EOI .  16))
      ((default . error) (WITH .  7) (PROJECT .  5))
      ((default . error) ($EOI . (compilation_unit . 1)))
      ((default . error) (PROJECT . (context_clause . 1)) (WITH . (context_clause . 1)))
      ((default . error) ($EOI . (project_declaration . 0)))
      ((default . error) (PROJECT . (context_clause . 0)) (WITH . (context_clause . 0)))
      ((default . error) ($EOI . (compilation_unit . 0)))
      ((default . error) (WITH . (context_clause . 2)) (PROJECT . (context_clause . 2)))
      ((default . error) ($EOI . accept) (WITH . accept) (WHEN . accept) (USE . accept) (TYPE . accept) (STANDARD . accept) (RIGHT_PAREN . accept) (PROJECT . accept) (PACKAGE . accept) (OTHERS . accept) (NULL . accept) (LIBRARY . accept) (LEFT_PAREN . accept) (IS . accept) (FOR . accept) (EXTERNAL . accept) (END . accept) (CONFIGURATION . accept) (CASE . accept) (AGGREGATE . accept) (ABSTRACT . accept) (STRING_LITERAL . accept) (IDENTIFIER . accept) (VERTICAL_BAR . accept) (SEMICOLON . accept) (QUOTE . accept) (EQUAL_GREATER . accept) (DOT . accept) (COMMA . accept) (COLON_EQUALS . accept) (COLON . accept) (AMPERSAND . accept))
      ((default . error) (IS . (name . 0)) (RIGHT_PAREN . (name . 0)) (COMMA . (name . 0)) (AMPERSAND . (name . 0)) (SEMICOLON . (name . 0)) (DOT . (name . 0)) (QUOTE . (name . 0)))
      ((default . error) (COMMA . (string_expression . 0)) (RIGHT_PAREN . (string_expression . 0)) (SEMICOLON . (string_expression . 0)) (AMPERSAND . (string_expression . 0)))
      ((default . error) (LEFT_PAREN .  34))
      ((default . error) (QUOTE . (attribute_prefix . 0)))
      ((default . error) (QUOTE .  33))
      ((default . error) (COMMA . (string_expression . 3)) (RIGHT_PAREN . (string_expression . 3)) (SEMICOLON . (string_expression . 3)) (AMPERSAND . (string_expression . 3)))
      ((default . error) (COMMA . (string_expression . 2)) (RIGHT_PAREN . (string_expression . 2)) (SEMICOLON . (string_expression . 2)) (AMPERSAND . (string_expression . 2)))
      ((default . error) (COMMA . (string_expression . 1)) (RIGHT_PAREN . (string_expression . 1)) (SEMICOLON . (string_expression . 1)) (AMPERSAND . (string_expression . 1)) (DOT .  32) (QUOTE . (attribute_prefix . 1)))
      ((default . error) (SEMICOLON . (string_list . 0)) (RIGHT_PAREN . (string_list . 0)) (COMMA . (string_list . 0)))
      ((default . error) (COMMA .  30) (SEMICOLON .  31))
      ((default . error) (IS .  29))
      ((default . error) (PROJECT . (project_qualifier . 3)) (WITH . (project_qualifier . 3)))
      ((default . error) (TYPE .  44) (IDENTIFIER .  39) (NULL .  42) (CASE .  40) (FOR .  41) (PACKAGE .  43))
      ((default . error) (STRING_LITERAL .  18) (EXTERNAL .  19) (IDENTIFIER .  17) (PROJECT .  20))
      ((default . error) (WITH . (with_clause . 0)) (PROJECT . (with_clause . 0)))
      ((default . error) (IDENTIFIER .  37))
      ((default . error) (IDENTIFIER .  36))
      ((default . error) (STRING_LITERAL .  18) (EXTERNAL .  19) (IDENTIFIER .  17) (PROJECT .  20))
      ((default . error) (COMMA .  30) (RIGHT_PAREN .  63))
      ((default . error) (LEFT_PAREN .  62) (RIGHT_PAREN . (attribute_reference . 0)) (COMMA . (attribute_reference . 0)) (AMPERSAND . (attribute_reference . 0)) (SEMICOLON . (attribute_reference . 0)))
      ((default . error) (IS . (name . 1)) (RIGHT_PAREN . (name . 1)) (COMMA . (name . 1)) (AMPERSAND . (name . 1)) (SEMICOLON . (name . 1)) (DOT . (name . 1)) (QUOTE . (name . 1)))
      ((default . error) (RIGHT_PAREN . (string_list . 1)) (SEMICOLON . (string_list . 1)) (COMMA . (string_list . 1)))
      ((default . error) (COLON .  60) (COLON_EQUALS .  61))
      ((default . error) (IDENTIFIER .  17))
      ((default . error) (IDENTIFIER .  58))
      ((default . error) (SEMICOLON .  57))
      ((default . error) (IDENTIFIER .  56))
      ((default . error) (IDENTIFIER .  55))
      ((default . error) (WHEN . (simple_declarative_item . 2)) (END . (simple_declarative_item . 2)) (IDENTIFIER . (simple_declarative_item . 2)) (CASE . (simple_declarative_item . 2)) (FOR . (simple_declarative_item . 2)) (NULL . (simple_declarative_item . 2)) (PACKAGE . (simple_declarative_item . 2)) (TYPE . (simple_declarative_item . 2)))
      ((default . error) (WHEN . (simple_declarative_item . 3)) (END . (simple_declarative_item . 3)) (IDENTIFIER . (simple_declarative_item . 3)) (CASE . (simple_declarative_item . 3)) (FOR . (simple_declarative_item . 3)) (NULL . (simple_declarative_item . 3)) (PACKAGE . (simple_declarative_item . 3)) (TYPE . (simple_declarative_item . 3)))
      ((default . error) (WHEN . (declarative_items . 0)) (END . (declarative_items . 0)) (IDENTIFIER . (declarative_items . 0)) (CASE . (declarative_items . 0)) (FOR . (declarative_items . 0)) (NULL . (declarative_items . 0)) (PACKAGE . (declarative_items . 0)) (TYPE . (declarative_items . 0)))
      ((default . error) (END .  53) (TYPE .  44) (IDENTIFIER .  39) (NULL .  42) (CASE .  40) (FOR .  41) (PACKAGE .  43))
      ((default . error) (WHEN . (declarative_item . 2)) (END . (declarative_item . 2)) (TYPE . (declarative_item . 2)) (PACKAGE . (declarative_item . 2)) (NULL . (declarative_item . 2)) (FOR . (declarative_item . 2)) (CASE . (declarative_item . 2)) (IDENTIFIER . (declarative_item . 2)))
      ((default . error) (WHEN . (package_declaration . 0)) (END . (package_declaration . 0)) (IDENTIFIER . (package_declaration . 0)) (CASE . (package_declaration . 0)) (FOR . (package_declaration . 0)) (NULL . (package_declaration . 0)) (PACKAGE . (package_declaration . 0)) (TYPE . (package_declaration . 0)))
      ((default . error) (WHEN . (declarative_item . 0)) (END . (declarative_item . 0)) (TYPE . (declarative_item . 0)) (PACKAGE . (declarative_item . 0)) (NULL . (declarative_item . 0)) (FOR . (declarative_item . 0)) (CASE . (declarative_item . 0)) (IDENTIFIER . (declarative_item . 0)))
      ((default . error) (WHEN . (declarative_item . 1)) (END . (declarative_item . 1)) (TYPE . (declarative_item . 1)) (PACKAGE . (declarative_item . 1)) (NULL . (declarative_item . 1)) (FOR . (declarative_item . 1)) (CASE . (declarative_item . 1)) (IDENTIFIER . (declarative_item . 1)))
      ((default . error) (IDENTIFIER .  75))
      ((default . error) (WHEN . (declarative_items . 1)) (TYPE . (declarative_items . 1)) (PACKAGE . (declarative_items . 1)) (NULL . (declarative_items . 1)) (FOR . (declarative_items . 1)) (CASE . (declarative_items . 1)) (IDENTIFIER . (declarative_items . 1)) (END . (declarative_items . 1)))
      ((default . error) (IS .  74))
      ((default . error) (IS .  73))
      ((default . error) (WHEN . (simple_declarative_item . 4)) (TYPE . (simple_declarative_item . 4)) (PACKAGE . (simple_declarative_item . 4)) (NULL . (simple_declarative_item . 4)) (FOR . (simple_declarative_item . 4)) (CASE . (simple_declarative_item . 4)) (IDENTIFIER . (simple_declarative_item . 4)) (END . (simple_declarative_item . 4)))
      ((default . error) (USE .  72) (LEFT_PAREN .  71))
      ((default . error) (DOT .  32) (IS .  70))
      ((default . error) (IDENTIFIER .  69))
      ((default . error) (LEFT_PAREN .  65) (STRING_LITERAL .  18) (EXTERNAL .  19) (IDENTIFIER .  17) (PROJECT .  20))
      ((default . error) (STRING_LITERAL .  64))
      ((default . error) (SEMICOLON . (external_value . 0)) (AMPERSAND . (external_value . 0)) (COMMA . (external_value . 0)) (RIGHT_PAREN . (external_value . 0)))
      ((default . error) (RIGHT_PAREN .  90))
      ((default . error) (RIGHT_PAREN .  88) (STRING_LITERAL .  18) (EXTERNAL .  19) (IDENTIFIER .  17) (PROJECT .  20))
      ((default . error) (AMPERSAND .  86) (SEMICOLON .  87))
      ((default . error) (AMPERSAND . (term . 0)) (SEMICOLON . (term . 0)))
      ((default . error) (SEMICOLON . (expression . 0)) (AMPERSAND . (expression . 0)))
      ((default . error) (COLON_EQUALS .  85))
      ((default . error) (WHEN .  82))
      ((default . error) (STRING_LITERAL .  81))
      ((default . error) (LEFT_PAREN .  65) (STRING_LITERAL .  18) (EXTERNAL .  19) (IDENTIFIER .  17) (PROJECT .  20))
      ((default . error) (IDENTIFIER .  39) (NULL .  42) (CASE .  40) (FOR .  41))
      ((default . error) (LEFT_PAREN .  77))
      ((default . error) (SEMICOLON .  76))
      ((default . error) ($EOI . (simple_project_declaration . 0)))
      ((default . error) (STRING_LITERAL .  18) (EXTERNAL .  19) (IDENTIFIER .  17) (PROJECT .  20))
      ((default . error) (END . (simple_declarative_items . 0)) (IDENTIFIER . (simple_declarative_items . 0)) (CASE . (simple_declarative_items . 0)) (FOR . (simple_declarative_items . 0)) (NULL . (simple_declarative_items . 0)))
      ((default . error) (END .  102) (IDENTIFIER .  39) (NULL .  42) (CASE .  40) (FOR .  41))
      ((default . error) (AMPERSAND .  86) (SEMICOLON .  101))
      ((default . error) (RIGHT_PAREN .  100))
      ((default . error) (STRING_LITERAL .  96) (OTHERS .  97))
      ((default . error) (END . (case_items . 0)) (WHEN . (case_items . 0)))
      ((default . error) (END .  94) (WHEN .  82))
      ((default . error) (LEFT_PAREN .  65) (STRING_LITERAL .  18) (EXTERNAL .  19) (IDENTIFIER .  17) (PROJECT .  20))
      ((default . error) (LEFT_PAREN .  65) (STRING_LITERAL .  18) (EXTERNAL .  19) (IDENTIFIER .  17) (PROJECT .  20))
      ((default . error) (WHEN . (simple_declarative_item . 0)) (TYPE . (simple_declarative_item . 0)) (PACKAGE . (simple_declarative_item . 0)) (NULL . (simple_declarative_item . 0)) (FOR . (simple_declarative_item . 0)) (CASE . (simple_declarative_item . 0)) (IDENTIFIER . (simple_declarative_item . 0)) (END . (simple_declarative_item . 0)))
      ((default . error) (SEMICOLON . (term . 1)) (AMPERSAND . (term . 1)))
      ((default . error) (COMMA .  30) (RIGHT_PAREN .  91))
      ((default . error) (SEMICOLON . (attribute_reference . 1)) (AMPERSAND . (attribute_reference . 1)) (COMMA . (attribute_reference . 1)) (RIGHT_PAREN . (attribute_reference . 1)))
      ((default . error) (AMPERSAND . (term . 2)) (SEMICOLON . (term . 2)))
      ((default . error) (SEMICOLON . (expression . 1)) (AMPERSAND . (expression . 1)))
      ((default . error) (AMPERSAND .  86) (SEMICOLON .  111))
      ((default . error) (CASE .  110))
      ((default . error) (WHEN . (case_items . 1)) (END . (case_items . 1)))
      ((default . error) (VERTICAL_BAR . (discrete_choice . 0)) (EQUAL_GREATER . (discrete_choice . 0)))
      ((default . error) (VERTICAL_BAR . (discrete_choice . 1)) (EQUAL_GREATER . (discrete_choice . 1)))
      ((default . error) (EQUAL_GREATER . (discrete_choice_list . 0)) (VERTICAL_BAR . (discrete_choice_list . 0)))
      ((default . error) (VERTICAL_BAR .  109) (EQUAL_GREATER .  108))
      ((default . error) (USE .  107))
      ((default . error) (WHEN . (attribute_declaration . 0)) (END . (attribute_declaration . 0)) (TYPE . (attribute_declaration . 0)) (PACKAGE . (attribute_declaration . 0)) (NULL . (attribute_declaration . 0)) (FOR . (attribute_declaration . 0)) (CASE . (attribute_declaration . 0)) (IDENTIFIER . (attribute_declaration . 0)))
      ((default . error) (IDENTIFIER .  106))
      ((default . error) (NULL . (simple_declarative_items . 1)) (FOR . (simple_declarative_items . 1)) (CASE . (simple_declarative_items . 1)) (IDENTIFIER . (simple_declarative_items . 1)) (END . (simple_declarative_items . 1)))
      ((default . error) (COMMA .  30) (RIGHT_PAREN .  105))
      ((default . error) (SEMICOLON .  117))
      ((default . error) (SEMICOLON .  116))
      ((default . error) (LEFT_PAREN .  65) (STRING_LITERAL .  18) (EXTERNAL .  19) (IDENTIFIER .  17) (PROJECT .  20))
      ((default . error) (TYPE .  44) (IDENTIFIER .  39) (NULL .  42) (CASE .  40) (FOR .  41) (PACKAGE .  43))
      ((default . error) (STRING_LITERAL .  96) (OTHERS .  97))
      ((default . error) (SEMICOLON .  112))
      ((default . error) (WHEN . (simple_declarative_item . 1)) (TYPE . (simple_declarative_item . 1)) (PACKAGE . (simple_declarative_item . 1)) (NULL . (simple_declarative_item . 1)) (FOR . (simple_declarative_item . 1)) (CASE . (simple_declarative_item . 1)) (IDENTIFIER . (simple_declarative_item . 1)) (END . (simple_declarative_item . 1)))
      ((default . error) (WHEN . (case_statement . 0)) (END . (case_statement . 0)) (TYPE . (case_statement . 0)) (PACKAGE . (case_statement . 0)) (NULL . (case_statement . 0)) (FOR . (case_statement . 0)) (CASE . (case_statement . 0)) (IDENTIFIER . (case_statement . 0)))
      ((default . error) (EQUAL_GREATER . (discrete_choice_list . 1)) (VERTICAL_BAR . (discrete_choice_list . 1)))
      ((default . error) (END . (case_item . 0)) (WHEN . (case_item . 0)) (TYPE .  44) (IDENTIFIER .  39) (NULL .  42) (CASE .  40) (FOR .  41) (PACKAGE .  43))
      ((default . error) (AMPERSAND .  86) (SEMICOLON .  118))
      ((default . error) (WHEN . (package_spec . 0)) (END . (package_spec . 0)) (TYPE . (package_spec . 0)) (PACKAGE . (package_spec . 0)) (NULL . (package_spec . 0)) (FOR . (package_spec . 0)) (CASE . (package_spec . 0)) (IDENTIFIER . (package_spec . 0)))
      ((default . error) (WHEN . (typed_string_declaration . 0)) (END . (typed_string_declaration . 0)) (IDENTIFIER . (typed_string_declaration . 0)) (CASE . (typed_string_declaration . 0)) (FOR . (typed_string_declaration . 0)) (NULL . (typed_string_declaration . 0)) (PACKAGE . (typed_string_declaration . 0)) (TYPE . (typed_string_declaration . 0)))
      ((default . error) (WHEN . (attribute_declaration . 1)) (IDENTIFIER . (attribute_declaration . 1)) (CASE . (attribute_declaration . 1)) (FOR . (attribute_declaration . 1)) (NULL . (attribute_declaration . 1)) (PACKAGE . (attribute_declaration . 1)) (TYPE . (attribute_declaration . 1)) (END . (attribute_declaration . 1)))]
     [((compilation_unit . 8)(context_clause . 9)(project_declaration . 10)(project_qualifier . 11)(simple_project_declaration . 12)(with_clause . 13))
      nil
      nil
      nil
      nil
      nil
      nil
      ((attribute_prefix . 21)(attribute_reference . 22)(external_value . 23)(name . 24)(string_expression . 25)(string_list . 26))
      nil
      ((project_declaration . 14)(simple_project_declaration . 12)(with_clause . 15))
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
      nil
      nil
      ((attribute_declaration . 45)(case_statement . 46)(declarative_item . 47)(declarative_items . 48)(package_declaration . 49)(package_spec . 50)(simple_declarative_item . 51)(typed_string_declaration . 52))
      ((attribute_prefix . 21)(attribute_reference . 22)(external_value . 23)(name . 24)(string_expression . 38))
      nil
      nil
      nil
      ((attribute_prefix . 21)(attribute_reference . 22)(external_value . 23)(name . 24)(string_expression . 25)(string_list . 35))
      nil
      nil
      nil
      nil
      nil
      ((name . 59))
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      ((attribute_declaration . 45)(case_statement . 46)(declarative_item . 54)(package_declaration . 49)(package_spec . 50)(simple_declarative_item . 51)(typed_string_declaration . 52))
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
      ((attribute_prefix . 21)(attribute_reference . 22)(expression . 66)(external_value . 23)(name . 24)(string_expression . 67)(term . 68))
      nil
      nil
      nil
      ((attribute_prefix . 21)(attribute_reference . 22)(external_value . 23)(name . 24)(string_expression . 25)(string_list . 89))
      nil
      nil
      nil
      nil
      ((case_item . 83)(case_items . 84))
      nil
      ((attribute_prefix . 21)(attribute_reference . 22)(expression . 80)(external_value . 23)(name . 24)(string_expression . 67)(term . 68))
      ((attribute_declaration . 45)(case_statement . 46)(simple_declarative_item . 78)(simple_declarative_items . 79))
      nil
      nil
      nil
      ((attribute_prefix . 21)(attribute_reference . 22)(external_value . 23)(name . 24)(string_expression . 25)(string_list . 104))
      nil
      ((attribute_declaration . 45)(case_statement . 46)(simple_declarative_item . 103))
      nil
      nil
      ((discrete_choice . 98)(discrete_choice_list . 99))
      nil
      ((case_item . 95))
      ((attribute_prefix . 21)(attribute_reference . 22)(expression . 93)(external_value . 23)(name . 24)(string_expression . 67)(term . 68))
      ((attribute_prefix . 21)(attribute_reference . 22)(external_value . 23)(name . 24)(string_expression . 67)(term . 92))
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
      nil
      nil
      nil
      ((attribute_prefix . 21)(attribute_reference . 22)(expression . 115)(external_value . 23)(name . 24)(string_expression . 67)(term . 68))
      ((attribute_declaration . 45)(case_statement . 46)(declarative_item . 47)(declarative_items . 114)(package_declaration . 49)(package_spec . 50)(simple_declarative_item . 51)(typed_string_declaration . 52))
      ((discrete_choice . 113))
      nil
      nil
      nil
      nil
      ((attribute_declaration . 45)(case_statement . 46)(declarative_item . 54)(package_declaration . 49)(package_spec . 50)(simple_declarative_item . 51)(typed_string_declaration . 52))
      nil
      nil
      nil
      nil]))
  "Parser table.")

(provide 'gpr-grammar-wy)

;; end of file
