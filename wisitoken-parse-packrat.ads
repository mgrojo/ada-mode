--  Abstract :
--
--  Types and operations for a packrat parser runtime.
--
--  References:
--
--  [ford thesis] Bryan Ford thesis http://bford.info/pub/lang/thesis
--
--  [langkit]     AdaCore langkit   https://github.com/adacore/langkit
--
--  [tratt 2010]  http://tratt.net/laurie/research/pubs/papers/
--                tratt__direct_left_recursive_parsing_expression_grammars.pdf
--
--  [warth 2008]  Warth, A., Douglass, J.R. and Millstein, T.D., 2008. Packrat
--                parsers can support left recursion. PEPM, 8, pp.103-110.
--
--  Copyright (C) 2018 Stephen Leake All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

--  Design:
--
--  [ford thesis] uses Haskell lazy evaluation, and does not use a
--  lexer. We use a lexer to reduce the memory requirement. Although
--  eliminating the lexer would make it easier to support additional
--  syntax for a preprocessor or template generator.
--
--  [langkit] uses a lexer, and implements lazy evaluation via
--  Memo_State, Memo_Entry as we do here, except that their result
--  type is a specific AST type provided by a generic parameter; we
--  use the general purpose Syntax_Tree.Tree type.
--
--  [langkit] also applies a memory optimization; it only saves the
--  last 16 results for each nonterminal. We don't do that yet, so we
--  can get some data on how well that works.

pragma License (Modified_GPL);
with WisiToken.Syntax_Trees;
package WisiToken.Parse.Packrat is

   Recursive : exception;
   --  Raised when parser detects recursion at runtime.

   type Memo_State is (No_Result, Failure, Success);
   subtype Result_States is Memo_State range Failure .. Success;

   type Memo_Entry (State : Memo_State := No_Result) is record
      case State is
      when No_Result =>
         Recursive : Boolean := False;
         --  Set True on first access; used to detect recursion.

      when Failure =>
         null;

      when Success =>
         Result : aliased WisiToken.Syntax_Trees.Valid_Node_Index;

         Last_Token : Token_Index;
      end case;
   end record;

   package Memos is new SAL.Gen_Unbounded_Definite_Vectors (Token_Index, Memo_Entry);

   subtype Result_Type is Memo_Entry
   with Dynamic_Predicate => Result_Type.State in Result_States;

   function Tree_Index (Terminal_Index : in Token_Index) return Syntax_Trees.Valid_Node_Index
     is (Syntax_Trees.Valid_Node_Index (Terminal_Index));
   --  All tokens are read and entered into the syntax tree before any
   --  nonterms are reduced, so the mapping from token_index to
   --  tree node_index is identity.

   package Derivs is new SAL.Gen_Unbounded_Definite_Vectors (Token_ID, Memos.Vector);

   type Parse_WisiToken_Accept is access
     function (Parser : in out Base_Parser'Class; Last_Pos : in Base_Token_Index) return Result_Type;

   type Parser is new Base_Parser with record
      Base_Tree : aliased WisiToken.Syntax_Trees.Base_Tree;
      Tree      : WisiToken.Syntax_Trees.Tree;
      --  FIXME: Current we only need Base_Tree for Execute_Actions, except
      --  that Syntax_Trees only declares the needed operations on Tree. But
      --  we may need more trees for error recovery; if not, fix
      --  Syntax_Trees, move Base_Tree and Execute_Actions up to
      --  base_parser.

      Derivs : Packrat.Derivs.Vector;

      Parse_WisiToken_Accept : Packrat.Parse_WisiToken_Accept;
   end record;

   overriding procedure Parse (Parser : aliased in out Packrat.Parser);
   overriding function Any_Errors (Parser : in Packrat.Parser) return Boolean;
   overriding procedure Put_Errors (Parser : in Packrat.Parser; Input_File_Name : in String);
   overriding procedure Execute_Actions (Parser : in out Packrat.Parser);

end WisiToken.Parse.Packrat;
