--  Abstract :
--
--  Types and operations for a packrat parser.
--
--  References:
--
--  [1] Bryan Ford thesis http://bford.info/pub/lang/thesis
--
--  [2] AdaCore langkit   https://github.com/adacore/langkit
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
--  [1] uses Haskell lazy evaluation, and does not use a lexer. We use
--  a lexer to reduce the memory requirement. Although eliminating the
--  lexer would make it easier to support additional syntax for a
--  preprocessor or template generator.
--
--  [2] uses a lexer, and implements lazy evaluation via Memo_State,
--  Memo_Entry as we do here, except that their result type is a
--  specific AST type provided by a generic parameter; we use the
--  general purpose Syntax_Tree.Tree type.
--
--  [2] also applies a memory optimization; it only saves the last 16
--  results for each nonterminal. We don't do that yet, so we can get
--  some data on how well that works.

pragma License (Modified_GPL);
with WisiToken.Syntax_Trees;
package WisiToken.Packrat is

   type Memo_State is (No_Result, Failure, Success);
   subtype Result_States is Memo_State range Failure .. Success;

   type Memo_Entry (State : Memo_State := No_Result) is record
      case State is
      when No_Result | Failure =>
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
   --  nonterms are reduced, so there is a one-to-one mapping.

end WisiToken.Packrat;
