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
--  Copyright (C) 2018, 2020 - 2022 Free Software Foundation, Inc.
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
--  can experiment with error recovery.

pragma License (Modified_GPL);
with WisiToken.Syntax_Trees;
limited with WisiToken.Parse.Parser;
package WisiToken.Parse.Packrat is

   type Memo_State is (No_Result, Failure, Success);
   subtype Result_States is Memo_State range Failure .. Success;

   type Memo_Entry (State : Memo_State := No_Result) is record
      Max_Examined_Pos : Syntax_Trees.Stream_Index;
      --  For error message.

      case State is
      when No_Result =>
         Recursive : Boolean := False;

      when Failure =>
         null;

      when Success =>
         Result   : Syntax_Trees.Node_Access;
         Last_Pos : Syntax_Trees.Stream_Index; -- Last terminal in Result

      end case;
   end record;
   subtype Success_Memo_Entry is Memo_Entry (Success);

   No_Result_Memo : constant Memo_Entry := (No_Result, WisiToken.Syntax_Trees.Invalid_Stream_Index, False);

   function Image_Pos
     (Tree    : in Syntax_Trees.Tree;
      Stream  : in Syntax_Trees.Stream_ID;
      Element : in Syntax_Trees.Stream_Index)
     return String
   with Pre => Tree.Contains (Stream, Element);
   --  "0" for Invalid_Stream_Index, Node_Index'Image otherwise.

   function Image (Item : in Memo_Entry; Tree : in Syntax_Trees.Tree) return String;

   function Image
     (Item    : in Memo_Entry;
      Nonterm : in Token_ID;
      Pos     : in Syntax_Trees.Node_Index;
      Tree    : in Syntax_Trees.Tree)
     return String;

   function Image
     (Item    : in Memo_Entry;
      Nonterm : in Token_ID;
      Pos     : in Syntax_Trees.Stream_Index;
      Tree    : in Syntax_Trees.Tree)
     return String;

   subtype Positive_Node_Index is Syntax_Trees.Node_Index range 1 .. Syntax_Trees.Node_Index'Last;
   package Memos is new SAL.Gen_Unbounded_Definite_Vectors
     (Positive_Node_Index, Memo_Entry, Default_Element => (others => <>));
   --  Memos is indexed by Node_Index of terminals in Shared_Stream
   --  (incremental parse is not supported).

   type Derivs is array (Token_ID range <>) of Memos.Vector;

   procedure Clear (Derivs : in out Packrat.Derivs);
   --  Free memory allocated by Derivs; set all to Empty_Vector.

   procedure Set_Deriv
     (Derivs  : in out Packrat.Derivs;
      Nonterm : in     Token_ID;
      Pos     : in     Positive_Node_Index;
      Memo    : in     Memo_Entry);
   --  Add or replace Derivs (Nonterm)(Pos).

   procedure Finish_Parse (Parser : in out WisiToken.Parse.Parser.Parser'Class; Result : in out Memo_Entry);
   --  Call Tree.Set_Root, Clear_Parse_Streams; raise Parse_Error with an
   --  error message if the parse did not succeed.

end WisiToken.Parse.Packrat;
