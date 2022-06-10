--  Abstract :
--
--  Packrat Parser type.
--
--  Packrat error recovery strategy is to build an LR parse state at
--  the error point, run LR error recovery, then merge the result into
--  the packrat parser state, running parallel packrat parsers to
--  match the parallel LR parsers if there are multiple error recover
--  solutions.
--
--  Copyright (C) 2022 Free Software Foundation All Rights Reserved.
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

pragma License (Modified_GPL);

with SAL.Gen_Unbounded_Definite_Red_Black_Trees;
with WisiToken.Parse.Parser;
package WisiToken.Parse.Packrat.Parser is
   use all type WisiToken.Syntax_Trees.Node_Index;

   type Recover_Op_Nodes is record
      --  The subset of Parse.Recover_Op required by the packrat parser to
      --  alter the input stream after error recover.

      Op : Insert_Delete_Op_Label := Insert;

      ID : Token_ID := Invalid_Token_ID;
      --  The token ID inserted or deleted.

      Node : Syntax_Trees.Node_Access := Syntax_Trees.Invalid_Node_Access;
      --  For Insert, the parsed node holding the inserted token.
      --  For Delete, the deleted node. Node.Parent is the previous non-deleted terminal.

      Pos : Syntax_Trees.Node_Index := Syntax_Trees.Invalid_Node_Index;
      --  For Insert, ID is inserted before Pos in the Shared_Stream.
      --  For Delete, the token at Pos is deleted.

      Error_Pos : Buffer_Pos := Invalid_Buffer_Pos;
      --  Position of the error that is repaired by this op. Used by the
      --  editor to apply the op to the source text.

   end record;

   package Recover_Op_Nodes_Lists is new SAL.Gen_Definite_Doubly_Linked_Lists (Recover_Op_Nodes);

   function To_Key (Element : in Recover_Op_Nodes_Lists.List) return Syntax_Trees.Node_Index
   is (Element (Element.First).Pos)
   with Pre => (for all Item of Element => Item.Pos = Element (Element.First).Pos);

   function Compare is new SAL.Gen_Compare_Integer (Syntax_Trees.Node_Index);

   package Recover_Op_Nodes_Trees is new SAL.Gen_Unbounded_Definite_Red_Black_Trees
     (Element_Type => Recover_Op_Nodes_Lists.List,
      Key_Type     => Syntax_Trees.Node_Index,
      Key          => To_Key,
      Key_Compare  => Compare);
   --  There can be only one Delete at Pos, but multiple Inserts; Delete
   --  is always first in the list.

   function Image (Item : in Recover_Op_Nodes; Tree : in Syntax_Trees.Tree) return String;

   type Parser (First_Nonterminal, Last_Nonterminal : Token_ID) is abstract new WisiToken.Parse.Parser.Parser
     (First_Nonterminal => First_Nonterminal,
      Last_Nonterminal  => Last_Nonterminal)
   with record
      Direct_Left_Recursive : Token_ID_Array_Token_ID_Set_Access (First_Nonterminal .. Last_Nonterminal);

      Derivs                : Packrat.Derivs (First_Nonterminal .. Last_Nonterminal);
      Insert_Delete : aliased Recover_Op_Nodes_Trees.Tree;
      --  Recover_Insert_Delete is not emptied between error recovery
      --  sessions, because later parse operations can examine the same
      --  inputs.
   end record;

   overriding procedure Finalize (Object : in out Parser);

   function Delete_Valid (Parser : in Packrat.Parser.Parser; Pos : in Syntax_Trees.Stream_Index) return Boolean;
   --  Return True if Pos in Tree.Shared_Stream should be skipped.

   function Has_Input (Parser : in Packrat.Parser.Parser; Pos : in Syntax_Trees.Stream_Index) return Boolean;
   --  Return True if there are virtual terminals inserted before Pos.

   type ID_Node_Type is record
      ID   : Token_ID := Invalid_Token_ID;
      Node : Syntax_Trees.Node_Access;
   end record;

   function Input_Op
     (Parser    : in Packrat.Parser.Parser;
      Pos       : in Syntax_Trees.Stream_Index;
      Prev_Node : in Syntax_Trees.Node_Access)
     return ID_Node_Type
   with Pre => Parser.Has_Input (Pos);
   --  Prev_Node is the terminal before the production terminal being
   --  tested; invalid_node_Access if the production terminal is the
   --  first terminal in a production. This determines which of several
   --  inserts at pos is returned. Result.ID is Invalid_Token_ID if no
   --  inserted virtual is after Prev_Node; Pos is the input token to
   --  test.

   procedure Packrat_Parse
     (Shared_Parser : in out Parser;
      Log_File      : in     Ada.Text_IO.File_Type);
   --  Call Lex_All, then run the packrat parse algorithm.
   --  If it fails, run the LR error recover algorithm thru resume done,
   --  then resume packrat parse. Errors are reported as in LR_Parse.

   procedure Packrat_Parse_No_Recover
     (Parser : in out WisiToken.Parse.Packrat.Parser.Parser;
      Resume : in     Boolean)
   is abstract;
   --  If not Resume, call Lex_All, initialize Parser.
   --
   --  Then run the packrat parse algorithm, with no error recover.
   --
   --  Raise Parse_Error for a parse error; leave Parser.Derivs intact
   --  for error recover.
   --
   --  Resume => True is for resuming packrat parsing after LR error
   --  recover.

   procedure Finish_Parse (Parser : in out Packrat.Parser.Parser'Class; Result : in out Memo_Entry);
   --  Call Tree.Set_Root, Clear_Parse_Streams, Tree.Insert/Delete_Token;
   --  raise Parse_Error with an error message if the parse did not
   --  succeed.

   ----------
   --  Debugging

   procedure Print_Derivs (Parser : in WisiToken.Parse.Packrat.Parser.Parser);
   --  Print Parser.Derivs to Parser.Tree.Lexer.Trace.

end WisiToken.Parse.Packrat.Parser;
