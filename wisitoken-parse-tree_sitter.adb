--  Abstract :
--
--  Ada binding to tree-sitter runtime.
--
--  Copyright (C) 2020 Free Software Foundation All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (Modified_GPL);

with Ada.IO_Exceptions;
with System;
package body WisiToken.Parse.Tree_Sitter is

   --  function TS_Node_Is_Null (Node : in Syntax_Tree_Node) return Interfaces.C.Extensions.bool
   --  with Import     => True,
   --    External_Name => "ts_node_is_null",
   --    Convention    => C;

   --  function TS_Node_Is_Missing (Node : in Syntax_Tree_Node) return Interfaces.C.Extensions.bool
   --  with Import     => True,
   --    External_Name => "ts_node_is_missing",
   --    Convention    => C;

   function TS_Node_Is_Extra (Node : in Syntax_Tree_Node) return Interfaces.C.Extensions.bool
   with Import     => True,
     External_Name => "ts_node_is_extra",
     Convention    => C;

   --  function TS_Node_Has_Error (Node : in Syntax_Tree_Node) return Interfaces.C.Extensions.bool
   --  with Import     => True,
   --    External_Name => "ts_node_has_error",
   --    Convention    => C;

   --  function Parent (Node : in Syntax_Tree_Node) return Syntax_Tree_Node
   --  --  null if none
   --  with Import     => True,
   --    External_Name => "ts_node_parent",
   --    Convention    => C;

   function TS_Node_Child (Node : in Syntax_Tree_Node; Child_Index : in Interfaces.Unsigned_32) return Syntax_Tree_Node
   --  0 origin, null if none
   with Import     => True,
     External_Name => "ts_node_child",
     Convention    => C;

   function TS_Node_Child_Count (Node : in Syntax_Tree_Node) return Interfaces.Unsigned_32
   with Import     => True,
     External_Name => "ts_node_child_count",
     Convention    => C;

   --  function TS_Node_Next_Sibling (Node : in Syntax_Tree_Node) return Syntax_Tree_Node
   --  --  null if none
   --  with Import     => True,
   --    External_Name => "ts_node_next_sibling",
   --    Convention    => C;

   ----------
   --  Body subprograms, random order.

   function Count_Terminals
     (Descriptor : in     WisiToken.Descriptor;
      Node       : in     Syntax_Tree_Node)
     return SAL.Base_Peek_Type
   is
      use all type Interfaces.Unsigned_32;
      use all type SAL.Base_Peek_Type;

      subtype Terminal is Token_ID range Descriptor.First_Terminal .. Descriptor.Last_Terminal;
      subtype Nonterminal is Token_ID range Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal;
   begin
      if TS_Node_Is_Extra (Node) then
         --  non-grammar
         return 0;

      elsif ID (Node) in Terminal then
         return 1;

      elsif ID (Node) in Nonterminal | Error_Token_ID then
         return Result : SAL.Base_Peek_Type := 0 do
            for I in 1 .. TS_Node_Child_Count (Node) loop
               Result := @ + Count_Terminals (Descriptor, TS_Node_Child (Node, I - 1));
            end loop;
         end return;

      else
         --  something else
         return 0;
      end if;
   end Count_Terminals;

   procedure Get_Terminals
     (Descriptor : in     WisiToken.Descriptor;
      Node       : in     Syntax_Tree_Node;
      Result     : in out Node_Array;
      Last       : in out SAL.Base_Peek_Type)
   is
      use all type Interfaces.Unsigned_32;
      use all type SAL.Base_Peek_Type;

      subtype Terminal is Token_ID range Descriptor.First_Terminal .. Descriptor.Last_Terminal;
      subtype Nonterminal is Token_ID range Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal;
      ID : constant Token_ID := Tree_Sitter.ID (Node);
   begin
      if TS_Node_Is_Extra (Node) then
         --  non-grammar
         null;

      elsif ID in Terminal then
         Last := Last + 1;
         Result (Last) := Node;

      elsif ID in Nonterminal | Error_Token_ID then
         for I in 1 .. TS_Node_Child_Count (Node) loop
            Get_Terminals (Descriptor, TS_Node_Child (Node, I - 1), Result, Last);
         end loop;

      else
         --  Non-grammar
         null;
      end if;
   end Get_Terminals;

   ----------
   --  Spec subprograms, declaration order

   function ID (Node : in Syntax_Tree_Node) return Token_ID
   is
      function TS_Node_Symbol (Node : in Syntax_Tree_Node) return Interfaces.Unsigned_16
      with Import     => True,
        External_Name => "ts_node_symbol",
        Convention    => C;
   begin
      return Token_ID (TS_Node_Symbol (Node));
   end ID;

   function Byte_Region (Node : in Syntax_Tree_Node) return Buffer_Region
   is
      function TS_Node_Start_Byte (Node : in Syntax_Tree_Node) return Interfaces.Unsigned_32
      with Import     => True,
        External_Name => "ts_node_start_byte",
        Convention    => C;

      function TS_Node_End_Byte (Node : in Syntax_Tree_Node) return Interfaces.Unsigned_32
      with Import     => True,
        External_Name => "ts_node_end_byte",
        Convention    => C;
   begin
      return
        (First => Buffer_Pos (TS_Node_Start_Byte (Node)),
         Last  => Buffer_Pos (TS_Node_End_Byte (Node)));
   end Byte_Region;

   function Get_Terminals
     (Node       : in Syntax_Tree_Node;
      Descriptor : in WisiToken.Descriptor)
     return Node_Array
   is
      Last : SAL.Base_Peek_Type := 0;
   begin
      return Result : Node_Array (1 .. Count_Terminals (Descriptor, Node)) do
         Get_Terminals (Descriptor, Node, Result, Last);
      end return;
   end Get_Terminals;

   function Root (Tree : in Syntax_Tree) return Syntax_Tree_Node
   is
      function TS_Tree_Root_Node (Tree : in TS_Tree) return Syntax_Tree_Node
      with Import     => True,
        External_Name => "ts_tree_root_node",
        Convention    => C;
   begin
      return TS_Tree_Root_Node (Tree.TS_Tree);
   end Root;

   overriding procedure Initialize (Parser : in out Tree_Sitter.Parser)
   is
      function TS_Parser_New return Interfaces.C.Extensions.void_ptr
      with Import     => True,
        External_Name => "ts_parser_new",
        Convention    => C;

      function TS_Parser_Set_Language
        (Self     : in Interfaces.C.Extensions.void_ptr;
         Language : in Interfaces.C.Extensions.void_ptr)
        return Interfaces.C.Extensions.bool
      with Import     => True,
        External_Name => "ts_parser_set_language",
        Convention    => C;
   begin
      Parser.TS_Parser := TS_Parser_New;
      if TS_Parser_Set_Language (Parser.TS_Parser, Parser.Language.all) then
         null;
      else
         raise Ada.IO_Exceptions.Use_Error with "tree-sitter language/library version mismatch";
      end if;
   end Initialize;

   overriding procedure Finalize (Parser : in out Tree_Sitter.Parser)
   is
      use all type System.Address;

      procedure TS_Tree_Delete (Self : in TS_Tree)
      with Import     => True,
        External_Name => "ts_tree_delete",
        Convention    => C;

      procedure TS_Parser_Delete (Self : in Interfaces.C.Extensions.void_ptr)
      with Import     => True,
        External_Name => "ts_parser_delete",
        Convention    => C;
   begin
      if Parser.Tree.TS_Tree /= System.Null_Address then
         TS_Tree_Delete (Parser.Tree.TS_Tree);
         Parser.Tree.TS_Tree := System.Null_Address;

         TS_Parser_Delete (Parser.TS_Parser);
         Parser.TS_Parser := System.Null_Address;

         GNATCOLL.Mmap.Free (Parser.Region);
         GNATCOLL.Mmap.Close (Parser.File);
      end if;
   end Finalize;

   procedure Parse
     (Parser    : in out Tree_Sitter.Parser;
      File_Name : in     String)
   is
      use GNATCOLL.Mmap;

      function TS_Parser_Parse_String
        (Self     : in Interfaces.C.Extensions.void_ptr;
         Old_Tree : in TS_Tree;
         String   : in Interfaces.C.Extensions.void_ptr;
         Length   : in Interfaces.Unsigned_32)
        return TS_Tree
      with Import     => True,
        External_Name => "ts_parser_parse_string",
        Convention    => C;

   begin
      Parser.File   := Open_Read (File_Name);
      Parser.Region := Read (Parser.File);

      Parser.Tree.TS_Tree := TS_Parser_Parse_String
        (Parser.TS_Parser, System.Null_Address, Data (Parser.Region).all'Address,
         Interfaces.Unsigned_32 (Short.Last (Parser.Region)));
   end Parse;

   function Buffer_Text (Parser : in Tree_Sitter.Parser; Region : in Buffer_Region) return String
   is begin
      --  Region is 1 origin
      return GNATCOLL.Mmap.Short.Data (Parser.Region) (Integer (Region.First) .. Integer (Region.Last));
   end Buffer_Text;

end WisiToken.Parse.Tree_Sitter;
