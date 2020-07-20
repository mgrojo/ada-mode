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

with Ada.Finalization;
with GNATCOLL.Mmap;
with Interfaces.C.Extensions;
package WisiToken.Parse.Tree_Sitter is

   type Language_Function is access function return Interfaces.C.Extensions.void_ptr
   with Convention => C;

   type Syntax_Tree_Node is private;
   --  Can't be tagged because it is a C type.

   Error_Token_ID  : constant Token_ID := 16#ffff#; --  parser.h ts_builtin_sym_error
   Tree_Sitter_EOI : constant Token_ID := 0;

   function ID (Node : in Syntax_Tree_Node) return Token_ID;
   function Byte_Region (Node : in Syntax_Tree_Node) return Buffer_Region;

   type Node_Array is array  (Positive_Index_Type range <>) of Syntax_Tree_Node;

   function Get_Terminals
     (Node       : in Syntax_Tree_Node;
      Descriptor : in WisiToken.Descriptor)
     return Node_Array;

   type Syntax_Tree is tagged private;
   --  Tagged for Object.Method notation.

   function Root (Tree : in Syntax_Tree) return Syntax_Tree_Node;

   type Parser (Language : Language_Function) is new Ada.Finalization.Limited_Controlled with private;

   overriding procedure Initialize (Parser : in out Tree_Sitter.Parser);
   overriding procedure Finalize (Parser : in out Tree_Sitter.Parser);

   procedure Parse
     (Parser    : in out Tree_Sitter.Parser;
      File_Name : in     String);

   function Buffer_Text (Parser : in Tree_Sitter.Parser; Region : in Buffer_Region) return String;

   function Tree (Parser : in Tree_Sitter.Parser) return Syntax_Tree'Class;

private

   subtype TS_Tree is Interfaces.C.Extensions.void_ptr;

   type Context_Array_4 is array (1 .. 4) of Interfaces.Unsigned_32;

   subtype Token_ID_Ptr is Interfaces.C.Extensions.void_ptr;

   type Syntax_Tree_Node is record
      --  tree-sitter api.h TSNode
      Context : Context_Array_4;
      Tree    : TS_Tree;
      ID      : Token_ID_Ptr;
   end record
   with Convention => C;

   type Syntax_Tree is tagged record
      TS_Tree : Tree_Sitter.TS_Tree;
   end record;

   type Parser (Language : Language_Function) is new Ada.Finalization.Limited_Controlled with record
      TS_Parser   : Interfaces.C.Extensions.void_ptr;
      File        : GNATCOLL.Mmap.Mapped_File;
      Region      : GNATCOLL.Mmap.Mapped_Region;
      Buffer_Last : Positive;
      Tree        : Syntax_Tree;
   end record;

   function Tree (Parser : in Tree_Sitter.Parser) return Syntax_Tree'Class
   is (Parser.Tree);

end WisiToken.Parse.Tree_Sitter;
