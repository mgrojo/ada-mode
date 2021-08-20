--  Abstract :
--
--  Grammar in parse action routines.
--
--  Copyright (C) 2017 - 2021 Free Software Foundation, Inc.
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

with WisiToken.Syntax_Trees;
package WisiToken.In_Parse_Actions is

   type Status_Label is
     (Ok,
      Missing_Name_Error, -- block start has name, required block end name missing
      Extra_Name_Error,   -- block start has no name, end has one
      Match_Names_Error); -- both names present, but don't match

   subtype Error is Status_Label range Status_Label'Succ (Ok) .. Status_Label'Last;

   type Status (Label : Status_Label := Ok) is record
      case Label is
      when Ok =>
         null;

      when Error =>
         Begin_Name : Positive_Index_Type;
         End_Name   : Positive_Index_Type;
      end case;
   end record;

   subtype Error_Status is Status
   with Dynamic_Predicate => Error_Status.Label /= Ok;

   function Image
     (Item       : in Status;
      Tree       : in Syntax_Trees.Tree'Class;
      Error_Node : in Syntax_Trees.Valid_Node_Access)
     return String;

   type In_Parse_Action is access function
     (Tree           : in     Syntax_Trees.Tree;
      Nonterm        : in out Syntax_Trees.Recover_Token;
      Tokens         : in     Syntax_Trees.Recover_Token_Array;
      Recover_Active : in     Boolean)
     return Status;
   --  Called during parsing and error recovery to implement higher level
   --  checks, such as block name matching in Ada.

   function Match_Names
     (Tree         : in Syntax_Trees.Tree;
      Tokens       : in Syntax_Trees.Recover_Token_Array;
      Start_Index  : in Positive_Index_Type;
      End_Index    : in Positive_Index_Type;
      End_Optional : in Boolean)
     return Status;
   --  Check that buffer text at Tokens (Start_Index).Name matches buffer
   --  text at Tokens (End_Index).Name. Comparison is controlled by
   --  Descriptor.Case_Insensitive.

   function Propagate_Name
     (Tree       : in     Syntax_Trees.Tree;
      Nonterm    : in out Syntax_Trees.Recover_Token;
      Tokens     : in     Syntax_Trees.Recover_Token_Array;
      Name_Index : in     Positive_Index_Type)
     return Status;
   function Merge_Names
     (Tree       : in     Syntax_Trees.Tree;
      Nonterm    : in out Syntax_Trees.Recover_Token;
      Tokens     : in     Syntax_Trees.Recover_Token_Array;
      Name_Index : in     Positive_Index_Type)
     return Status
   renames Propagate_Name;
   --  Set Nonterm.Name to Tokens (Name_Index).Name, or .Byte_Region, if
   --  .Name is Null_Buffer_Region. Return Ok.

   function Merge_Names
     (Tree        : in     Syntax_Trees.Tree;
      Nonterm     : in out Syntax_Trees.Recover_Token;
      Tokens      : in     Syntax_Trees.Recover_Token_Array;
      First_Index : in     Positive_Index_Type;
      Last_Index  : in     Positive_Index_Type)
     return Status;
   --  Set Nonterm.Name to the merger of Tokens (First_Index ..
   --  Last_Index).Name, return Ok.
   --
   --  If Tokens (Last_Index).Name is Null_Buffer_Region, use Tokens
   --  (Last_Index).Byte_Region instead.

   function Terminate_Partial_Parse
     (Tree                    : in Syntax_Trees.Tree;
      Partial_Parse_Active    : in Boolean;
      Partial_Parse_Byte_Goal : in Buffer_Pos;
      Recover_Active          : in Boolean;
      Nonterm                 : in Syntax_Trees.Recover_Token)
     return Status;
   pragma Inline (Terminate_Partial_Parse);
   --  If partial parse is complete, raise Wisitoken.Partial_Parse;
   --  otherwise return Ok.

end WisiToken.In_Parse_Actions;
