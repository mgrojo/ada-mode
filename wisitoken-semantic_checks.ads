--  Abstract :
--
--  Grammar semantic check routines.
--
--  Copyright (C) 2017, 2018 Stephen Leake All Rights Reserved.
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

with WisiToken.Lexer;
with WisiToken.Syntax_Trees;
package WisiToken.Semantic_Checks is

   type Check_Status_Label is (Ok, Error);

   type Error_Label is
     (Missing_Name_Error, -- block start has name, required block end name missing
      Extra_Name_Error,   -- block start has no name, end has one
      Match_Names_Error); -- both names present, but don't match

   type Error_Label_Set is array (Error_Label) of Boolean;

   function Image (Item : in Error_Label_Set) return String;

   type Check_Status (Label : Check_Status_Label := Check_Status_Label'First) is record
      case Label is
      when Ok =>
         null;

      when Error =>
         Code : Error_Label;

         Tokens : Syntax_Trees.Valid_Node_Index_Arrays.Vector;
         --  The tokens involved in the error; for example, for
         --  Match_Names_Error, the two name tokens.
      end case;

   end record;

   function Image (Item : in Check_Status) return String;

   type Semantic_Check is access function
     (Syntax_Tree : in out WisiToken.Syntax_Trees.Abstract_Tree'Class;
      Lexer       : in     WisiToken.Lexer.Handle;
      Nonterm     : in     WisiToken.Syntax_Trees.Valid_Node_Index;
      Tokens      : in     WisiToken.Syntax_Trees.Valid_Node_Index_Array)
     return Check_Status;
   --  Called during parsing and error recovery to implement higher level
   --  checks, such as block name matching in Ada.

   Null_Check : constant Semantic_Check := null;

   function Match_Names
     (Syntax_Tree  : in WisiToken.Syntax_Trees.Abstract_Tree'Class;
      Lexer        : in WisiToken.Lexer.Handle;
      Tokens       : in WisiToken.Syntax_Trees.Valid_Node_Index_Array;
      Start_Index  : in Ada.Containers.Count_Type;
      End_Index    : in Ada.Containers.Count_Type;
      End_Optional : in Boolean)
     return Check_Status;
   --  Check that buffer text at Tokens (Start_Index).Name matches buffer
   --  text at Tokens (End_Index).Name.

   function Propagate_Name
     (Syntax_Tree : in out WisiToken.Syntax_Trees.Abstract_Tree'Class;
      Nonterm     : in     WisiToken.Syntax_Trees.Valid_Node_Index;
      Tokens      : in     WisiToken.Syntax_Trees.Valid_Node_Index_Array;
      Name_Index  : in     Ada.Containers.Count_Type)
     return Check_Status;
   --  Set Syntax_Tree.Name (Nonterm) to Syntax_Tree.Name (Tokens
   --  (Name_Index)), return Ok.

   function Merge_Names
     (Syntax_Tree : in out WisiToken.Syntax_Trees.Abstract_Tree'Class;
      Nonterm     : in     WisiToken.Syntax_Trees.Valid_Node_Index;
      Tokens      : in     WisiToken.Syntax_Trees.Valid_Node_Index_Array;
      First_Index : in     Ada.Containers.Count_Type;
      Last_Index  : in     Ada.Containers.Count_Type)
     return Check_Status;
   --  Then set Nonterm.Name to the merger of Tokens (First_Index ..
   --  Last_Index).Name, return Ok.
   --
   --  If Tokens (Last_Index).Name is Null_Buffer_Region, use Tokens
   --  (Last_Index).Byte_Region instead.

end WisiToken.Semantic_Checks;
