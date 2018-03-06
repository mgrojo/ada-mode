--  Abstract :
--
--  Ada implementation of language-specific action parameters used in subprograms.wy
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

package WisiToken.Wisi_Runtime.Subprograms is

   --  Weird defaults, to test specifying these at runtime.
   Subp_Indent               : Integer := 0;
   Subp_Indent_Broken        : Integer := 0;
   Subp_Indent_Comment_Col_0 : Boolean := False;

   type Parse_Data_Type is new Wisi_Runtime.Parse_Data_Type with null record;

   overriding
   procedure Initialize
     (Data             : in out Parse_Data_Type;
      Descriptor       : access constant WisiToken.Descriptor'Class;
      Source_File_Name : in     String;
      Parse_Action     : in     Parse_Action_Type;
      Line_Count       : in     Line_Number_Type;
      Params           : in     String);
   --  Set all params from Params, in declaration order. Boolean is
   --  represented by 0 | 1. Parameter values are space delimited.

   function Function_1
     (Data              : in out Wisi_Runtime.Parse_Data_Type'Class;
      State             : in     Semantic_State.Semantic_State;
      Tree              : in     Syntax_Trees.Tree;
      Tree_Tokens       : in     Syntax_Trees.Valid_Node_Index_Array;
      Tree_Indenting    : in     Syntax_Trees.Valid_Node_Index;
      Indenting_Comment : in     Boolean;
      Args              : in     Wisi_Runtime.Indent_Arg_Arrays.Vector)
     return WisiToken.Wisi_Runtime.Delta_Type;

end WisiToken.Wisi_Runtime.Subprograms;
