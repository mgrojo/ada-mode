--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2019 - 2021 Free Software Foundation, Inc.
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

with Wisitoken_Grammar_1_Process_Actions;
package body Wisi.WisiToken_Grammar is

   Check_Parens_Action_Index : constant String := "0";

   procedure Initialize (Data : in out Parse_Data_Type)
   is
      use all type Wisitoken_Grammar_1_Process_Actions.Token_Enum_ID;
   begin
      Data.First_Comment_ID := +COMMENT_ID;
   end Initialize;

   overriding
   procedure Initialize_Partial_Parse
     (Data              : in out Parse_Data_Type;
      Trace             : in     WisiToken.Trace_Access;
      Post_Parse_Action : in     Post_Parse_Action_Type;
      Begin_Line        : in     WisiToken.Line_Number_Type;
      End_Line          : in     WisiToken.Line_Number_Type)
   is begin
      Wisi.Initialize_Partial_Parse (Wisi.Parse_Data_Type (Data), Trace, Post_Parse_Action, Begin_Line, End_Line);

      Initialize (Data);
   end Initialize_Partial_Parse;

   overriding
   procedure Initialize_Full_Parse
     (Data     : in out Parse_Data_Type;
      Trace    : in     WisiToken.Trace_Access;
      End_Line : in     WisiToken.Line_Number_Type)
   is begin
      Wisi.Initialize_Full_Parse (Wisi.Parse_Data_Type (Data), Trace, End_Line);

      Initialize (Data);
   end Initialize_Full_Parse;

   procedure Check_Parens
     (Data    : in out Wisi.Parse_Data_Type'Class;
      Tree    : in     WisiToken.Syntax_Trees.Tree;
      Nonterm : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Args    : in     Arg_Index_Array)
   is
      use WisiToken;
   begin
      for Index of Args loop
         declare
            Char_Region : constant Buffer_Region := Tree.Char_Region
              (Tree.First_Terminal (Tree.Child (Nonterm, Index)));
         begin
            Data.Put_Language_Action
              (Check_Parens_Action_Index & Buffer_Pos'Image (Char_Region.First) &
                 Buffer_Pos'Image (Char_Region.Last));
         end;
      end loop;
   end Check_Parens;

end Wisi.WisiToken_Grammar;
