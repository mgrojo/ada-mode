--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017 Stephen Leake All Rights Reserved.
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

with Ada.Exceptions;
with Ada.Strings.Fixed;
package body WisiToken.Wisi_Runtime.Subprograms is

   overriding
   procedure Initialize
     (Data             : in out Parse_Data_Type;
      Semantic_State   : in     WisiToken.Token_Line_Comment.State_Access;
      Source_File_Name : in     String;
      Parse_Action     : in     Parse_Action_Type;
      Line_Count       : in     Line_Number_Type;
      Params           : in     String)
   is
      use Ada.Strings.Fixed;
      First : Integer := Params'First;
      Last  : Integer := Index (Source => Params, Pattern => " ");
      Temp  : Integer;
   begin
      Wisi_Runtime.Initialize
        (Wisi_Runtime.Parse_Data_Type (Data), Semantic_State, Source_File_Name, Parse_Action, Line_Count, "");

      if Params /= "" then
         Subp_Indent := Integer'Value (Params (First .. Last));

         First := Last;
         Last  := Index (Source => Params, Pattern => " ", From => Last + 1);

         Subp_Indent_Broken := Integer'Value (Params (First .. Last));

         First := Last;
         Last  := Params'Last;

         Temp := Integer'Value (Params (First .. Last));
         Subp_Indent_Comment_Col_0 := (if Temp = 1 then True else False);
      end if;

      Data.Indent_Comment_Col_0 := Subp_Indent_Comment_Col_0;

   exception
      when E : Constraint_Error =>
         raise Constraint_Error with "params '" & Params & "' raised Constraint_Error: " &
           Ada.Exceptions.Exception_Message (E);
   end Initialize;

   function Function_1
     (Data              : in out Wisi_Runtime.Parse_Data_Type'Class;
      Tokens            : in     Semantic_State.Augmented_Token_Array;
      Indenting         : in     Token_Line_Comment.Token;
      Indenting_Comment : in     Boolean;
      Args              : in     Wisi_Runtime.Indent_Arg_Arrays.Vector)
     return WisiToken.Wisi_Runtime.Delta_Type
   is
      pragma Unreferenced (Data, Tokens, Indenting, Indenting_Comment, Args);
   begin
      --  subprograms.el subp-indent-function
      return (Simple, (Int, Subp_Indent_Broken));
   end Function_1;

end WisiToken.Wisi_Runtime.Subprograms;
