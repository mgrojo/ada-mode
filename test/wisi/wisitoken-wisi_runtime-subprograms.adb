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

   procedure Set_Params (Params : in String)
   is
      use Ada.Strings.Fixed;
      First : Integer := Params'First;
      Last  : Integer := Index (Source => Params, Pattern => " ");
      Temp  : Integer;
   begin
      Subp_Indent := Integer'Value (Params (First .. Last));

      First := Last;
      Last  := Index (Source => Params, Pattern => " ", From => Last + 1);

      Subp_Indent_Broken := Integer'Value (Params (First .. Last));

      First := Last;
      Last  := Params'Last;

      Temp := Integer'Value (Params (First .. Last));
      Subp_Indent_Comment_Col_0 := (if Temp = 1 then True else False);
   exception
      when E : Constraint_Error =>
         raise Constraint_Error with "params '" & Params & "' raised Constraint_Error: " &
           Ada.Exceptions.Exception_Message (E);
   end Set_Params;

   function Function_1
     (Args : in WisiToken.Wisi_Runtime.Indent_Arg_Arrays.Vector)
     return WisiToken.Wisi_Runtime.Delta_Type
   is
      pragma Unreferenced (Args);
   begin
      --  subprograms.el subp-indent-function
      return (Int, Subp_Indent_Broken);
   end Function_1;

end WisiToken.Wisi_Runtime.Subprograms;
