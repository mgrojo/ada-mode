--  Abstract :
--
--  See spec
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

with Ada.Strings.Fixed;
with Gpr_Process_Actions;
package body Wisi.Gpr is

   overriding
   procedure Initialize
     (Data  : in out Parse_Data_Type;
      Trace : in     WisiToken.Trace_Access)
   is
      use all type Gpr_Process_Actions.Token_Enum_ID;
   begin
      Wisi.Initialize (Wisi.Parse_Data_Type (Data), Trace);

      Data.First_Comment_ID := +COMMENT_ID;
      Data.Last_Comment_ID  := WisiToken.Invalid_Token_ID;
      Data.Left_Paren_ID    := WisiToken.Invalid_Token_ID;
      Data.Right_Paren_ID   := WisiToken.Invalid_Token_ID;
   end Initialize;

   overriding
   procedure Parse_Language_Params
     (Data   : in out Parse_Data_Type;
      Params : in     String)
   is
      pragma Unreferenced (Data);
      use Ada.Strings.Fixed;
      First : Integer := Params'First;
      Last  : Integer := Index (Params, " ");
   begin
      if Params /= "" then
         --  must match [1] wisi-parse-format-language-options
         Gpr_Indent := Integer'Value (Params (First .. Last - 1));

         First := Last + 1;
         Last := Index (Params, " ", First);
         Gpr_Indent_Broken := Integer'Value (Params (First .. Last - 1));

         First := Last + 1;
         Gpr_Indent_When := Integer'Value (Params (First .. Params'Last));
      end if;
   end Parse_Language_Params;

end Wisi.Gpr;
