--  Abstract :
--
--  see spec.
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

with Ada.Strings.Fixed;
package body WisiToken.Wisi_Runtime.Ada is

   procedure Set_Params (Params : in String)
   is
      use Standard.Ada.Strings.Fixed;
      First : Integer := Params'First;
      Last  : Integer := Index (Params, " ");
   begin
      Ada_Indent := Integer'Value (Params (First .. Last - 1));

      First := Last + 1;
      Last := Index (Params, " ", First);
      Ada_Indent_Broken := Integer'Value (Params (First .. Last - 1));

      First := Last + 1;
      Last := First + 1;
      Ada_Indent_Comment_Col_0 := Params (First) = '1';

      First := Last + 1;
      Last := First + 1;
      Ada_Indent_Comment_GNAT := Params (First) = '1';

      First := Last + 1;
      Last := Index (Params, " ", First);
      Ada_Indent_Label := Integer'Value (Params (First .. Last - 1));

      First := Last + 1;
      Last := Index (Params, " ", First);
      Ada_Indent_Record_Rel_Type := Integer'Value (Params (First .. Last - 1));

      First := Last + 1;
      Last := Index (Params, " ", First);
      Ada_Indent_Renames := Integer'Value (Params (First .. Last - 1));

      First := Last + 1;
      Last := Index (Params, " ", First);
      Ada_Indent_Return := Integer'Value (Params (First .. Last - 1));

      First := Last + 1;
      Last := Index (Params, " ", First);
      Ada_Indent_Use := Integer'Value (Params (First .. Last - 1));

      First := Last + 1;
      Last := Index (Params, " ", First);
      Ada_Indent_When := Integer'Value (Params (First .. Last - 1));

      First := Last + 1;
      Last := Index (Params, " ", First);
      Ada_Indent_With := Integer'Value (Params (First .. Last - 1));

      First := Last + 1;
      Ada_Indent_Hanging_Rel_Exp := Params (First) = '1';
   end Set_Params;

   function Ada_Indent_Aggregate
     (Args : in WisiToken.Wisi_Runtime.Indent_Arg_Arrays.Vector)
     return WisiToken.Wisi_Runtime.Delta_Type
   is begin
      raise SAL.Not_Implemented;
      return Null_Delta;
   end Ada_Indent_Aggregate;

   function Ada_Indent_Renames_0
     (Args : in WisiToken.Wisi_Runtime.Indent_Arg_Arrays.Vector)
     return WisiToken.Wisi_Runtime.Delta_Type
   is begin
      raise SAL.Not_Implemented;
      return Null_Delta;
   end Ada_Indent_Renames_0;

   function Ada_Indent_Return_0
     (Args : in WisiToken.Wisi_Runtime.Indent_Arg_Arrays.Vector)
     return WisiToken.Wisi_Runtime.Delta_Type
   is begin
      raise SAL.Not_Implemented;
      return Null_Delta;
   end Ada_Indent_Return_0;

   function Ada_Indent_Record_0
     (Args : in WisiToken.Wisi_Runtime.Indent_Arg_Arrays.Vector)
     return WisiToken.Wisi_Runtime.Delta_Type
   is begin
      raise SAL.Not_Implemented;
      return Null_Delta;
   end Ada_Indent_Record_0;

   function Ada_Indent_Record_1
     (Args : in WisiToken.Wisi_Runtime.Indent_Arg_Arrays.Vector)
     return WisiToken.Wisi_Runtime.Delta_Type
   is begin
      raise SAL.Not_Implemented;
      return Null_Delta;
   end Ada_Indent_Record_1;

end WisiToken.Wisi_Runtime.Ada;
