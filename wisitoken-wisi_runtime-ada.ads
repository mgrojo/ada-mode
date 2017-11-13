--  Abstract :
--
--  Ada implementation of:
--
--  [1] ada-wisi.el
--  [2] ada-indent-user-options.el
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

package WisiToken.Wisi_Runtime.Ada is

   --  Indent parameters from [2]
   Ada_Indent                 : Integer;
   Ada_Indent_Broken          : Integer;
   Ada_Indent_Comment_Col_0   : Boolean;
   Ada_Indent_Comment_GNAT    : Boolean;
   Ada_Indent_Label           : Integer;
   Ada_Indent_Record_Rel_Type : Integer;
   Ada_Indent_Renames         : Integer;
   Ada_Indent_Return          : Integer;
   Ada_Indent_Use             : Integer;
   Ada_Indent_When            : Integer;
   Ada_Indent_With            : Integer;
   Ada_Indent_Hanging_Rel_Exp : Boolean;

   procedure Set_Params (Params : in String);
   --  Set all indent parameters from Params, in declaration order.
   --  Boolean is represented by 0 | 1. Parameter values are space
   --  delimited.

   ----------
   --  The following are declared in ada.wy %elisp_indent

   function Ada_Indent_Aggregate
     (Args : in WisiToken.Wisi_Runtime.Indent_Arg_Arrays.Vector)
     return WisiToken.Wisi_Runtime.Delta_Type;

   function Ada_Indent_Renames_0
     (Args : in WisiToken.Wisi_Runtime.Indent_Arg_Arrays.Vector)
     return WisiToken.Wisi_Runtime.Delta_Type;

   function Ada_Indent_Return_0
     (Args : in WisiToken.Wisi_Runtime.Indent_Arg_Arrays.Vector)
     return WisiToken.Wisi_Runtime.Delta_Type;

   function Ada_Indent_Record_0
     (Args : in WisiToken.Wisi_Runtime.Indent_Arg_Arrays.Vector)
     return WisiToken.Wisi_Runtime.Delta_Type;

   function Ada_Indent_Record_1
     (Args : in WisiToken.Wisi_Runtime.Indent_Arg_Arrays.Vector)
     return WisiToken.Wisi_Runtime.Delta_Type;

end WisiToken.Wisi_Runtime.Ada;
