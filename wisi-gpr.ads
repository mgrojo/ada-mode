--  Abstract :
--
--  Ada implementation of:
--
--  [1] gpr-wisi.el
--  [2] gpr-indent-user-options.el
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

package Wisi.Gpr is

   Language_Protocol_Version : constant String := "1";
   --  Defines the data passed to Initialize in Params.
   --
   --  This value must match gpr-wisi.el
   --  gpr-wisi-language-protocol-version.

   --  Indent parameters from [2]
   Gpr_Indent        : Integer := 3;
   Gpr_Indent_Broken : Integer := 2;
   Gpr_Indent_When   : Integer := 3;

   --  Other parameters
   End_Names_Optional : Boolean := False;

   type Parse_Data_Type is new Wisi.Parse_Data_Type with null record;

   overriding
   procedure Initialize_Partial_Parse
     (Data                : in out Parse_Data_Type;
      Trace               : in     WisiToken.Trace_Access;
      Post_Parse_Action   : in     Post_Parse_Action_Type;
      Action_Region_Bytes : in     WisiToken.Buffer_Region;
      Begin_Line          : in     WisiToken.Line_Number_Type;
      End_Line            : in     WisiToken.Line_Number_Type;
      Begin_Indent        : in     Integer);
   --  Call Wisi_Runtime.Initialize, also do any other initialization
   --  that Gpr_Data needs.

   overriding
   procedure Initialize_Full_Parse
     (Data     : in out Parse_Data_Type;
      Trace    : in     WisiToken.Trace_Access;
      End_Line : in     WisiToken.Line_Number_Type);
   --  Call Wisi.Initialize_Full_Parse, then do any other
   --  initialization that Data needs.

   overriding
   procedure Parse_Language_Params
     (Data   : in out Parse_Data_Type;
      Params : in     String);

end Wisi.Gpr;
