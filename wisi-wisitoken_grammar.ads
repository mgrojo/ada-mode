--  Abstract :
--
--  Wisitoken_grammar parser language-specific runtime
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

package Wisi.WisiToken_Grammar is

   Language_Protocol_Version : constant String := "1";

   type Parse_Data_Type is new Wisi.Parse_Data_Type with null record;

   overriding
   function New_User_Data (Template : in Parse_Data_Type) return WisiToken.Syntax_Trees.User_Data_Access
   is (new Parse_Data_Type);

   overriding
   procedure Initialize_Partial_Parse
     (Data              : in out Parse_Data_Type;
      Trace             : in     WisiToken.Trace_Access;
      Post_Parse_Action : in     Post_Parse_Action_Type;
      Begin_Line        : in     WisiToken.Line_Number_Type;
      End_Line          : in     WisiToken.Line_Number_Type);
   --  Call Wisi.Initialize_Partial_Parse, then do any other
   --  initialization that Data needs.

   overriding
   procedure Initialize_Full_Parse
     (Data     : in out Parse_Data_Type;
      Trace    : in     WisiToken.Trace_Access;
      End_Line : in     WisiToken.Line_Number_Type);
   --  Call Wisi.Initialize_Full_Parse, then do any other
   --  initialization that Data needs.

   procedure Check_Parens
     (Data    : in out Wisi.Parse_Data_Type'Class;
      Tree    : in     WisiToken.Syntax_Trees.Tree;
      Nonterm : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Args    : in     Arg_Index_Array);

end Wisi.WisiToken_Grammar;
