--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2019 Stephen Leake All Rights Reserved.
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

with Java_Expressions_Antlr_Actions;
package body WisiToken.Parse.LR.McKenzie_Recover.Java_Expressions_Antlr is

   use all type Java_Expressions_Antlr_Actions.Token_Enum_ID; -- token names

   Descriptor : WisiToken.Descriptor renames Java_Expressions_Antlr_Actions.Descriptor;

   subtype Terminal_Token_ID_Set is WisiToken.Token_ID_Set (Descriptor.First_Terminal .. Descriptor.Last_Terminal);

   Minimal_Complete_Action_IDs : constant Terminal_Token_ID_Set :=
     To_Token_ID_Set
       (Descriptor.First_Terminal, Descriptor.Last_Terminal,
          (+CLASS_ID) &
          Descriptor.EOI_ID);

   procedure Use_Minimal_Complete_Actions
     (Tokens               : in     Token_ID_Array_1_3;
      Config               : in     Configuration;
      Use_Complete         :    out Boolean;
      Matching_Begin_Token :    out Token_ID_Arrays.Vector)
   is
      use all type SAL.Base_Peek_Type;
      use Java_Expressions_Antlr_Actions;
      use Token_ID_Arrays;
   begin
      if Config.Stack.Depth = 1 and Tokens (1) = Descriptor.EOI_ID then
         --  Empty input buffer
         Use_Complete         := True;
         Matching_Begin_Token := To_Vector (+IDENTIFIER_ID);

      elsif Minimal_Complete_Action_IDs (Tokens (1)) then
         Use_Complete := True;
         Matching_Begin_Token := Empty_Vector;
      else
         Use_Complete := False;
         Matching_Begin_Token := Empty_Vector;
      end if;
   end Use_Minimal_Complete_Actions;

   function String_ID_Set
     (Descriptor        : in WisiToken.Descriptor;
      String_Literal_ID : in Token_ID)
     return Token_ID_Set
   is
      pragma Unreferenced (Descriptor);
   begin
      return (String_Literal_ID .. String_Literal_ID => True);
   end String_ID_Set;

end WisiToken.Parse.LR.McKenzie_Recover.Java_Expressions_Antlr;
