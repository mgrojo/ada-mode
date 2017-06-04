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

pragma License (GPL);

with Ada.Characters.Handling;
package body FastToken.Token_Plain is

   procedure Put_Trace
     (Trace   : in out FastToken.Trace'Class;
      Nonterm : in     Token_ID;
      Index   : in     Natural;
      Tokens  : in     Token.List.Instance)
   is
      use Ada.Characters.Handling;
      use Token.List;

      Action_Name : constant String := To_Lower (Image (Trace.Descriptor.all, Nonterm)) &
        "_" & Int_Image (Index);
   begin
      Put_Trace (Trace, Action_Name & ": " & Image (Trace.Descriptor.all, Nonterm) & " <= ");
      Put_Trace (Trace, Tokens);
      Put_Trace_Line (Trace, "");
   end Put_Trace;

   overriding procedure Merge_Tokens
     (Nonterm : in     Token_ID;
      Index   : in     Natural;
      Tokens  : in     Token.List.Instance;
      Action  : in     Semantic_Action;
      State   : access State_Type)
   is
      function To_Augmented (Item : in Token.List.Instance) return Token_Stack_Type
      is
         use Token.List;

         Result : Token_Stack_Type;
         I      : List_Iterator := Item.First;
      begin
         loop
            exit when Is_Null (I);
            Result.Append (Augmented_Token'(ID => ID (I)));
            Next (I);
         end loop;
         return Result;
      end To_Augmented;

   begin
      if Action /= null then
         Action (Augmented_Token'(ID => Nonterm), Index, To_Augmented (Tokens));
         if Trace_Parse > 1 then
            Put_Trace (State.Trace.all, Nonterm, Index, Tokens);
         end if;
      end if;
   end Merge_Tokens;

end FastToken.Token_Plain;
