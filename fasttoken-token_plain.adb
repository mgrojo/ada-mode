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

   procedure Put_Trace (Nonterm : in Token_Pkg.Nonterminal_ID; Index : in Natural; Tokens : in Token_Pkg.List.Instance)
   is
      use Ada.Characters.Handling;
      use Token_Pkg.List;
      use Token_Pkg;

      Action_Name : constant String := To_Lower (Image (Nonterm)) &
        "_" & Int_Image (Index);
   begin
      Put_Trace (Action_Name & ": " & Image (Nonterm) & " <= ");
      Put_Trace (Tokens);
      Put_Trace_Line ("");
   end Put_Trace;

   procedure Merge_Tokens
     (Nonterm : in     Token_Pkg.Nonterminal_ID;
      Index   : in     Natural;
      Tokens  : in     Token_Pkg.List.Instance;
      Action  : in     Semantic_Action;
      State   : access State_Type)
   is
      pragma Unreferenced (State);
   begin
      if Action /= null then
         Action (Nonterm, Index, Tokens);
         if Trace_Parse > 1 then
            Put_Trace (Nonterm, Index, Tokens);
         end if;
      end if;
   end Merge_Tokens;

end FastToken.Token_Plain;
