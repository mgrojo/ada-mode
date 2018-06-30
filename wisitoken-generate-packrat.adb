--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018 Stephen Leake All Rights Reserved.
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

package body WisiToken.Generate.Packrat is

   function Potential_Direct_Left_Recursive
     (Grammar : in WisiToken.Productions.Prod_Arrays.Vector;
      Empty   : in Token_ID_Set)
     return Token_ID_Set
   is
      subtype Nonterminal is Token_ID range Grammar.First_Index .. Grammar.Last_Index;
   begin
      return Result : Token_ID_Set (Nonterminal) := (others => False) do
         for Prod of Grammar loop
            RHS_Loop :
            for RHS of Prod.RHSs loop
               ID_Loop :
               for ID of RHS.Tokens loop
                  if ID = Prod.LHS then
                     Result (ID) := True;
                     exit RHS_Loop;
                  elsif not (ID in Nonterminal) then
                     exit ID_Loop;
                  elsif not Empty (ID) then
                     exit ID_Loop;
                  end if;
               end loop ID_Loop;
            end loop RHS_Loop;
         end loop;
      end return;
   end Potential_Direct_Left_Recursive;

   function Potential_Direct_Right_Recursive
     (Grammar : in WisiToken.Productions.Prod_Arrays.Vector;
      Empty   : in Token_ID_Set)
     return Token_ID_Set
   is
      subtype Nonterminal is Token_ID range Grammar.First_Index .. Grammar.Last_Index;
   begin
      return Result : Token_ID_Set (Nonterminal) := (others => False) do
         for Prod of Grammar loop
            RHS_Loop :
            for RHS of Prod.RHSs loop
               ID_Loop :
               for I in reverse RHS.Tokens.First_Index + 1 .. RHS.Tokens.Last_Index loop
                  declare
                     ID : constant Token_ID := RHS.Tokens (I);
                  begin
                     if ID = Prod.LHS then
                        Result (ID) := True;
                        exit RHS_Loop;
                     elsif not (ID in Nonterminal) then
                        exit ID_Loop;
                     elsif not Empty (ID) then
                        exit ID_Loop;
                     end if;
                  end;
               end loop ID_Loop;
            end loop RHS_Loop;
         end loop;
      end return;
   end Potential_Direct_Right_Recursive;

   function Initialize
     (Source_File_Name : in String;
      Grammar          : in WisiToken.Productions.Prod_Arrays.Vector;
      Source_Line_Map  : in Productions.Source_Line_Maps.Vector)
     return Packrat.Data
   is
      Empty : constant Token_ID_Set := WisiToken.Generate.Has_Empty_Production (Grammar);
   begin
      return Result : constant Packrat.Data :=
        (First_Nonterminal => Grammar.First_Index,
         Last_Nonterminal => Grammar.Last_Index,
         Source_File_Name => +Source_File_Name,
         Grammar => Grammar,
         Source_Line_Map => Source_Line_Map,
         Empty => Empty,
         Left_Recursive => Potential_Direct_Left_Recursive (Grammar, Empty));
   end Initialize;

   procedure Check_Recursion (Data : in Packrat.Data; Descriptor : in WisiToken.Descriptor)
   is
      Right_Recursive : constant Token_ID_Set := Potential_Direct_Right_Recursive (Data.Grammar, Data.Empty);
   begin
      for Prod of Data.Grammar loop
         if Data.Left_Recursive (Prod.LHS) and Right_Recursive (Prod.LHS) then
            --  We only implement the simplest left recursion solution ([warth
            --  2008] figure 3); [tratt 2010] section 6.3 gives this condition for
            --  that to be valid. Indirect left recursion is detected at runtime.
            Put_Error
              (Error_Message
                 (-Data.Source_File_Name, Data.Source_Line_Map (Prod.LHS).Line, "'" & Image (Prod.LHS, Descriptor) &
                    "' is both left and right recursive; not supported."));
         end if;
      end loop;
   end Check_Recursion;

   procedure Check_RHS_Order (Data : in Packrat.Data; Descriptor : in WisiToken.Descriptor)
   is
      use all type Ada.Containers.Count_Type;
   begin
      for Prod of Data.Grammar loop
         --  Special case; typical LALR list is written:
         --
         --  statement_list
         --    : statement
         --    | statement_list statement
         --    ;
         --  association_list
         --    : association
         --    | association_list COMMA association
         --    ;
         --
         --  For packrat, that must change to:
         --
         --  statement_list
         --    : statement_list statement
         --    | statement
         --    ;
         --  association_list
         --    : association_list COMMA association
         --    | association
         --    ;

         if Prod.RHSs.Length > 1 then
            declare
               Tokens_0 : Token_ID_Arrays.Vector renames Prod.RHSs (0).Tokens;
               Tokens_1 : Token_ID_Arrays.Vector renames Prod.RHSs (1).Tokens;
            begin
               if (Tokens_0.Length = 1 and Tokens_1.Length > 1) and then
                   Tokens_1 (1) = Prod.LHS and then
                   Tokens_0 (1) = Tokens_1 (Tokens_1.Last_Index)
               then
                  Put_Error
                    (Error_Message
                       (-Data.Source_File_Name, Data.Source_Line_Map (Prod.LHS).Line,
                        "LALR recursive list must be rewritten for packrat; swap order of right hand sides."));
               end if;
            end;
         end if;

         for I in Prod.RHSs.First_Index .. Prod.RHSs.Last_Index - 1 loop
            if Prod.RHSs (I).Tokens.Length = 0 then
               Put_Error
                 (Error_Message
                    (-Data.Source_File_Name, Data.Source_Line_Map (Prod.LHS).RHS_Map (I),
                     "right hand side" & Integer'Image (I) & " in " & Image (Prod.LHS, Descriptor) &
                       " is empty, but not last; no later right hand side will match."));
               WisiToken.Generate.Error := True;
            end if;
         end loop;

         for I in Prod.RHSs.First_Index + 1 .. Prod.RHSs.Last_Index loop
            declare
               Cur : Token_ID_Arrays.Vector renames Prod.RHSs (I).Tokens;
            begin
               for J in Prod.RHSs.First_Index .. I - 1 loop
                  declare
                     Prev : Token_ID_Arrays.Vector renames Prod.RHSs (J).Tokens;
                     K    : constant Natural := Shared_Prefix (Prev, Cur);
                  begin
                     if K > 0 and Prev.Length < Cur.Length then
                        Put_Error
                          (Error_Message
                             (-Data.Source_File_Name, Data.Source_Line_Map (Prod.LHS).RHS_Map (I),
                              "right hand side" & Integer'Image (I) & " in " & Image (Prod.LHS, Descriptor) &
                                " may never match; it shares a prefix with a shorter previous rhs" &
                                Integer'Image (J) & "."));
                     end if;
                  end;
               end loop;
            end;
         end loop;
      end loop;
   end Check_RHS_Order;

   procedure Check_All (Data : in Packrat.Data; Descriptor : in WisiToken.Descriptor)
   is begin
      Check_Recursion (Data, Descriptor);
      Check_RHS_Order (Data, Descriptor);
   end Check_All;

end WisiToken.Generate.Packrat;
