--  Abstract :
--
--  See spec
--
--  Copyright (C) 2017 Stephen LeakeAll Rights Reserved.
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

package body FastToken.Parser.LR.Panic_Mode is

   function Pop_To_Good (Table : in Parse_Table; Cursor : in Parser_Lists.Cursor) return Boolean
   is
      use Token;
      Panic    : Parser_Lists.Panic_Reference renames Cursor.Panic_Ref;
      Top      : Parser_Lists.Stack_Item := Cursor.Peek;
      Prev_Top : Parser_Lists.Stack_Item := (Unknown_State, null);
   begin
      Panic.Element.all :=
        (Nonterm    => Nonterminal_ID'First,
         Goto_State => Unknown_State);

      Pop_Stack :
      loop
         Nonterms :
         loop
            if Table.Panic_Recover (Panic.Nonterm) then
               Panic.Goto_State := Goto_For (Table, Top.State, Panic.Nonterm);
               if Panic.Goto_State = Prev_Top.State and
                 (Prev_Top.Token /= null and then ID (Prev_Top.Token) = Panic.Nonterm)
               then
                  --  parser already tried this; keep going back.
                  Panic.Goto_State := Unknown_State;
               end if;
               exit Pop_Stack when Panic.Goto_State /= Unknown_State;
            end if;

            if Panic.Nonterm = Nonterminal_ID'Last then
               exit Nonterms;
            else
               Panic.Nonterm := Nonterminal_ID'Succ (Panic.Nonterm);
            end if;
         end loop Nonterms;

         Prev_Top := Cursor.Pop;
         Top      := Cursor.Peek;
         exit Pop_Stack when Top.State = State_Index'First;

         Panic.Nonterm := Nonterminal_ID'First;
      end loop Pop_Stack;

      if Trace_Parse > 0 then
         Put_Trace (Integer'Image (Cursor.Label) & ": recover");
         if Top.State = State_Index'First then
            Put_Trace_Line (" failed");
         else
            Put_Trace_Line
              (" state" & Unknown_State_Index'Image (Top.State) &
                 " " & Token_Image (Panic.Nonterm) & " goto" & Unknown_State_Index'Image (Panic.Goto_State));
         end if;
      end if;

      return not Cursor.Stack_Empty;
   end Pop_To_Good;

   function Panic_Mode
     (Table         : in     Parse_Table;
      Parsers       : in out Parser_Lists.List;
      Current_Token : in     Token.Token_ID)
     return Boolean
   is
      Try_Again : Boolean := False;
   begin
      for I in Parsers.Iterate loop
         Try_Again := Try_Again or Pop_To_Good (Table, Parser_Lists.To_Cursor (Parsers, I));
      end loop;

      if Try_Again then

         for I in Parsers.Iterate loop
            declare
               use Parser_Lists;
               Cursor : constant Parser_Lists.Cursor := To_Cursor (Parsers, I);
               Panic  : Panic_Reference renames Cursor.Panic_Ref;
            begin
               if Table.Follow (Panic.Nonterm)(Current_Token) then
                  Try_Again := True;
                  Cursor.Push ((Panic.Goto_State, new Nonterminal.Class'(Nonterminal.Get (Panic.Nonterm))));
               end if;
            end;
         end loop;
      end if;

      --  FIXME: loop on skip input

      return Try_Again;
   end Panic_Mode;

end FastToken.Parser.LR.Panic_Mode;
