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

   procedure Pop_To_Good (Table : in Parse_Table; Cursor : in Parser_Lists.Cursor)
   is
      use Parser_Lists;
      use Token;
      Nonterm    : Nonterminal_ID      := Nonterminal_ID'First;
      Top        : Stack_Item          := Cursor.Peek;
      Goto_State : Unknown_State_Index := Unknown_State;
   begin
      Pop_Stack :
      loop
         Nonterms :
         loop
            if Table.Panic_Recover (Nonterm) then
               Goto_State := Goto_For (Table, Top.State, Nonterm);
               exit Pop_Stack when Goto_State /= Unknown_State;
            end if;
            if Nonterm = Nonterminal_ID'Last then
               exit Nonterms;
            else
               Nonterm := Nonterminal_ID'Succ (Nonterm);
            end if;
         end loop Nonterms;

         Cursor.Pop;
         exit Pop_Stack when Cursor.Stack_Empty;
         Top := Cursor.Peek;
      end loop Pop_Stack;

      --  Cursor.Set_Panic_Nonterm (Nonterm);
   end Pop_To_Good;

   function Panic_Mode
     (Table         : in     Parse_Table;
      Parsers       : in out Parser_Lists.List;
      Current_Token : in     Token.Token_ID)
     return Boolean
   is
      pragma Unreferenced (Current_Token);
      Try_Again : constant Boolean := False;
   begin
      for I in Parsers.Iterate loop
         Pop_To_Good (Table, Parser_Lists.To_Cursor (Parsers, I));
      end loop;


      return Try_Again;
   end Panic_Mode;

end FastToken.Parser.LR.Panic_Mode;
