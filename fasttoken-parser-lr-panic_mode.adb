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
      Prev_Top : Parser_Lists.Stack_Item := (Unknown_State, Token.Grammar_ID'First);
   begin
      Panic.Element.all := Default_Panic;

      Pop_Stack :
      loop
         Nonterms :
         loop
            if Table.Panic_Recover (Panic.Nonterm) then
               Panic.Goto_State := Goto_For (Table, Top.State, Panic.Nonterm);
               if Panic.Goto_State = Prev_Top.State and
                 Prev_Top.Token = Panic.Nonterm
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
         Panic.Popped_Tokens.Prepend (Prev_Top.Token);

         Top := Cursor.Peek;
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

            if Trace_Parse > 2 then
               Parser_Lists.Put_Trace_Top_10 (Cursor);
            end if;
         end if;
      end if;

      return Top.State /= State_Index'First;
   end Pop_To_Good;

   function Panic_Mode
     (Table         : in     Parse_Table;
      Parsers       : in out Parser_Lists.List;
      Current_Token : in out Token_ID;
      Lexer         : in     Lexer_Pkg.Handle)
     return Boolean
   is
      Keep_Going : Boolean  := False;
      Last_ID    : Token_ID := Current_Token;
   begin
      for I in Parsers.Iterate loop
         Keep_Going := Keep_Going or Pop_To_Good (Table, Parser_Lists.To_Cursor (Parsers, I));
      end loop;

      if not Keep_Going then
         return False;
      end if;

      Keep_Going := False;

      Matching_Input :
      loop
         for I in Parsers.Iterate loop
            declare
               use Parser_Lists;

               Cursor : constant Parser_Lists.Cursor := To_Cursor (Parsers, I);
               Panic  : Panic_Reference renames Cursor.Panic_Ref;
            begin
               if Table.Follow (Panic.Nonterm)(Current_Token) then
                  Keep_Going := True;
                  Panic.Pushed_Token := Panic.Nonterm;
                  Cursor.Push ((Panic.Goto_State, Panic.Nonterm));

                  if Trace_Parse > 0 then
                     Put_Trace_Line
                       (Integer'Image (Cursor.Label) & ": recover: resume, pushed " & Token_Image (Panic.Nonterm));
                  end if;
               end if;
            end;
         end loop;

         exit Matching_Input when Keep_Going;

         if Trace_Parse > 1 then
            Ada.Text_IO.Put_Line ("  discard " & Token_Image (Current_Token));
         end if;
         Current_Token := Lexer.Find_Next;
         if Trace_Parse > 1 then
            Ada.Text_IO.Put_Line ("  next " & Token_Image (Current_Token));
         end if;

         --  Allow EOF_Token to succeed
         exit Matching_Input when Last_ID = EOF_Token;
         Last_ID := Current_Token;
      end loop Matching_Input;

      if Trace_Parse > 0 then
         if not Keep_Going then
            Put_Trace_Line ("recover: fail");
         end if;
      end if;
      return Keep_Going;
   end Panic_Mode;

end FastToken.Parser.LR.Panic_Mode;
