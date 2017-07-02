--  Abstract :
--
--  See spec
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

package body WisiToken.Parser.LR.Panic_Mode is

   function Pop_To_Good
     (Table  : in     Parse_Table;
      Cursor : in     Parser_Lists.Cursor;
      Trace  : in out WisiToken.Trace'Class)
     return Boolean
   is
      use Token;
      Panic    : Panic_Data := Default_Panic;
      Top      : Parser_Lists.Stack_Item := Cursor.Peek;
      Prev_Top : Parser_Lists.Stack_Item := (Unknown_State, Token_ID'Last);
   begin
      Panic.Nonterm := Trace.Descriptor.First_Nonterminal;

      Pop_Stack :
      loop
         exit Pop_Stack when Top.State = State_Index'First;

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

            exit Nonterms when Panic.Nonterm = Trace.Descriptor.Last_Nonterminal;

            Panic.Nonterm := Panic.Nonterm + 1;
         end loop Nonterms;

         Prev_Top := Cursor.Pop;
         Panic.Popped_Tokens.Append (Prev_Top.Token);

         Top := Cursor.Peek;

         Panic.Nonterm := Trace.Descriptor.First_Nonterminal;
      end loop Pop_Stack;

      if Trace_Parse > 0 then
         Trace.Put (Integer'Image (Cursor.Label) & ": recover");
         if Top.State = State_Index'First then
            Trace.Put_Line (" failed");
         else
            Trace.Put_Line
              (" state" & Unknown_State_Index'Image (Top.State) &
                 " " & Image (Trace.Descriptor.all, Panic.Nonterm) &
                 " goto" & Unknown_State_Index'Image (Panic.Goto_State));

            if Trace_Parse > 2 then
               Parser_Lists.Put_Trace_Top_10 (Trace, Cursor);
            end if;
         end if;
      end if;

      Cursor.Panic_Ref.Element.all := Panic;
      return Top.State /= State_Index'First;
   end Pop_To_Good;

   function Panic_Mode
     (Parser        : in out LR.Instance'Class;
      Parsers       : in out Parser_Lists.List;
      Current_Token : in out Token_ID)
     return Boolean
   is
      Trace : WisiToken.Trace'Class renames Parser.Semantic_State.Trace.all;

      Keep_Going : Boolean  := False;
      Last_ID    : Token_ID := Current_Token;
   begin
      for I in Parsers.Iterate loop
         Keep_Going := Keep_Going or Pop_To_Good
           (Parser.Table.all, Parser_Lists.To_Cursor (Parsers, I), Parser.Semantic_State.Trace.all);
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
               use all type Ada.Containers.Count_Type;

               Cursor     : constant Parser_Lists.Cursor := To_Cursor (Parsers, I);
               Panic      : Panic_Reference renames Cursor.Panic_Ref;

               Use_Popped : Token_ID := Token_ID'Last;

               function Check_Popped return Boolean
               is begin
                  if Panic.Popped_Tokens.Length > 0 then
                     declare
                        ID : constant Token_ID := Panic.Popped_Tokens.Peek (1);
                     begin
                        if ID in Trace.Descriptor.First_Terminal .. Trace.Descriptor.Last_Terminal then
                           if Parser.Table.Follow (Panic.Nonterm, ID) then
                              Use_Popped := ID;
                              return True;
                           end if;
                        end if;
                     end;
                  end if;
                  return False;
               end Check_Popped;

            begin
               --  We check the first popped token, as well as
               --  the current tokens, to handle languages like Ada
               --  with "end * ;", where there is a missing 'end', and
               --  this one is in the popped tokens.
               if Parser.Table.Follow (Panic.Nonterm, Current_Token) or Check_Popped then
                  Keep_Going := True;
                  Panic.Pushed_Tokens.Append (Panic.Nonterm);
                  Cursor.Push ((Panic.Goto_State, Panic.Nonterm));

                  if Use_Popped /= Token_ID'Last then
                     declare
                        ID     : constant Token_ID     := Panic.Popped_Tokens.Peek (1);
                        State  : constant State_Index  := Cursor.Peek.State;
                        Action : constant Parse_Action_Rec := Action_For (Parser.Table.all, State, ID).Item;
                        --  We ignore conflicts in Action; just take the first one.
                     begin
                        Panic.Pushed_Tokens.Append (ID);
                        if Trace_Parse > 1 then
                           Trace.Put
                             (Integer'Image (Cursor.Label) & " recover: " &
                                State_Image (State) & ": " &
                                Image (Trace.Descriptor.all, ID) & " : ");
                           Put_Trace (Trace, Action);
                           Trace.New_Line;
                        end if;

                        case Action.Verb is
                        when Shift =>
                           Cursor.Push ((Action.State, ID));

                        when Reduce | Accept_It | Error =>
                           --  We should not get here.
                           raise Programmer_Error with "recover: non-shift action not supported";

                        end case;
                     end;
                  end if;

                  if Trace_Parse > 0 then
                     Trace.Put_Line
                       (Integer'Image (Cursor.Label) & ": recover: resume, pushed " &
                          Image (Trace.Descriptor.all, Panic.Nonterm));
                  end if;
               end if;
            end;
         end loop;

         exit Matching_Input when Keep_Going;

         if Trace_Parse > 1 then
            Trace.Put_Line ("  discard " & Image (Trace.Descriptor.all, Current_Token));
         end if;
         WisiToken.Token.Discard_Token (Current_Token, Parser.Semantic_State);

         Current_Token := Parser.Lexer.Find_Next;
         WisiToken.Token.Input_Token (Current_Token, Parser.Semantic_State, Parser.Lexer);
         if Trace_Parse > 1 then
            Trace.Put_Line ("  next " & Image (Trace.Descriptor.all, Current_Token));
         end if;

         --  Allow EOF_Token to succeed
         exit Matching_Input when Last_ID = Trace.Descriptor.EOF_ID;
         Last_ID := Current_Token;
      end loop Matching_Input;

      if Trace_Parse > 0 then
         if not Keep_Going then
            Trace.Put_Line ("recover: fail");
         end if;
      end if;
      return Keep_Going;
   end Panic_Mode;

end WisiToken.Parser.LR.Panic_Mode;
