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

   type Panic_Data is new LR.Recover_Data with record
      Nonterm       : Token_ID;
      Goto_State    : Unknown_State_Index;
      Popped_Tokens : Token_Array; -- from parse stack
      Pushed_Tokens : Token_Array; -- to parse stack from input queue
   end record;

   Default_Panic : constant Panic_Data :=
     (Invalid_Token, Unknown_State, Empty_Token_Array, Empty_Token_Array);

   function Pop_To_Good
     (Table        : in     Parse_Table;
      Parser_State : in out Parser_Lists.Parser_State;
      Trace        : in out WisiToken.Trace'Class)
     return Boolean
   is
      use Token;
      Panic    : Panic_Data renames Panic_Data (Parser_State.Recover.all);
      Top      : Parser_Stack_Item   := Parser_State.Stack.Peek;
      Prev_Top : Parser_Stack_Item   := (Unknown_State, Invalid_Token);
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

         Prev_Top := Parser_State.Stack.Pop;
         Panic.Popped_Tokens.Append (Prev_Top.Token);

         Top := Parser_State.Stack.Peek;

         Panic.Nonterm := Trace.Descriptor.First_Nonterminal;
      end loop Pop_Stack;

      if Trace_Parse > 0 then
         Trace.Put (Integer'Image (Parser_State.Label) & ": recover");
         if Top.State = State_Index'First then
            Trace.Put_Line (" failed");
         else
            Trace.Put_Line
              (" state" & Unknown_State_Index'Image (Top.State) &
                 " " & Image (Trace.Descriptor.all, Panic.Nonterm) &
                 " goto" & Unknown_State_Index'Image (Panic.Goto_State));

            if Trace_Parse > 2 then
               Parser_State.Put_Top_10 (Trace);
            end if;
         end if;
      end if;

      return Top.State /= State_Index'First;
   end Pop_To_Good;

   function Recover
     (Parser        : in out LR.Instance'Class;
      Parsers       : in out Parser_Lists.List;
      Current_Token : in out Token_ID)
     return Boolean
   is
      Trace : WisiToken.Trace'Class renames Parser.Semantic_State.Trace.all;

      Keep_Going : Boolean  := False;
      Last_ID    : Token_ID := Current_Token;
   begin
      for Parser_State of Parsers loop
         Parser_State.Recover := new Panic_Data'(Default_Panic);

         Keep_Going := Keep_Going or Pop_To_Good
           (Parser.Table.all, Parser_State, Parser.Semantic_State.Trace.all);
      end loop;

      if not Keep_Going then
         return False;
      end if;

      Keep_Going := False;

      Matching_Input :
      loop
         for Parser_State of Parsers loop
            declare
               use Parser_Lists;
               use all type Ada.Containers.Count_Type;

               Panic : Panic_Data renames Panic_Data (Parser_State.Recover.all);

               Use_Popped : Token_ID := Invalid_Token;

               function Check_Popped return Boolean
               is begin
                  if Panic.Popped_Tokens.Length > 0 then
                     declare
                        ID : constant Token_ID := Panic.Popped_Tokens (1);
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
                  if Use_Popped = Invalid_Token then
                     --  Check_Popped returned false.
                     Keep_Going := True;
                     Panic.Pushed_Tokens.Append (Panic.Nonterm);
                     Parser_State.Stack.Push ((Panic.Goto_State, Panic.Nonterm));

                  else
                     --  Check to see if we really can use the popped token
                     declare
                        ID     : constant Token_ID         := Panic.Popped_Tokens (1);
                        Action : constant Parse_Action_Rec := Action_For (Parser.Table.all, Panic.Goto_State, ID).Item;
                        --  We ignore conflicts in Action; just take the first one.
                     begin
                        case Action.Verb is
                        when Shift =>
                           Keep_Going := True;
                           Panic.Pushed_Tokens.Append (Panic.Nonterm);
                           Parser_State.Stack.Push ((Panic.Goto_State, Panic.Nonterm));

                           Panic.Pushed_Tokens.Append (ID);
                           if Trace_Parse > 1 then
                              Trace.Put
                                (Integer'Image (Parser_State.Label) & " recover: " &
                                   State_Image (Panic.Goto_State) & ": " &
                                   Image (Trace.Descriptor.all, ID) & " : ");
                              Put (Trace, Action);
                              Trace.New_Line;
                           end if;

                           Parser_State.Stack.Push ((Action.State, ID));

                        when Reduce | Accept_It | Error =>
                           --  We can't handle this; check next nonterm
                           null;

                        end case;
                     end;
                  end if;

                  if Trace_Parse > 0 then
                     Trace.Put_Line
                       (Integer'Image (Parser_State.Label) & ": recover: resume, pushed " &
                          Image (Trace.Descriptor.all, Panic.Nonterm));
                  end if;
               end if;
            end;
         end loop;

         exit Matching_Input when Keep_Going;

         if Trace_Parse > 1 then
            Trace.Put_Line ("  discard " & Image (Trace.Descriptor.all, Current_Token));
         end if;
         Parser.Semantic_State.Discard_Token (Current_Token);

         Current_Token := Parser.Lexer.Find_Next;
         Parser.Semantic_State.Input_Token (Current_Token, Parser.Lexer);
         if Trace_Parse > 1 then
            Trace.Put_Line ("  next " & Image (Trace.Descriptor.all, Current_Token));
         end if;

         --  Allow EOF_Token to succeed
         exit Matching_Input when Last_ID = Trace.Descriptor.EOF_ID;
         Last_ID := Current_Token;
      end loop Matching_Input;

      if Keep_Going then
         declare
            Recover : Panic_Data renames Panic_Data (Parsers.First.State_Ref.Recover.all);
         begin
            --  FIXME: handle parsers.count > 1 (or delete Panic_Mode), set Recover
            Parser.Semantic_State.Recover (Recover.Popped_Tokens, Recover.Pushed_Tokens, Recover => null);
         end;
      end if;

      if Trace_Parse > 0 then
         if not Keep_Going then
            Trace.Put_Line ("panic recover: fail");
         end if;
      end if;
      return Keep_Going;
   end Recover;

end WisiToken.Parser.LR.Panic_Mode;
