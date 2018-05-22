--  Abstract :
--
--  Base utilities for McKenzie_Recover
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

with Ada.Task_Identification;
with GNAT.Traceback.Symbolic;
package body WisiToken.LR.McKenzie_Recover.Base is

   function Get_Barrier
     (Parsers                 : not null access Parser_Lists.List;
      Parser_Status           : in              Parser_Status_Array;
      Parser_States           : in              Parser_State_Array;
      Active_Workers          : in              Parser_Natural_Array;
      Cost_Limit              : in              Natural;
      Min_Success_Check_Count : in              Natural;
      Check_Delta_Limit       : in              Natural)
     return Boolean
   is
      use all type SAL.Base_Peek_Type;
      Done_Count : SAL.Base_Peek_Type := 0;
   begin
      --  Return True if all parsers are done, or if any parser has a config
      --  available to check.
      for I in 1 .. Parsers.Count loop
         case Parser_Status (I) is
         when Active =>
            if Parser_States (I).Recover.Check_Count - Check_Delta_Limit >= Min_Success_Check_Count then
               --  fail; another parser succeeded, this one taking too long.
               Done_Count := Done_Count + 1;

            elsif Parser_States (I).Recover.Config_Heap.Count > 0 then
               if Parser_States (I).Recover.Config_Heap.Min_Key <= Cost_Limit then
                  return True;
               else
                  if Active_Workers (I) = 0 then
                     --  fail; remaining configs exceed cost limit
                     Done_Count := Done_Count + 1;
                  end if;
               end if;

            else
               if Active_Workers (I) = 0 then
                  --  fail; no configs left to check (rarely happens with real
                  --  languages).
                  Done_Count := Done_Count + 1;
               end if;
            end if;

         when Ready =>
            if Parser_States (I).Recover.Config_Heap.Count > 0 and then
              Parser_States (I).Recover.Config_Heap.Min_Key <= Parser_States (I).Recover.Results.Min_Key
            then
               --  Still more to check.
               return True;

            elsif Active_Workers (I) = 0 then
               Done_Count := Done_Count + 1;
            end if;

         when Success | Fail =>
            Done_Count := Done_Count + 1;
         end case;
      end loop;

      return Done_Count = Parsers.Count;
   end Get_Barrier;

   protected body Supervisor is

      procedure Initialize
        (Parsers   : not null access Parser_Lists.List;
         Terminals : not null access constant Base_Token_Arrays.Vector)
      is
         use all type SAL.Base_Peek_Type;
         Index : SAL.Peek_Type := 1;
      begin
         Supervisor.Parsers      := Parsers;
         Supervisor.Terminals    := Terminals;
         All_Parsers_Done        := False;
         Active_Workers          := (others => 0);
         Success_Counter         := 0;
         Min_Success_Check_Count := Natural'Last;
         Fatal_Called            := False;
         Result                  := Fail;
         Error_ID                := Ada.Exceptions.Null_Id;

         for I in Parsers.Iterate loop
            if Parsers.Reference (I).Recover_Insert_Delete.Length > 0 then
               --  Previous error recovery resume not finished; this is supposed to
               --  be checked in Parser.
               raise Programmer_Error;
            end if;

            Parser_Status (Index) := Active;
            Parser_States (Index) := Parser_Lists.Persistent_State_Ref (I);
            Parser_Labels (Index) := Parsers.Constant_Reference (I).Label;

            declare
               Data : McKenzie_Data renames Parsers.Reference (I).Recover;
            begin
               Data.Config_Heap.Clear;
               Data.Results.Clear;
               Data.Enqueue_Count := 0;
               Data.Check_Count   := 0;
               Data.Success       := False;
            end;

            Index := Index + 1;
         end loop;
      end Initialize;

      entry Get
        (Parser_Index : out SAL.Base_Peek_Type;
         Config       : out Configuration;
         Status       : out Config_Status)
        when (Fatal_Called or All_Parsers_Done) or else
          Get_Barrier
            (Parsers, Parser_Status, Parser_States, Active_Workers, Cost_Limit, Min_Success_Check_Count,
             Check_Delta_Limit)
      is
         use all type SAL.Base_Peek_Type;
         Done_Count     : SAL.Base_Peek_Type := 0;
         Min_Cost       : Integer            := Integer'Last;
         Min_Cost_Index : SAL.Base_Peek_Type;

         procedure Set_Outputs (I : in SAL.Peek_Type)
         is begin
            Parser_Index := I;
            Config       := Parser_States (I).Recover.Config_Heap.Remove;
            Status       := Valid;

            Parser_States (I).Recover.Check_Count := Parser_States (I).Recover.Check_Count + 1;

            Active_Workers (I) := Active_Workers (I) + 1;
         end Set_Outputs;

         procedure Set_All_Done
         is begin
            Parser_Index := SAL.Base_Peek_Type'First;
            Config       := (others => <>);
            Status       := All_Done;
         end Set_All_Done;

      begin
         if Fatal_Called or All_Parsers_Done then
            Set_All_Done;
            return;
         end if;

         --  Same logic as in Get_Barrier, but different actions.
         for I in 1 .. Parsers.Count loop
            case Parser_Status (I) is
            when Active =>
               if Parser_States (I).Recover.Config_Heap.Count > 0 then
                  if Parser_States (I).Recover.Check_Count - Check_Delta_Limit >= Min_Success_Check_Count then
                     if Trace_McKenzie > Detail then
                        Put_Line (Trace.all, Parser_Labels (I), "fail; too slow (limit" &
                                    Integer'Image (Min_Success_Check_Count + Check_Delta_Limit) & ")");
                     end if;
                     Parser_Status (I) := Fail;
                     Done_Count        := Done_Count + 1;

                  elsif Parser_States (I).Recover.Config_Heap.Min_Key <= Cost_Limit then
                     if Parser_States (I).Recover.Config_Heap.Min_Key < Min_Cost then
                        Min_Cost       := Parser_States (I).Recover.Config_Heap.Min_Key;
                        Min_Cost_Index := I;
                     end if;

                  else
                     if Active_Workers (I) = 0 then
                        if Trace_McKenzie > Detail then
                           Put_Line (Trace.all, Parser_Labels (I), "fail; too expensive");
                        end if;
                        Parser_Status (I) := Fail;
                        Done_Count        := Done_Count + 1;
                     end if;
                  end if;
               else
                  if Active_Workers (I) = 0 then
                     --  No configs left to check (rarely happens with real languages).
                     if Trace_McKenzie > Detail then
                        Put_Line (Trace.all, Parser_Labels (I), "fail (no configs left)");
                     end if;
                     Parser_Status (I) := Fail;
                     Done_Count        := Done_Count + 1;
                  end if;
               end if;

            when Ready =>
               if Parser_States (I).Recover.Config_Heap.Count > 0 and then
                 Parser_States (I).Recover.Config_Heap.Min_Key <= Parser_States (I).Recover.Results.Min_Key
               then
                  --  Still more to check.
                  Set_Outputs (I);
                  return;

               elsif Active_Workers (I) = 0 then
                  Parser_Status (I) := Success;
                  Done_Count        := Done_Count + 1;
               end if;

            when Success | Fail =>
               Done_Count := Done_Count + 1;
            end case;
         end loop;

         if Min_Cost /= Integer'Last then
            Set_Outputs (Min_Cost_Index);

         elsif Done_Count = Parsers.Count then
            if Trace_McKenzie > Extra then
               Trace.Put_Line ("Supervisor: done, " & (if Success_Counter > 0 then "succeed" else "fail"));
            end if;

            Set_All_Done;
            All_Parsers_Done := True;
         else
            raise Programmer_Error with "Get_Barrier and Get logic do not match";
         end if;
      end Get;

      procedure Success
        (Parser_Index : in     SAL.Peek_Type;
         Config       : in     Configuration;
         Configs      : in out Config_Heaps.Heap_Type)
      is
         use all type SAL.Base_Peek_Type;
         Data : McKenzie_Data renames Parser_States (Parser_Index).Recover;
      begin
         Put (Parser_Index, Configs); --  Decrements Active_Worker_Count.

         if Trace_McKenzie > Detail then
            Put
              ("succeed: enqueue" & Integer'Image (Data.Enqueue_Count) & ", check " & Integer'Image (Data.Check_Count),
               Trace.all, Parser_Labels (Parser_Index), Terminals.all, Config);
         end if;

         if Force_Full_Explore then
            return;
         end if;

         Success_Counter := Success_Counter + 1;
         Result          := Success;

         Data.Success := True;

         if Data.Check_Count < Min_Success_Check_Count then
            Min_Success_Check_Count := Data.Check_Count;
         end if;

         if Force_High_Cost_Solutions then
            Data.Results.Add (Config);
            if Data.Results.Count > 3 then
               Parser_Status (Parser_Index) := Ready;
            end if;
         else
            if Data.Results.Count = 0 then
               Data.Results.Add (Config);

               Parser_Status (Parser_Index) := Ready;

            elsif Config.Cost < Data.Results.Min_Key then
               --  delete higher cost configs from Results
               loop
                  Data.Results.Drop;
                  exit when Data.Results.Count = 0 or else
                    Config.Cost >= Data.Results.Min_Key;
               end loop;

               Data.Results.Add (Config);

            elsif Config.Cost = Data.Results.Min_Key then
               Data.Results.Add (Config);

            else
               --  Config.Cost > Results.Min_Key
               null;
            end if;
         end if;
      end Success;

      procedure Put (Parser_Index : in SAL.Peek_Type; Configs : in out Config_Heaps.Heap_Type)
      is
         use all type SAL.Base_Peek_Type;
         Configs_Count : constant SAL.Base_Peek_Type := Configs.Count; -- Before it is emptied, for Trace.

         Data : McKenzie_Data renames Parser_States (Parser_Index).Recover;
      begin
         Active_Workers (Parser_Index) := Active_Workers (Parser_Index) - 1;

         loop
            exit when Configs.Count = 0;

            --  [1] has a check for duplicate configs here; that only happens with
            --  higher costs, which take too long for our application.
            Data.Config_Heap.Add (Configs.Remove);
            Data.Enqueue_Count := Data.Enqueue_Count + 1;
         end loop;

         if Trace_McKenzie > Detail then
            Put_Line
              (Trace.all, Parser_Labels (Parser_Index),
               "enqueue:" & SAL.Base_Peek_Type'Image (Configs_Count) &
                 "/" & SAL.Base_Peek_Type'Image (Data.Config_Heap.Count) &
                 "/" & Int_Image (Data.Enqueue_Count) &
                 "/" & Int_Image (Data.Check_Count) &
                 ", min cost:" &
                 (if Data.Config_Heap.Count > 0
                  then Integer'Image (Data.Config_Heap.Min_Key)
                  else " ? ") &
                 ", active workers:" & Integer'Image (Active_Workers (Parser_Index)));
         end if;
      end Put;

      function Recover_Result return Recover_Status
      is begin
         return Result;
      end Recover_Result;

      procedure Fatal (E : in Ada.Exceptions.Exception_Occurrence)
      is
         use Ada.Exceptions;
         Task_ID : constant String := Ada.Task_Identification.Image (Ada.Task_Identification.Current_Task);
      begin
         if Trace_McKenzie > Outline then
            Trace.Put_Line (Task_ID & " Supervisor: Error");
         end if;
         Fatal_Called   := True;
         Error_ID       := Exception_Identity (E);
         Error_Message  := +Exception_Message (E);
         Trace.Put_Line (Exception_Name (E) & ": " & Exception_Message (E));
         Trace.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
      end Fatal;

      entry Done (Error_ID : out Ada.Exceptions.Exception_Id; Message : out Ada.Strings.Unbounded.Unbounded_String)
        when All_Parsers_Done or Fatal_Called
      is begin
         Error_ID := Supervisor.Error_ID;
         Message  := Error_Message;
         if Trace_McKenzie > Detail then
            Trace.Put_Line ("Supervisor: Done");
         end if;
      end Done;

      function Parser_State (Parser_Index : in SAL.Peek_Type) return Parser_Lists.Constant_Reference_Type
      is begin
         return (Element => Parser_States (Parser_Index));
      end Parser_State;

      function Label (Parser_Index : in SAL.Peek_Type) return Natural
      is begin
         return Parser_Labels (Parser_Index);
      end Label;

   end Supervisor;

   protected body Shared_Lookahead is

      procedure Initialize  (Shared_Parser : not null access LR.Parser.Parser)
      is begin
         Shared_Lookahead.Shared_Parser := Shared_Parser;
      end Initialize;

      function Get_Token (Index : in Token_Index) return Token_Index
      is
         Temp : Token_Index := Index;
      begin
         if Index > Shared_Parser.Terminals.Last_Index then
            Temp := Next_Grammar_Token
              (Shared_Parser.Terminals, Shared_Parser.Lexer_Errors, Shared_Parser.Line_Begin_Token,
               Shared_Parser.Trace.Descriptor.all, Shared_Parser.Lexer, Shared_Parser.User_Data);
            pragma Assert (Temp = Index);
         end if;
         return Temp;
      end Get_Token;

      function Token (Index : in Token_Index) return Base_Token
      is begin
         return Shared_Parser.Terminals.Element (Index);
      end Token;

      function Last_Index return Token_Index
      is begin
         return Shared_Parser.Terminals.Last_Index;
      end Last_Index;

      procedure Lex_Line (Line : in Line_Number_Type)
      is
         use Ada.Containers;
         EOF_ID            : constant Token_ID := Shared_Parser.Trace.Descriptor.EOF_ID;
         Line_Token        : Token_Index       := Shared_Parser.Terminals.Last_Index;
      begin
         if Shared_Parser.String_Quote_Checked /= Invalid_Line_Number and then
           Shared_Parser.String_Quote_Checked >= Line
         then
            return;

         elsif Shared_Parser.Terminals (Line_Token).Line > Line then
            Shared_Parser.String_Quote_Checked := Line;
            return;

         else
            loop
               exit when Shared_Parser.Terminals (Line_Token).Line > Line or
                 Shared_Parser.Terminals (Line_Token).ID = EOF_ID;

               Line_Token := Get_Token (Line_Token + 1);
            end loop;

            Shared_Parser.String_Quote_Checked := Line;
         end if;
      end Lex_Line;

      function Recovered_Lexer_Error (Line : in Line_Number_Type) return Base_Token_Index
      is
         use WisiToken.Lexer;
         use WisiToken.Lexer.Error_Lists;
      begin
         for Err of reverse Shared_Parser.Lexer_Errors loop
            if Err.Recover_Token /= Invalid_Token_Index and then
              Shared_Parser.Terminals (Err.Recover_Token).Line = Line
            then
               return Err.Recover_Token;
            end if;
         end loop;
         return Invalid_Token_Index;
      end Recovered_Lexer_Error;

      function Next_Line_Token (Line : in Line_Number_Type) return Token_Index
      is begin
         return Shared_Parser.Line_Begin_Token (Line + 1);
      end Next_Line_Token;

   end Shared_Lookahead;

   procedure Put
     (Message      : in              String;
      Super        : not null access Base.Supervisor;
      Shared       : not null access Base.Shared_Lookahead;
      Parser_Index : in              SAL.Peek_Type;
      Config       : in              Configuration;
      Task_ID      : in              Boolean := True)
   is begin
      Put (Message, Super.Trace.all, Super.Parser_State (Parser_Index).Label,
           Shared.Terminals.all, Config, Task_ID);
   end Put;

end WisiToken.LR.McKenzie_Recover.Base;
