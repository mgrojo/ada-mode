--  Abstract :
--
--  Base utilities for McKenzie_Recover
--
--  Copyright (C) 2018 - 2021 Free Software Foundation, Inc.
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

with GNAT.Traceback.Symbolic;
package body WisiToken.Parse.LR.McKenzie_Recover.Base is

   function Get_Barrier
     (Parsers                 : not null access Parser_Lists.List;
      Parser_Status           : in              Parser_Status_Array;
      Min_Success_Check_Count : in              Natural;
      Total_Enqueue_Count     : in              Natural;
      Check_Delta_Limit       : in              Natural;
      Enqueue_Limit           : in              Natural)
     return Boolean
   is
      Done_Count : SAL.Base_Peek_Type := 0;
      Skip : Boolean;
   begin
      --  Return True if all parsers are done, or if any parser has a config
      --  available to check.
      for P_Status of Parser_Status loop
         Skip := False;

         case P_Status.Recover_State is
         when Active | Ready =>
            if P_Status.Parser_State.Recover.Config_Heap.Count > 0 then
               if P_Status.Parser_State.Recover.Check_Count - Check_Delta_Limit >= Min_Success_Check_Count then
                  --  fail; another parser succeeded, this one taking too long.
                  Done_Count := Done_Count + 1;
                  Skip := True;

               elsif Total_Enqueue_Count + P_Status.Parser_State.Recover.Config_Full_Count >= Enqueue_Limit then
                  --  fail
                  Done_Count := Done_Count + 1;
                  Skip := True;
               end if;
            end if;

            if not Skip then
               case P_Status.Recover_State is
               when Active =>
                  if P_Status.Parser_State.Recover.Config_Heap.Count > 0 then
                     --  Still working
                     return True;
                  else
                     if P_Status.Active_Workers = 0 then
                        --  fail; no configs left to check.
                        Done_Count := Done_Count + 1;
                     end if;
                  end if;

               when Ready =>
                  if P_Status.Parser_State.Recover.Config_Heap.Count > 0 and then
                    P_Status.Parser_State.Recover.Config_Heap.Min_Key <= P_Status.Parser_State.Recover.Results.Min_Key
                  then
                     --  Still more to check.
                     return True;

                  elsif P_Status.Active_Workers = 0 then
                     Done_Count := Done_Count + 1;
                  end if;

               when others =>
                  null;
               end case;
            end if;

         when Success | Fail =>
            Done_Count := Done_Count + 1;
         end case;
      end loop;

      return Done_Count = Parsers.Count;
   end Get_Barrier;

   protected body Supervisor is

      procedure Initialize (Parsers : not null access Parser_Lists.List)
      is begin
         Supervisor.Parsers      := Parsers;
         All_Parsers_Done        := False;
         Success_Counter         := 0;
         Min_Success_Check_Count := Natural'Last;
         Total_Enqueue_Count     := 0;
         Fatal_Called            := False;
         Result                  := Recover_Status'First;
         Error_ID                := Ada.Exceptions.Null_Id;

         declare
            Index : SAL.Peek_Type := 1;
         begin
            for I in Parsers.Iterate loop
               if Parsers (I).Recover_Insert_Delete_Current /= Recover_Op_Arrays.No_Index then
                  --  Previous error recovery resume not finished; this is supposed to
                  --  be checked in Parser.
                  raise SAL.Programmer_Error;
               end if;

               Parser_Status (Index) :=
                 (Recover_State  => Active,
                  Parser_State   => Parser_Lists.Persistent_State_Ref (I),
                  Fail_Mode      => Success,
                  Active_Workers => 0);

               declare
                  Data : McKenzie_Data renames Parsers (I).Recover;
               begin
                  Data.Config_Heap.Clear;
                  Data.Results.Clear;
                  Data.Enqueue_Count := 0;
                  Data.Check_Count   := 0;
                  Data.Success       := False;
               end;

               Index := Index + 1;
            end loop;
         end;

         --  Set Sequential_Index
         declare
            use all type WisiToken.Syntax_Trees.Sequential_Index;

            Default_Positive_Sequential_Index : constant Syntax_Trees.Sequential_Index := 10;
            Default_Negative_Sequential_Index : constant Syntax_Trees.Sequential_Index := -10;

            Tree      : Syntax_Trees.Tree renames Supervisor.Tree.all;
            Seq_Index : constant Syntax_Trees.Sequential_Index := 1;
         begin
            --  The parsers may have different error points, and different parse
            --  stream input after the error point; we arbitrarily pick the first
            --  parser as the origin for Sequential_Index. Because most terminal
            --  nodes are shared, we must set Sequential_Index consistently for
            --  all parsers, including in terminal tokens copied from
            --  Shared_Stream (for Set_Error or Add_Deleted). So we walk prev/next
            --  terminal for each parser, skipping deleted nodes, and using
            --  Copied_From to handle copied nodes.

            --  First find common starting point. Parser 1 is the "reference" parser.
            for Parser_Index in 1 .. Parser_Count loop
               Max_Sequential_Indices (Parser_Index) := Tree.To_Stream_Node_Parents
                 (Parser_Status (Parser_Index).Parser_State.Current_Token);

               if Max_Sequential_Indices (Parser_Index).Ref.Node = Syntax_Trees.Invalid_Node_Access -- empty nonterm
                 or else
                 Tree.Label (Max_Sequential_Indices (Parser_Index).Ref.Node) not in Syntax_Trees.Terminal_Label
               then
                  Tree.First_Terminal (Max_Sequential_Indices (Parser_Index));
               end if;

               if Parser_Index = 1 then
                  Tree.Set_Sequential_Index (Max_Sequential_Indices (Parser_Index).Ref.Node, Seq_Index);

               elsif Max_Sequential_Indices (Parser_Index).Ref.Node /= Max_Sequential_Indices (1).Ref.Node then
                  --  There are several cases:
                  --
                  --  1. parser_index node is copied
                  --  2. Reference node is before or after parser node.
                  --  3. Reference node is deleted in parser.
                  --
                  --  In case 3, the parser node does not need Sequential_Index.
                  --
                  --  Note that the reference node cannot be inserted or deleted in the
                  --  reference parser, because we start with Parse_State.Current_Token,
                  --  which is after any deleted tokens.
                  if Tree.Copied_From (Max_Sequential_Indices (Parser_Index).Ref.Node) =
                    Max_Sequential_Indices (1).Ref.Node
                  then
                     --  case 1.
                     Tree.Set_Sequential_Index (Max_Sequential_Indices (Parser_Index).Ref.Node, Seq_Index);
                  else
                     declare
                        Ref_Byte_Pos : constant Buffer_Pos := Tree.Byte_Region
                          (Max_Sequential_Indices (1).Ref, Trailing_Non_Grammar => True).First;
                     begin
                        loop
                           declare
                              Byte_Pos : Buffer_Pos := Tree.Byte_Region
                                (Max_Sequential_Indices (Parser_Index).Ref, Trailing_Non_Grammar => True).First;
                           begin
                              if Ref_Byte_Pos < Byte_Pos then
                                 Tree.Prev_Terminal (Max_Sequential_Indices (Parser_Index));

                                 Byte_Pos := Tree.Byte_Region
                                   (Max_Sequential_Indices (Parser_Index).Ref, Trailing_Non_Grammar => True).First;

                                 exit when Ref_Byte_Pos > Byte_Pos; -- case 3.
                              else
                                 Tree.Next_Terminal (Max_Sequential_Indices (Parser_Index));
                                 Byte_Pos := Tree.Byte_Region
                                   (Max_Sequential_Indices (Parser_Index).Ref, Trailing_Non_Grammar => True).First;

                                 exit when Ref_Byte_Pos < Byte_Pos; -- case 3.
                              end if;

                              if Max_Sequential_Indices (Parser_Index).Ref.Node =
                                Max_Sequential_Indices (1).Ref.Node -- case 2.
                                or else
                                Tree.Copied_From (Max_Sequential_Indices (Parser_Index).Ref.Node) =
                                Max_Sequential_Indices (1).Ref.Node --  case 1 and 2.
                              then
                                 Tree.Set_Sequential_Index (Max_Sequential_Indices (Parser_Index).Ref.Node, Seq_Index);
                                 exit;
                              end if;

                              if Ref_Byte_Pos = Byte_Pos then
                                 raise SAL.Programmer_Error;
                              end if;
                           end;
                        end loop;
                     end;
                  end if;
               end if;
            end loop;

            Min_Sequential_Indices := Max_Sequential_Indices;

            Extend_Max_Sequential_Index (Default_Positive_Sequential_Index);
            Extend_Min_Sequential_Index (Default_Negative_Sequential_Index);
         end;
      end Initialize;

      entry Get
        (Parser_Index : out SAL.Base_Peek_Type;
         Config       : out Configuration;
         Status       : out Config_Status)
        when (Fatal_Called or All_Parsers_Done) or else Get_Barrier
          (Parsers, Parser_Status, Min_Success_Check_Count, Total_Enqueue_Count, Check_Delta_Limit, Enqueue_Limit)
      is
         Done_Count     : SAL.Base_Peek_Type := 0;
         Skip           : Boolean;
         Min_Cost       : Integer            := Integer'Last;
         Min_Cost_Index : SAL.Base_Peek_Type;

         procedure Set_Outputs (I : in SAL.Peek_Type)
         is begin
            Parser_Index := I;
            Config       := Parser_Status (I).Parser_State.Recover.Config_Heap.Remove;
            Status       := Valid;

            Parser_Status (I).Parser_State.Recover.Check_Count :=
              Parser_Status (I).Parser_State.Recover.Check_Count + 1;

            Parser_Status (I).Active_Workers := Parser_Status (I).Active_Workers + 1;
         end Set_Outputs;

         procedure Set_All_Done
         is begin
            Parser_Index := SAL.Base_Peek_Type'First;

            pragma Warnings (Off, "aggregate not fully initialized");
            --  Config.Stack.Data is not initialized, but no uninitialized data is
            --  ever referenced.
            Config       := (others => <>);
            pragma Warnings (On, "aggregate not fully initialized");

            Status       := All_Done;
         end Set_All_Done;

      begin
         if Fatal_Called or All_Parsers_Done then
            Set_All_Done;
            return;
         end if;

         --  Same logic as in Get_Barrier, but different actions.
         --
         --  No task_id in outline trace messages, because they may appear in
         --  .parse_good
         for I in Parser_Status'Range loop
            Skip := False;

            declare
               P_Status : Base.Parser_Status renames Parser_Status (I);
            begin
               case P_Status.Recover_State is
               when Active | Ready =>
                  if P_Status.Parser_State.Recover.Config_Heap.Count > 0 then
                     if P_Status.Parser_State.Recover.Check_Count - Check_Delta_Limit >= Min_Success_Check_Count then
                        if Trace_McKenzie > Outline then
                           Put_Line
                             (Trace.all, Tree.all,
                              P_Status.Parser_State.Stream, "fail; check delta (limit" &
                                Integer'Image (Min_Success_Check_Count + Check_Delta_Limit) & ")",
                              Task_ID => False);
                        end if;
                        P_Status.Recover_State := Fail;
                        P_Status.Fail_Mode     := Fail_Check_Delta;

                        Done_Count := Done_Count + 1;
                        Skip := True;

                     elsif Total_Enqueue_Count + P_Status.Parser_State.Recover.Config_Full_Count >= Enqueue_Limit then
                        if Trace_McKenzie > Outline then
                           Put_Line
                             (Trace.all, Tree.all,
                              P_Status.Parser_State.Stream, "fail; total enqueue limit (" &
                                Enqueue_Limit'Image & " cost" &
                                P_Status.Parser_State.Recover.Config_Heap.Min_Key'Image & ")",
                              Task_ID => False);
                        end if;
                        P_Status.Recover_State := Fail;
                        P_Status.Fail_Mode     := Fail_Enqueue_Limit;

                        Done_Count := Done_Count + 1;
                        Skip := True;
                     end if;
                  end if;

                  if not Skip then
                     case P_Status.Recover_State is
                     when Active =>
                        if P_Status.Parser_State.Recover.Config_Heap.Count > 0 then
                           if P_Status.Parser_State.Recover.Config_Heap.Min_Key < Min_Cost then
                              Min_Cost       := P_Status.Parser_State.Recover.Config_Heap.Min_Key;
                              Min_Cost_Index := I;
                              --  not done
                           end if;
                        else
                           if P_Status.Active_Workers = 0 then
                              --  No configs left to check (rarely happens with real languages).
                              if Trace_McKenzie > Outline then
                                 Put_Line
                                   (Trace.all, Tree.all, P_Status.Parser_State.Stream, "fail; no configs left",
                                    Task_ID => False);
                              end if;
                              P_Status.Recover_State := Fail;
                              P_Status.Fail_Mode     := Fail_No_Configs_Left;

                              Done_Count := Done_Count + 1;
                           end if;
                        end if;

                     when Ready =>
                        if P_Status.Parser_State.Recover.Config_Heap.Count > 0 and then
                          P_Status.Parser_State.Recover.Config_Heap.Min_Key <=
                          P_Status.Parser_State.Recover.Results.Min_Key
                        then
                           --  Still more to check. We don't check Min_Cost here so this parser
                           --  can finish quickly.
                           Set_Outputs (I);
                           return;

                        elsif P_Status.Active_Workers = 0 then
                           P_Status.Recover_State := Success;
                           Done_Count             := Done_Count + 1;
                        end if;
                     when others =>
                        null;
                     end case;
                  end if;

               when Success | Fail =>
                  Done_Count := Done_Count + 1;
               end case;
            end;
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
            raise SAL.Programmer_Error with "Get_Barrier and Get logic do not match";
         end if;
      end Get;

      procedure Success
        (Parser_Index : in     SAL.Peek_Type;
         Config       : in     Configuration;
         Configs      : in out Config_Heaps.Heap_Type)
      is
         Data : McKenzie_Data renames Parser_Status (Parser_Index).Parser_State.Recover;
      begin
         Put (Parser_Index, Configs); --  Decrements Active_Worker_Count.

         if Trace_McKenzie > Detail then
            Put
              ("succeed: enqueue" & Integer'Image (Data.Enqueue_Count) & ", check " & Integer'Image (Data.Check_Count),
               Trace.all, Tree.all, Parser_Status (Parser_Index).Parser_State.Stream, Config);
         end if;

         Success_Counter := Success_Counter + 1;
         Result          := Success;

         Data.Success := True;

         if Force_Full_Explore then
            Data.Results.Add (Config);
            return;
         end if;

         if Data.Check_Count < Min_Success_Check_Count then
            Min_Success_Check_Count := Data.Check_Count;
         end if;

         if Force_High_Cost_Solutions then
            Data.Results.Add (Config);
            if Data.Results.Count > 3 then
               Parser_Status (Parser_Index).Recover_State := Ready;
            end if;
         else
            if Data.Results.Count = 0 then
               Data.Results.Add (Config);

               Parser_Status (Parser_Index).Recover_State := Ready;

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
         Configs_Count : constant SAL.Base_Peek_Type := Configs.Count; -- Before it is emptied, for Trace.

         P_Status : Base.Parser_Status renames Parser_Status (Parser_Index);
         Data : McKenzie_Data renames P_Status.Parser_State.Recover;
      begin
         P_Status.Active_Workers := P_Status.Active_Workers - 1;

         Total_Enqueue_Count := Total_Enqueue_Count + Integer (Configs_Count);
         Data.Enqueue_Count  := Data.Enqueue_Count + Integer (Configs_Count);
         loop
            exit when Configs.Count = 0;

            Data.Config_Heap.Add (Configs.Remove);
         end loop;

         if Trace_McKenzie > Detail then
            Put_Line
              (Trace.all, Tree.all, P_Status.Parser_State.Stream,
               "enqueue:" & SAL.Base_Peek_Type'Image (Configs_Count) &
                 "/" & SAL.Base_Peek_Type'Image (Data.Config_Heap.Count) &
                 "/" & Trimmed_Image (Total_Enqueue_Count) &
                 "/" & Trimmed_Image (Data.Check_Count) &
                 ", min cost:" &
                 (if Data.Config_Heap.Count > 0
                  then Integer'Image (Data.Config_Heap.Min_Key)
                  else " ? ") &
                 ", active workers:" & Integer'Image (P_Status.Active_Workers));
         end if;
      end Put;

      procedure Config_Full (Prefix : in String; Parser_Index : in SAL.Peek_Type)
      is
         P_Status : Base.Parser_Status renames Parser_Status (Parser_Index);
         Data : McKenzie_Data renames P_Status.Parser_State.Recover;
      begin
         Data.Config_Full_Count := Data.Config_Full_Count + 1;
         if Trace_McKenzie > Outline then
            Put_Line (Trace.all, Tree.all, Stream (Parser_Index), Prefix & ": config.ops is full; " &
                        Data.Config_Full_Count'Image);
         end if;
      end Config_Full;

      function Recover_Result return Recover_Status
      is
         Temp : Recover_Status := Result;
      begin
         if Result = Success then
            return Success;
         else
            for S of Parser_Status loop
               Temp := Recover_Status'Max (Result, S.Fail_Mode);
            end loop;
            return Temp;
         end if;
      end Recover_Result;

      procedure Fatal (E : in Ada.Exceptions.Exception_Occurrence)
      is
         use Ada.Exceptions;
      begin
         if Trace_McKenzie > Outline then
            Trace.Put_Line ("task " & Task_Attributes.Value'Image & " Supervisor: Error");
         end if;
         Fatal_Called   := True;
         Error_ID       := Exception_Identity (E);
         Error_Message  := +Exception_Message (E);
         if Debug_Mode then
            Trace.Put_Line
              ("task " & Task_Attributes.Value'Image & ": " & Exception_Name (E) & ": " & Exception_Message (E) &
                 ASCII.LF & -- keep the message together
                 GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
         end if;
      end Fatal;

      entry Done (Error_ID : out Ada.Exceptions.Exception_Id; Message : out Ada.Strings.Unbounded.Unbounded_String)
        when All_Parsers_Done or Fatal_Called
      is begin
         Error_ID := Supervisor.Error_ID;
         Message  := Error_Message;

         for Parser_Index in 1 .. Parser_Count loop
            Parser_Status  (Parser_Index).Parser_State.Max_Sequential_Index := Max_Sequential_Indices (Parser_Index);
         end loop;
         if Trace_McKenzie > Detail then
            Trace.New_Line;
            Trace.Put_Line ("Supervisor: Done");
         end if;
      end Done;

      function Parser_State (Parser_Index : in SAL.Peek_Type) return Parser_Lists.Constant_Reference_Type
      is begin
         return (Element => Parser_Status (Parser_Index).Parser_State);
      end Parser_State;

      function Stream (Parser_Index : in SAL.Peek_Type) return Syntax_Trees.Stream_ID
      is begin
         return Parser_Status (Parser_Index).Parser_State.Stream;
      end Stream;

      function Min_Sequential_Index return Syntax_Trees.Sequential_Index
      is
         use Syntax_Trees;
      begin
         return Result : Sequential_Index := Sequential_Index'Last do
            for I in 1 .. Parser_Count loop
               Result := Sequential_Index'Min (@,  Tree.Get_Sequential_Index (Min_Sequential_Indices (I).Ref.Node));
            end loop;
         end return;
      end Min_Sequential_Index;

      function Max_Sequential_Index return Syntax_Trees.Sequential_Index
      is
         use Syntax_Trees;
      begin
         return Result : Sequential_Index := Sequential_Index'First do
            for I in 1 .. Parser_Count loop
               Result := Sequential_Index'Max (@,  Tree.Get_Sequential_Index (Max_Sequential_Indices (I).Ref.Node));
            end loop;
         end return;
      end Max_Sequential_Index;

      function Min_Sequential_Index (Parser_Index : in SAL.Peek_Type) return Syntax_Trees.Stream_Node_Parents
      is begin
         return Min_Sequential_Indices (Parser_Index);
      end Min_Sequential_Index;

      function Max_Sequential_Index (Parser_Index : in SAL.Peek_Type) return Syntax_Trees.Stream_Node_Parents
      is begin
         return Max_Sequential_Indices (Parser_Index);
      end Max_Sequential_Index;

      function Max_Sequential_Index_Is_EOI (Parser_Index : in SAL.Peek_Type) return Boolean
      is begin
         return Tree.EOI = Max_Sequential_Indices (Parser_Index).Ref.Node;
      end Max_Sequential_Index_Is_EOI;

      procedure Extend_Min_Sequential_Index (Target : in Syntax_Trees.Sequential_Index)
      is
         use Syntax_Trees;
         Index              : Sequential_Index := Min_Sequential_Index;
         Skip_Set_Reference : Boolean          := False;
         --  If one of the parsers has a node inserted before the current
         --  reference node, or a node in the reference stream was deleted, the
         --  reference node gets an earlier or later sequential_index.
      begin
         if Min_Sequential_Index (1).Ref.Node = Tree.SOI then
            --  Can't extend it any further.
            raise SAL.Programmer_Error;
         end if;

         loop
            exit when Min_Sequential_Indices (1).Ref.Node = Tree.SOI;
            exit when Index = Target;
            if Skip_Set_Reference then
               Skip_Set_Reference := False;
            else
               Tree.Prev_Terminal (Min_Sequential_Indices (1));
            end if;
            Index := @ - 1;
            declare
               Ref_Byte_Pos : constant Buffer_Pos := Tree.Byte_Region
                 (Min_Sequential_Indices (1).Ref, Trailing_Non_Grammar => True).First;

            begin
               for Parser_Index in 2 .. Parser_Count loop
                  declare
                     Byte_Pos : Buffer_Pos := Tree.Byte_Region
                       (Min_Sequential_Indices (Parser_Index).Ref, Trailing_Non_Grammar => True).First;
                  begin
                     if Ref_Byte_Pos > Byte_Pos then
                        --  Reference node is deleted in parser; wait for reference to catch
                        --  up.
                        null;

                     elsif Ref_Byte_Pos < Byte_Pos then
                        Tree.Next_Terminal (Min_Sequential_Indices (Parser_Index));
                        Byte_Pos := Tree.Byte_Region
                          (Min_Sequential_Indices (Parser_Index).Ref, Trailing_Non_Grammar => True).First;
                     end if;

                     if Ref_Byte_Pos = Byte_Pos then
                        Tree.Set_Sequential_Index (Min_Sequential_Indices (Parser_Index).Ref.Node, Index);

                        if Min_Sequential_Indices (Parser_Index).Ref.Node =
                          Min_Sequential_Indices (1).Ref.Node
                          or else
                          Tree.Copied_From (Min_Sequential_Indices (Parser_Index).Ref.Node) =
                          Min_Sequential_Indices (1).Ref.Node
                        then
                           null;
                        else
                           --  parser node is inserted before reference node, or corresponding
                           --  reference node was deleted.
                           Skip_Set_Reference := True;
                        end if;
                     end if;
                  end;
               end loop;
               if not Skip_Set_Reference then
                  Tree.Set_Sequential_Index (Min_Sequential_Indices (1).Ref.Node, Index);
               end if;
            end;
         end loop;
      end Extend_Min_Sequential_Index;

      procedure Extend_Max_Sequential_Index (Target : in Syntax_Trees.Sequential_Index)
      is
         use Syntax_Trees;
         Index              : Sequential_Index := Max_Sequential_Index;
         Skip_Set_Reference : Boolean          := False;
      begin
         loop
            exit when Tree.ID (Max_Sequential_Indices (1).Ref.Node) = Tree.ID (Tree.EOI);
            --  EOI can be copied for error. test_incremental.adb Preserve_Parse_Errors_1

            exit when Index = Target;
            if Skip_Set_Reference then
               Skip_Set_Reference := False;
            else
               Tree.Next_Terminal (Max_Sequential_Indices (1));
            end if;
            Index := @ + 1;
            declare
               Ref_Byte_Pos : constant Buffer_Pos := Tree.Byte_Region
                 (Max_Sequential_Indices (1).Ref, Trailing_Non_Grammar => True).First;
            begin
               for Parser_Index in 2 .. Parser_Count loop
                  declare
                     Byte_Pos : Buffer_Pos := Tree.Byte_Region
                       (Max_Sequential_Indices (Parser_Index).Ref, Trailing_Non_Grammar => True).First;
                  begin
                     if Ref_Byte_Pos < Byte_Pos then
                        --  Reference node is deleted in parser; wait for reference to catch
                        --  up.
                        null;

                     elsif Ref_Byte_Pos > Byte_Pos then
                        Tree.Next_Terminal (Max_Sequential_Indices (Parser_Index));
                        Byte_Pos := Tree.Byte_Region
                          (Max_Sequential_Indices (Parser_Index).Ref, Trailing_Non_Grammar => True).First;
                     end if;

                     if Ref_Byte_Pos = Byte_Pos then
                        Tree.Set_Sequential_Index (Max_Sequential_Indices (Parser_Index).Ref.Node, Index);

                        if Max_Sequential_Indices (Parser_Index).Ref.Node =
                          Max_Sequential_Indices (1).Ref.Node
                          or else
                          Tree.Copied_From (Max_Sequential_Indices (Parser_Index).Ref.Node) =
                          Max_Sequential_Indices (1).Ref.Node
                        then
                           null;
                        else
                           --  parser node is inserted before reference node
                           Skip_Set_Reference := True;
                        end if;
                     end if;
                  end;
               end loop;
               if not Skip_Set_Reference then
                  Tree.Set_Sequential_Index (Max_Sequential_Indices (1).Ref.Node, Index);
               end if;
            end;
         end loop;
      end Extend_Max_Sequential_Index;

   end Supervisor;

   procedure Extend_Sequential_Index
     (Super    : not null access Base.Supervisor;
      Thru     : in              Syntax_Trees.Valid_Node_Access;
      Positive : in              Boolean)
   is
      use all type WisiToken.Syntax_Trees.Sequential_Index;
   begin
      if Super.Tree.Get_Sequential_Index (Thru) /= Syntax_Trees.Invalid_Sequential_Index then
         return;
      end if;
      if Positive then
         loop
            exit when Super.Tree.Get_Sequential_Index (Thru) /= Syntax_Trees.Invalid_Sequential_Index;
            Super.Extend_Max_Sequential_Index (Target => 2 * Super.Max_Sequential_Index);
         end loop;

      else
         loop
            exit when Super.Tree.Get_Sequential_Index (Thru) /= Syntax_Trees.Invalid_Sequential_Index;
            Super.Extend_Min_Sequential_Index (Target => 2 * Super.Min_Sequential_Index);
         end loop;
      end if;
   end Extend_Sequential_Index;

   procedure Put
     (Message      : in              String;
      Super        : not null access Base.Supervisor;
      Parser_Index : in              SAL.Peek_Type;
      Config       : in              Configuration;
      Task_ID      : in              Boolean := True)
   is begin
      Put (Message, Super.Trace.all, Super.Tree.all, Super.Parser_State (Parser_Index).Stream,
           Config, Task_ID);
   end Put;

end WisiToken.Parse.LR.McKenzie_Recover.Base;
