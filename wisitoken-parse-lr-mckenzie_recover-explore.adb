--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018 - 2020 Free Software Foundation, Inc.
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

with Ada.Exceptions;
with SAL.Gen_Bounded_Definite_Queues;
with WisiToken.Parse.LR.McKenzie_Recover.Parse;
with WisiToken.Parse.LR.Parser;
package body WisiToken.Parse.LR.McKenzie_Recover.Explore is

   procedure Do_Shift
     (Label             : in              String;
      Super             : not null access Base.Supervisor;
      Shared            : not null access Base.Shared;
      Parser_Index      : in              SAL.Peek_Type;
      Local_Config_Heap : in out          Config_Heaps.Heap_Type;
      Config            : in out          Configuration;
      State             : in              State_Index;
      ID                : in              Token_ID;
      Cost_Delta        : in              Integer;
      Strategy          : in              Strategies)
   is
      use Config_Op_Arrays;

      McKenzie_Param : McKenzie_Param_Type renames Shared.Table.McKenzie_Param;

      Op : constant Config_Op :=
        (Insert, ID, Super.Tree.Get_Node_Index
           (Parse.Peek_Current_First_Shared_Terminal (Super.Tree.all, Config)));
   begin
      Config.Strategy_Counts (Strategy) := Config.Strategy_Counts (Strategy) + 1;

      if Is_Full (Config.Ops) then
         Super.Config_Full ("do_shift ops", Parser_Index);
         raise Bad_Config;
      else
         Append (Config.Ops, Op);
      end if;

      if Cost_Delta = 0 then
         Config.Cost := Config.Cost + McKenzie_Param.Insert (ID);
      else
         --  Cost_Delta /= 0 comes from Insert_Minimal_Complete_Actions. That
         --  doesn't mean it is better than any other solution, so don't let
         --  cost be 0.
         --
         --  We don't just eliminate all cost for Minimal_Complete_Actions;
         --  that leads to using it far too much at the expense of better
         --  solutions.
         Config.Cost := Integer'Max (1, Config.Cost + McKenzie_Param.Insert (ID) + Cost_Delta);
      end if;

      Config.Error_Token    := Syntax_Trees.Invalid_Recover_Token;
      Config.Check_Status   := (Label => WisiToken.Semantic_Checks.Ok);

      if Config.Stack.Is_Full then
         Super.Config_Full ("do_shift stack", Parser_Index);
         raise Bad_Config;
      else
         Config.Stack.Push ((State, (Virtual => True, ID => ID, others => <>)));
      end if;
      if Trace_McKenzie > Detail then
         Base.Put
           ((if Label'Length > 0 then Label & ": " else "") & "insert " & Image (ID, Super.Tree.Descriptor.all),
            Super, Parser_Index, Config);
      end if;

      Local_Config_Heap.Add (Config);
   end Do_Shift;

   procedure Do_Reduce_1
     (Label             : in              String;
      Super             : not null access Base.Supervisor;
      Shared            : not null access Base.Shared;
      Parser_Index      : in              SAL.Peek_Type;
      Local_Config_Heap : in out          Config_Heaps.Heap_Type;
      Config            : in out          Configuration;
      Action            : in              Reduce_Action_Rec;
      Do_Language_Fixes : in              Boolean := True)
   is
      use all type Semantic_Checks.Check_Status_Label;
      use all type WisiToken.Parse.LR.Parser.Language_Fixes_Access;

      Prev_State : constant Unknown_State_Index := Config.Stack.Peek.State;

      Descriptor : WisiToken.Descriptor renames Super.Tree.Descriptor.all;
      Table      : Parse_Table renames Shared.Table.all;
      Nonterm    : Syntax_Trees.Recover_Token;
      New_State  : Unknown_State_Index;
   begin
      Config.Check_Status := Parse.Reduce_Stack
        (Super, Shared, Config.Stack, Action, Nonterm, Default_Contains_Virtual => True);
      case Config.Check_Status.Label is
      when Ok =>
         null;

      when Semantic_Checks.Error =>
         Config.Error_Token       := Nonterm;
         Config.Check_Token_Count := Action.Token_Count;

         if Do_Language_Fixes then
            if Shared.Language_Fixes /= null then
               Shared.Language_Fixes
                 (Super.Trace.all, Shared.Lexer, Super.Stream (Parser_Index), Shared.Table.all, Super.Tree.all,
                  Local_Config_Heap, Config);
            end if;
         end if;

         --  Finish the reduce; ignore the check fail.
         if Config.Stack.Depth < SAL.Base_Peek_Type (Config.Check_Token_Count) then
            raise SAL.Programmer_Error;
         else
            Config.Stack.Pop (SAL.Base_Peek_Type (Config.Check_Token_Count));
         end if;
         Config.Error_Token    := Syntax_Trees.Invalid_Recover_Token;
         Config.Check_Status   := (Label => Ok);
      end case;

      if Config.Stack.Depth = 0 or else Config.Stack.Peek.State = Unknown_State then
         raise Bad_Config;
      end if;

      New_State := Goto_For (Table, Config.Stack.Peek.State, Action.Production.LHS);

      if New_State = Unknown_State then
         if Debug_Mode then
            Put_Line
              (Super.Trace.all, Super.Tree.all, Super.Stream (Parser_Index), Label &
                 ": Do_Reduce_1: BAD_CONFIG: unknown_State " & Config.Stack.Peek.State'Image & " " &
                 Image (Action.Production.LHS, Descriptor));
         end if;
         raise Bad_Config;
      end if;

      Config.Stack.Push ((New_State, Nonterm));

      if Trace_McKenzie > Extra and Label'Length > 0 then
         Put_Line
           (Super.Trace.all, Super.Tree.all, Super.Stream (Parser_Index), Label &
              ": state" & State_Index'Image (Prev_State) & " reduce" &
              Ada.Containers.Count_Type'Image (Action.Token_Count) & " to " &
              Image (Action.Production.LHS, Descriptor) & ", goto" &
              State_Index'Image (New_State) & " via" & State_Index'Image (Config.Stack.Peek (2).State));
      end if;
   end Do_Reduce_1;

   procedure Do_Reduce_2
     (Label             : in              String;
      Super             : not null access Base.Supervisor;
      Shared            : not null access Base.Shared;
      Parser_Index      : in              SAL.Peek_Type;
      Local_Config_Heap : in out          Config_Heaps.Heap_Type;
      Config            : in out          Configuration;
      Inserted_ID       : in              Token_ID;
      Cost_Delta        : in              Integer;
      Strategy          : in              Strategies)
   is
      --  Perform reduce actions until shift Inserted_ID; if all succeed,
      --  add the final configuration to the heap, return True. If a conflict is
      --  encountered, process the other action the same way. If a semantic
      --  check fails, enqueue possible solutions. For parse table error
      --  actions, or exception Bad_Config, return False.

      Orig_Config : Configuration;
      Table       : Parse_Table renames Shared.Table.all;
      Next_Action : Parse_Action_Node_Ptr := Action_For (Table, Config.Stack.Peek.State, Inserted_ID);
   begin
      if Next_Action.Next /= null then
         Orig_Config := Config;
      end if;

      case Next_Action.Item.Verb is
      when Shift =>
         Do_Shift
           (Label, Super, Shared, Parser_Index, Local_Config_Heap, Config, Next_Action.Item.State, Inserted_ID,
            Cost_Delta, Strategy);

      when Reduce =>
         Do_Reduce_1 (Label, Super, Shared, Parser_Index, Local_Config_Heap, Config, Next_Action.Item);
         Do_Reduce_2
           (Label, Super, Shared, Parser_Index, Local_Config_Heap, Config, Inserted_ID, Cost_Delta, Strategy);

      when Accept_It =>
         raise SAL.Programmer_Error with "found test case for Do_Reduce Accept_It";

      when Error =>
         if Trace_McKenzie > Extra and Label'Length > 0 then
            Put_Line
              (Super.Trace.all, Super.Tree.all, Super.Stream (Parser_Index), Label & ": error on " &
                 Image (Inserted_ID, Super.Tree.Descriptor.all) &
                 " in state" & State_Index'Image (Config.Stack.Peek.State));
         end if;
      end case;

      loop
         exit when Next_Action.Next = null;
         --  There is a conflict; create a new config to shift or reduce.
         declare
            New_Config : Configuration := Orig_Config;
            Action     : Parse_Action_Rec renames Next_Action.Next.Item;
         begin
            case Action.Verb is
            when Shift =>
               Do_Shift
                 (Label, Super, Shared, Parser_Index, Local_Config_Heap, New_Config, Action.State, Inserted_ID,
                  Cost_Delta, Strategy);

            when Reduce =>
               Do_Reduce_1 (Label, Super, Shared, Parser_Index, Local_Config_Heap, New_Config, Action);
               Do_Reduce_2
                 (Label, Super, Shared, Parser_Index, Local_Config_Heap, New_Config, Inserted_ID,
                  Cost_Delta, Strategy);

            when Accept_It =>
               raise SAL.Programmer_Error with "found test case for Do_Reduce Accept_It conflict";

            when Error =>
               null;
            end case;
         end;

         Next_Action := Next_Action.Next;
      end loop;
   exception
   when Bad_Config =>
      if Debug_Mode then
         raise;
      end if;
   end Do_Reduce_2;

   function Edit_Point_Matches_Ops
     (Super  : not null access Base.Supervisor;
      Config : in              Configuration)
     return Boolean
   is
      use Config_Op_Arrays, Config_Op_Array_Refs;
      pragma Assert (Length (Config.Ops) > 0);
      Op : Config_Op renames Constant_Ref (Config.Ops, Last_Index (Config.Ops));
   begin
      return Super.Tree.Get_Node_Index (Parse.Peek_Current_First_Shared_Terminal (Super.Tree.all, Config)) =
        (case Op.Op is
         when Fast_Forward => Op.FF_Token_Index,
         when Undo_Reduce  => Op.UR_Token_Index,
         when Push_Back    => Op.PB_Token_Index,
         when Insert       => Op.Ins_Before,
         when Delete       => Op.Del_Token_Index + 1);
   end Edit_Point_Matches_Ops;

   procedure Fast_Forward
     (Super             : not null access Base.Supervisor;
      Shared            : not null access Base.Shared;
      Parser_Index      : in              SAL.Base_Peek_Type;
      Local_Config_Heap : in out          Config_Heaps.Heap_Type;
      Config            : in              Configuration)
   --  Apply the ops in Config.Insert_Delete; they were inserted by some
   --  fix. Leaves Config.Error_Token, Config.Check_Status set. If there
   --  are conflicts, all are parsed. All succeeding configs are enqueued
   --  in Local_Config_Heap.
   is
      use Parse.Parse_Item_Arrays;
      use Config_Op_Arrays;

      Parse_Items : aliased Parse.Parse_Item_Arrays.Vector;

      Dummy : Boolean := Parse.Parse
        (Super, Shared, Parser_Index, Parse_Items, Config,
         Shared_Token_Goal => Syntax_Trees.Invalid_Node_Index,
         All_Conflicts     => True,
         Trace_Prefix      => "fast_forward");
   begin
      --  This solution is from Language_Fixes (see gate on call below); any
      --  cost increase is done there.
      --
      --  We used to handle the Parse_Items.Length = 1 case specially, and
      --  return Continue. Maintaining that requires too much code
      --  duplication.

      for I in First_Index (Parse_Items) .. Last_Index (Parse_Items) loop
         declare
            Item : Parse.Parse_Item renames Parse.Parse_Item_Array_Refs.Variable_Ref (Parse_Items, I);
         begin
            if Item.Parsed and Item.Config.Current_Insert_Delete = No_Insert_Delete then
               --  Parse processed all Config.Insert_Delete without error;
               --  Item.Config.Error_Token.ID, Check_Status are correct.

               if not Edit_Point_Matches_Ops (Super, Item.Config) then

                  if Is_Full (Item.Config.Ops) then
                     Super.Config_Full ("fast_forward 1", Parser_Index);
                     raise Bad_Config;
                  else
                     Append
                       (Item.Config.Ops,
                        (Fast_Forward,
                         Super.Tree.Get_Node_Index
                           (Parse.Peek_Current_First_Shared_Terminal (Super.Tree.all, Item.Config))));
                  end if;
               end if;

               Item.Config.Minimal_Complete_State := None;
               Item.Config.Matching_Begin_Done    := False;
               Local_Config_Heap.Add (Item.Config);

               if Trace_McKenzie > Detail then
                  Base.Put ("fast forward enqueue", Super, Parser_Index, Item.Config);
               end if;
            end if;
         exception
         when Bad_Config =>
            null;
         end;
      end loop;
   end Fast_Forward;

   function Check
     (Super             : not null access Base.Supervisor;
      Shared            : not null access Base.Shared;
      Parser_Index      : in              SAL.Base_Peek_Type;
      Config            : in out          Configuration;
      Local_Config_Heap : in out          Config_Heaps.Heap_Type)
     return Check_Status
   is
      use Config_Op_Arrays, Config_Op_Array_Refs;
      use Parse.Parse_Item_Arrays;
      use all type Semantic_Checks.Check_Status_Label;

      Parse_Items : aliased Parse.Parse_Item_Arrays.Vector;

      procedure Enqueue (Item : in out Parse.Parse_Item)
      is begin
         --  Append or update a Fast_Forward to indicate the changed edit
         --  point.
         Item.Config.Minimal_Complete_State := None;
         Item.Config.Matching_Begin_Done    := False;

         if Last_Index (Item.Config.Ops) /= Config_Op_Arrays.No_Index and then
           Constant_Ref (Item.Config.Ops, Last_Index (Item.Config.Ops)).Op = Fast_Forward
         then
            --  Update the trailing Fast_Forward.
            Variable_Ref (Item.Config.Ops, Last_Index (Item.Config.Ops)).FF_Token_Index :=
              Super.Tree.Get_Node_Index (Parse.Peek_Current_First_Shared_Terminal (Super.Tree.all, Item.Config));
         else
            if Is_Full (Item.Config.Ops) then
               Super.Config_Full ("check 1", Parser_Index);
               raise Bad_Config;
            else
               Append
                 (Item.Config.Ops,
                  (Fast_Forward, Super.Tree.Get_Node_Index
                     (Parse.Peek_Current_First_Shared_Terminal (Super.Tree.all, Item.Config))));
            end if;
         end if;
         Local_Config_Heap.Add (Item.Config);
         if Trace_McKenzie > Detail then
            Base.Put ("new error point ", Super, Parser_Index, Item.Config);
         end if;
      end Enqueue;

   begin
      if Length (Config.Ops) > 0 then
         declare
            Op : Config_Op renames Constant_Ref (Config.Ops, Last_Index (Config.Ops));
         begin
            case Op.Op is
            when Push_Back =>
               --  Check would undo the Push_Back, leading to
               --  duplicate results. See test_mckenzie_recover.adb Do_Delete_First and
               --  three_action_conflict_lalr.parse_good for examples.
               return Continue;

            when Undo_Reduce =>
               if Config.Check_Status.Label /= Ok then
                  --  This is the "ignore error" solution for a check fail; check it.
                  Config.Check_Status   := (Label => Ok);
                  Config.Error_Token    := Syntax_Trees.Invalid_Recover_Token;

               else
                  --  Check would undo the Undo_Reduce, leading to
                  --  duplicate results.
                  return Continue;
               end if;
            when others =>
               --  Check it
               null;
            end case;
         end;
      end if;

      if Parse.Parse
        (Super, Shared, Parser_Index, Parse_Items, Config, Config.Resume_Token_Goal,
         All_Conflicts => False,
         Trace_Prefix  => "check")
      then
         Config.Error_Token    := Syntax_Trees.Invalid_Recover_Token;
         if Trace_McKenzie > Extra then
            Put_Line (Super.Trace.all, Super.Tree.all, Super.Stream (Parser_Index), "check result: SUCCESS");
         end if;
         return Success;
      end if;

      if Parse.Parse_Item_Arrays.Length (Parse_Items) = 1 then
         --  Return Abandon or Continue.
         declare
            Item : Parse.Parse_Item renames Parse.Parse_Item_Array_Refs.Variable_Ref
              (Parse_Items, First_Index (Parse_Items));
         begin
            if Item.Config.Check_Status.Label /= Ok then
               Config.Check_Status := Item.Config.Check_Status;
               Config.Error_Token  := Item.Config.Error_Token;

               if Item.Shift_Count > 0 then
                  --  Progress was made, so let Language_Fixes try again on the new Config.
                  Enqueue (Item);
               end if;

               --  Explore cannot fix a check fail; only
               --  Language_Fixes can, which was already done. The "ignore error"
               --  case is handled immediately on return from Language_Fixes in
               --  Process_One, below.
               return Abandon;

            else
               pragma Assert (Syntax_Trees.ID (Item.Config.Error_Token) /= Invalid_Token_ID);

               if Item.Shift_Count = 0 then
                  --  Parse did not process any Deletes from Insert_Delete; Fast_Forward
                  --  did that. So the very first token caused an error, and Config is
                  --  unchanged. Just set the error.
                  Config.Error_Token  := Item.Config.Error_Token;
                  Config.Check_Status := (Label => Ok);
                  return Continue;
               else
                  --  Item.Config differs from Config, so enqueue it.
                  Enqueue (Item);

                  --  Also Continue
                  --  Config; Explore might find something that will help (see
                  --  test_mckenzie_recover.adb Extra_Begin). On the other hand, this
                  --  can lead to lots of bogus configs (see If_In_Handler).
                  Config.Error_Token    := Syntax_Trees.Invalid_Recover_Token;
                  Config.Check_Status   := (Label => Ok);

                  return Continue;
               end if;
            end if;
         end;
      end if;

      --  More than one Parse_Item, all failed, all made progress,
      --  so enqueue them.
      --
      --  We know they all made progress because not doing so means the
      --  first token encountered an error, there is no chance to encounter
      --  a conflict, and there can be only one Parse_Item, which is handled
      --  above.
      for I in First_Index (Parse_Items) .. Last_Index (Parse_Items) loop
         declare
            Item : Parse.Parse_Item renames Parse.Parse_Item_Array_Refs.Variable_Ref (Parse_Items, I);
         begin
            pragma Assert (Item.Parsed and Syntax_Trees.ID (Item.Config.Error_Token) /= Invalid_Token_ID);

            Enqueue (Item);
         end;
      end loop;
      return Abandon;

   exception
   when Bad_Config =>
      return Abandon;
   end Check;

   function Check_Reduce_To_Start
     (Super        : not null access Base.Supervisor;
      Shared       : not null access Base.Shared;
      Parser_Index : in              SAL.Base_Peek_Type;
      Orig_Config  : in              Configuration)
     return Boolean
      --  Returns True if Config reduces to the start nonterm.
   is
      Table : Parse_Table renames Shared.Table.all;

      function To_Reduce_Action (Item : in Minimal_Action) return Reduce_Action_Rec
      is begin
         return (Reduce, Item.Production, null, null, Item.Token_Count);
      end To_Reduce_Action;

      Local_Config_Heap : Config_Heaps.Heap_Type; -- never used, because Do_Language_Fixes is False.

      Config  : Configuration                := Orig_Config;
      Actions : Minimal_Action_Arrays.Vector := Table.States (Config.Stack.Peek.State).Minimal_Complete_Actions;
   begin
      loop
         case Actions.Length is
         when 0 =>
            if (for some Item of Table.States (Config.Stack.Peek.State).Kernel =>
                  Item.Production.LHS = Super.Tree.Descriptor.Accept_ID)
            then
               return True;
            else
               return False;
            end if;

         when 1 =>
            case Actions (Actions.First_Index).Verb is
            when Shift =>
               return False;

            when Reduce =>
               Do_Reduce_1
                 ("", Super, Shared, Parser_Index, Local_Config_Heap, Config,
                  To_Reduce_Action (Actions (Actions.First_Index)),
                  Do_Language_Fixes => False);

               Actions := Table.States (Config.Stack.Peek.State).Minimal_Complete_Actions;
            end case;

         when others =>
            return False;
         end case;

         --  loop only exits via returns above
      end loop;
   exception
   when Bad_Config =>
      --  From Do_Reduce_1
      return False;
   end Check_Reduce_To_Start;

   procedure Try_Push_Back
     (Super             : not null access Base.Supervisor;
      Shared            : not null access Base.Shared;
      Parser_Index      : in              SAL.Base_Peek_Type;
      Config            : in              Configuration;
      Local_Config_Heap : in out          Config_Heaps.Heap_Type)
   --  Try pushing back the stack top, to allow operations at that point.
   --  We assume the caller used Push_Back_Valid.
   is
      use Config_Op_Arrays;
      use Syntax_Trees;

      Token : constant Recover_Token := Config.Stack.Peek.Token;

      New_Config : Configuration := Config;
   begin
      --  Since we are not actually changing the source text, it is tempting
      --  to give this operation zero cost. But then we keep doing push_back
      --  forever, making no progress. So we give it a cost.

      New_Config.Error_Token    := Syntax_Trees.Invalid_Recover_Token;
      New_Config.Check_Status   := (Label => WisiToken.Semantic_Checks.Ok);

      if Is_Full (New_Config.Ops) then
         Super.Config_Full ("push_back 1", Parser_Index);
         raise Bad_Config;
      end if;

      Do_Push_Back (Super.Tree.all, New_Config);
      New_Config.Cost := @ + Shared.Table.McKenzie_Param.Push_Back (Syntax_Trees.ID (Token));
      New_Config.Strategy_Counts (Push_Back) := New_Config.Strategy_Counts (Push_Back) + 1;

      Local_Config_Heap.Add (New_Config);

      if Trace_McKenzie > Detail then
         Base.Put ("push_back " & Image (Syntax_Trees.ID (Token), Super.Tree.Descriptor.all), Super,
                   Parser_Index, New_Config);
      end if;
   end Try_Push_Back;

   function Just_Pushed_Back_Or_Deleted
     (Super  : not null access Base.Supervisor;
      Config : in              Configuration;
      ID     : in              Token_ID)
     return Boolean
   is
      use Config_Op_Arrays, Config_Op_Array_Refs;
      Target_Token_Index : Syntax_Trees.Node_Index :=
        Super.Tree.Get_Node_Index (Parse.Peek_Current_First_Shared_Terminal (Super.Tree.all, Config));
      --  Next token; ID might be inserted before it (see Do_Shift).
   begin
      --  This function is called when considering whether to insert ID before
      --  Config.Current_Shared_Token.
      --
      --  We need to consider more than one recent op here; see test_mckenzie_recover.adb
      --  Check_Multiple_Delete_For_Insert. Checking only one op allows this solution there:
      --
      --  ...  (DELETE, END, 7), (DELETE, SEMICOLON, 8), (INSERT, END, 9), (INSERT, SEMICOLON, 9)
      --
      for I in reverse First_Index (Config.Ops) .. Last_Index (Config.Ops) loop
         declare
            Op : Config_Op renames Constant_Ref (Config.Ops, I);
         begin
            case Op.Op is
            when Push_Back =>
               --  The case we are preventing for Push_Back is typically one of:
               --  (PUSH_BACK, Identifier, 2), (INSERT, Identifier, 2)
               --  (PUSH_BACK, Identifier, 2), (PUSH_BACK, END, 3), (INSERT, Identifier, 3), (INSERT, END, 3),
               if Op.PB_Token_Index = Target_Token_Index then
                  if Op.PB_ID = ID then
                     return True;
                  else
                     if Op.PB_Token_Index - 1 = Syntax_Trees.Invalid_Node_Index then
                        return False;
                     else
                        Target_Token_Index := Op.PB_Token_Index - 1;
                     end if;
                  end if;
               else
                  --  Op is at a different edit point.
                  return False;
               end if;

            when Delete =>
               if Op.Del_Token_Index = Target_Token_Index - 1 then
                  if Op.Del_ID = ID then
                     return True;
                  else
                     Target_Token_Index := Op.Del_Token_Index;
                  end if;
               else
                  --  Op is at a different edit point.
                  return False;
               end if;

            when Fast_Forward | Insert | Undo_Reduce =>
               return False;
            end case;
         end;
      end loop;
      return False;
   end Just_Pushed_Back_Or_Deleted;

   procedure Try_Undo_Reduce
     (Super             : not null access Base.Supervisor;
      Shared            : not null access Base.Shared;
      Parser_Index      : in              SAL.Base_Peek_Type;
      Config            : in              Configuration;
      Local_Config_Heap : in out          Config_Heaps.Heap_Type)
   is
      use Config_Op_Arrays;

      McKenzie_Param : McKenzie_Param_Type renames Shared.Table.McKenzie_Param;
      Token          : constant Syntax_Trees.Recover_Token := Config.Stack.Peek.Token;
      New_Config     : Configuration                       := Config;
      Token_Count    : Ada.Containers.Count_Type;
   begin
      pragma Assert (not Token.Virtual); -- We assume caller used Undo_Reduce_Valid.

      New_Config.Error_Token    := Syntax_Trees.Invalid_Recover_Token;
      New_Config.Check_Status   := (Label => WisiToken.Semantic_Checks.Ok);

      Token_Count := Unchecked_Undo_Reduce (New_Config.Stack, Super.Tree.all);

      if not Super.Tree.Buffer_Region_Is_Empty (Token.Element_Node) then
         --  Token is not empty.
         New_Config.Cost := New_Config.Cost + McKenzie_Param.Undo_Reduce (Syntax_Trees.ID (Token));
      end if;

      if Is_Full (New_Config.Ops) then
         Super.Config_Full ("undo_reduce 1", Parser_Index);
         raise Bad_Config;
      else
         Append
           (New_Config.Ops,
            (Undo_Reduce, Syntax_Trees.ID (Token), Token_Count, Super.Tree.Get_Node_Index
               (Super.Tree.First_Terminal (Token))));
      end if;
      New_Config.Strategy_Counts (Undo_Reduce) := New_Config.Strategy_Counts (Undo_Reduce) + 1;

      Local_Config_Heap.Add (New_Config);

      if Trace_McKenzie > Detail then
         Base.Put ("undo_reduce " & Image (Syntax_Trees.ID (Token), Super.Tree.Descriptor.all), Super,
                   Parser_Index, New_Config);
      end if;
   end Try_Undo_Reduce;

   procedure Insert_From_Action_List
     (Super             : not null access Base.Supervisor;
      Shared            : not null access Base.Shared;
      Parser_Index      : in              SAL.Base_Peek_Type;
      Config            : in              Configuration;
      Minimal_Insert    : in              Token_ID_Arrays.Vector;
      Local_Config_Heap : in out          Config_Heaps.Heap_Type)
   is
      Table      : Parse_Table renames Shared.Table.all;
      EOF_ID     : Token_ID renames Super.Tree.Descriptor.EOI_ID;
      Descriptor : WisiToken.Descriptor renames Super.Tree.Descriptor.all;

      --  Find terminal insertions from the current state's action_list to try.
      --
      --  We perform any needed reductions and one shift, so the config is
      --  in a consistent state, and enqueue the result. If there are any
      --  conflicts or semantic check fails encountered, they create other
      --  configs to enqueue.

      Current_First_Terminal_ID : constant Token_ID := Super.Tree.ID
        (Parse.Peek_Current_First_Terminal (Super.Tree.all, Config));

      Cached_Config : Configuration;
      Cached_Action : Reduce_Action_Rec;
      --  Most of the time, all the reductions in a state are the same. So
      --  we cache the first result. This includes one reduction; if an
      --  associated semantic check failed, this does not include the fixes.

      I : Parse_Action_Node_Ptr;
   begin
      for Node of Table.States (Config.Stack.Peek.State).Action_List loop
         I := Node.Actions;
         loop
            exit when I = null;
            declare
               ID     : constant Token_ID := Node.Symbol;
               Action : Parse_Action_Rec renames I.Item;
            begin
               if ID /= EOF_ID and then -- can't insert eof
                 ID /= Invalid_Token_ID -- invalid when Verb = Error
               then
                  if Just_Pushed_Back_Or_Deleted (Super, Config, ID) then
                     if Trace_McKenzie > Extra then
                        Put_Line
                          (Super.Trace.all, Super.Tree.all, Super.Stream (Parser_Index),
                           "Insert: abandon " & Image (ID, Descriptor) & ": undo push_back");
                     end if;
                  elsif ID = Current_First_Terminal_ID then
                     --  This is needed because we allow explore when the error is not at
                     --  the explore point; it prevents inserting useless tokens (ie
                     --  'identifier ;' in ada_lite).
                     if Trace_McKenzie > Extra then
                        Put_Line
                          (Super.Trace.all, Super.Tree.all, Super.Stream (Parser_Index),
                           "Insert: abandon " & Image (ID, Descriptor) & ": current token");
                     end if;

                  elsif (for some Minimal of Minimal_Insert => ID = Minimal) then
                     --  Was inserted by Insert_Minimal_Complete_Actions
                     null;

                  else
                     case Action.Verb is
                     when Shift =>
                        declare
                           New_Config : Configuration := Config;
                        begin
                           Do_Shift
                             ("Insert", Super, Shared, Parser_Index, Local_Config_Heap, New_Config, Action.State, ID,
                              Cost_Delta => 0,
                              Strategy   => Insert);
                        end;

                     when Reduce =>
                        if not Equal (Action, Cached_Action) then
                           declare
                              New_Config : Configuration := Config;
                           begin
                              New_Config.Error_Token    := Syntax_Trees.Invalid_Recover_Token;
                              New_Config.Check_Status   := (Label => WisiToken.Semantic_Checks.Ok);

                              Do_Reduce_1
                                ("Insert", Super, Shared, Parser_Index, Local_Config_Heap, New_Config, Action);
                              Cached_Config := New_Config;
                              Cached_Action := Action;

                              Do_Reduce_2
                                ("Insert", Super, Shared, Parser_Index, Local_Config_Heap, New_Config, ID,
                                 Cost_Delta => 0,
                                 Strategy   => Insert);
                           end;

                        else
                           declare
                              New_Config : Configuration := Cached_Config;
                           begin
                              Do_Reduce_2
                                ("Insert", Super, Shared, Parser_Index, Local_Config_Heap, New_Config, ID,
                                 Cost_Delta => 0,
                                 Strategy   => Insert);
                           end;
                        end if;

                     when Accept_It =>
                        raise SAL.Programmer_Error with "found test case for Process_One Accept_It";

                     when Error =>
                        null;
                     end case;
                  end if;
               end if;
            end;
            I := I.Next;
         end loop;
      end loop;
   end Insert_From_Action_List;

   function Insert_Minimal_Complete_Actions
     (Super             : not null access Base.Supervisor;
      Shared            : not null access Base.Shared;
      Parser_Index      : in              SAL.Base_Peek_Type;
      Orig_Config       : in out          Configuration;
      Local_Config_Heap : in out          Config_Heaps.Heap_Type)
     return Token_ID_Arrays.Vector
      --  Return tokens inserted (empty if none).
   is
      use Ada.Containers;

      Table         : Parse_Table renames Shared.Table.all;
      Descriptor    : WisiToken.Descriptor renames Super.Tree.Descriptor.all;
      Inserted      : Token_ID_Array (1 .. 10) := (others => Invalid_Token_ID);
      Inserted_Last : Integer                  := Inserted'First - 1;

      type Work_Item is record
         Action     : Minimal_Action;
         Cost_Delta : Integer;
         Config     : Configuration;
      end record;

      package Item_Queues is new SAL.Gen_Bounded_Definite_Queues (Work_Item);
      use Item_Queues;

      Work : Queue_Type (10);
      --  The required queue size depends on the number of multiple-item
      --  Minimal_Complete_Actions encountered. That is limited by compound
      --  statement nesting, and by the frequency of such actions.

      procedure Safe_Add_Work (Label : in String; Item : in Work_Item)
      is begin
         if Is_Full (Work) then
            Super.Config_Full ("Minimal_Complete_Actions " & Label, Parser_Index);
            raise Bad_Config;
         else
            Add (Work, Item);
         end if;
      end Safe_Add_Work;

      function To_Reduce_Action (Action : in Minimal_Action) return Reduce_Action_Rec
        is (Reduce, Action.Production, null, null, Action.Token_Count);

      procedure Minimal_Do_Shift
        (Action     : in     Minimal_Action;
         Cost_Delta : in     Integer;
         Config     : in out Configuration)
      is begin
         if Just_Pushed_Back_Or_Deleted (Super, Config, Action.ID) then
            if Trace_McKenzie > Extra then
               Put_Line
                 (Super.Trace.all, Super.Tree.all, Super.Stream (Parser_Index),
                  "Minimal_Complete_Actions: abandon " & Image (Action.ID, Descriptor) & ": undo push back");
            end if;
         else
            Config.Check_Status           := (Label => WisiToken.Semantic_Checks.Ok);
            Config.Minimal_Complete_State := Active;
            Inserted_Last                 := Inserted_Last + 1;
            if Inserted_Last <= Inserted'Last then
               Inserted (Inserted_Last)      := Action.ID;
            else
               Super.Config_Full ("minimal_do_shift Inserted", Parser_Index);
               raise Bad_Config;
            end if;

            Do_Shift
              ("Minimal_Complete_Actions", Super, Shared, Parser_Index, Local_Config_Heap, Config,
               Action.State, Action.ID, Cost_Delta,
               Strategy => Minimal_Complete);
         end if;
      end Minimal_Do_Shift;

      procedure Enqueue_Min_Actions
        (Label   : in String;
         Actions : in Minimal_Action_Arrays.Vector;
         Config  : in Configuration)
      is
         use SAL;
         Length : array (Actions.First_Index .. Actions.Last_Index) of Count_Type := (others => Count_Type'Last);

         Min_Length : Count_Type := Count_Type'Last;
      begin
         if Trace_McKenzie > Extra then
            Put_Line
              (Super.Trace.all, Super.Tree.all, Super.Stream (Parser_Index), "Minimal_Complete_Actions: " & Label &
                 Image (Actions, Descriptor));
         end if;

         if Actions.Length = 0 then
            return;
         elsif Actions.Length = 1 then
            Safe_Add_Work
              ("1", (Actions (Actions.First_Index), Table.McKenzie_Param.Minimal_Complete_Cost_Delta, Config));
            return;
         end if;

         --  More than one minimal action in State; try to use next states to pick one.
         Actions_Loop :
         for I in Actions.First_Index .. Actions.Last_Index loop
            declare
               function Matches (Item : in Kernel_Info; Action : in Minimal_Action) return Boolean
               is begin
                  case Action.Verb is
                  when Shift =>
                     return Item.Before_Dot = Action.ID;
                  when Reduce =>
                     return Item.Before_Dot = Action.Production.LHS;
                  end case;
               end Matches;

               function Length_After_Dot
                 (Item   : in Kernel_Info;
                  Action : in Minimal_Action;
                  Stack  : in Recover_Stacks.Stack)
                 return Ada.Containers.Count_Type
               is
                  Match_ID   : Token_ID;
                  New_Stack  : Recover_Stacks.Stack      := Stack;
                  Next_State : Unknown_State_Index;
                  Result     : Ada.Containers.Count_Type;
                  Min_Result : Ada.Containers.Count_Type := Ada.Containers.Count_Type'Last;
               begin
                  case Action.Verb is
                  when Shift =>
                     New_Stack.Push
                       ((Action.State, (ID => Action.ID, others => <>)));
                     Next_State := Action.State;
                     Match_ID   := Action.ID;

                  when Reduce =>
                     New_Stack.Pop (SAL.Base_Peek_Type (Action.Token_Count));
                     Next_State := Goto_For (Shared.Table.all, New_Stack.Peek.State, Action.Production.LHS);
                     if Next_State = Unknown_State then
                        --  We get here when Insert_From_Action_Table started us down a bad path
                        raise Bad_Config;
                     end if;

                     New_Stack.Push
                       ((Next_State, (ID => Action.Production.LHS, others => <>)));
                     Match_ID   := Action.Production.LHS;
                  end case;

                  if Trace_McKenzie > Extra then
                     Super.Trace.Put (Next_State'Image & " " & Trimmed_Image (Item.Production));
                  end if;

                  for Item of Shared.Table.States (Next_State).Kernel loop
                     if Item.Before_Dot = Match_ID then
                        if Item.Length_After_Dot = 0 then
                           Result := Length_After_Dot
                             (Item, (Reduce, Item.Reduce_Production, Item.Reduce_Count), New_Stack);
                        else
                           Result := Item.Length_After_Dot;
                        end if;
                     end if;

                     if Result < Min_Result then
                        Min_Result := Result;
                     end if;
                  end loop;
                  return Min_Result;
               end Length_After_Dot;

               Action     : constant Minimal_Action := Actions (I);
               Next_State : constant State_Index    :=
                 (case Action.Verb is
                  when Shift  => Action.State,
                  when Reduce => Goto_For
                    (Shared.Table.all,
                     Config.Stack.Peek (Base_Peek_Type (Action.Token_Count) + 1).State,
                     Action.Production.LHS));
            begin
               if Trace_McKenzie > Extra then
                  Put_Line
                    (Super.Trace.all, Super.Tree.all, Super.Stream (Parser_Index),
                     ": Minimal_Complete_Actions: " & Image (Action, Descriptor));
               end if;

               for Item of Shared.Table.States (Next_State).Kernel loop

                  if Matches (Item, Action) then
                     --  For Action.Verb = Reduce, more than one item may match
                     if Item.Length_After_Dot = 0 then
                        --  Set Length from a non-zero-length non-recursive item.
                        Length (I) := Length_After_Dot (Item, Action, Config.Stack);

                     elsif Item.Length_After_Dot < Length (I) then
                        if Trace_McKenzie > Extra then
                           --  Length_After_Dot outputs this in other branch
                           Super.Trace.Put (Next_State'Image & " " & Trimmed_Image (Item.Production));
                        end if;
                        Length (I) := Item.Length_After_Dot;

                     end if;

                     if Trace_McKenzie > Extra then
                        Super.Trace.Put (" length" & Length (I)'Image);
                     end if;
                     if Length (I) < Min_Length then
                        Min_Length := Length (I);
                     end if;
                  end if;
               end loop;
               if Trace_McKenzie > Extra then
                  Super.Trace.New_Line;
               end if;
            end;
         end loop Actions_Loop;

         for I in Length'Range loop
            if Length (I) = Min_Length then
               Safe_Add_Work ("2", (Actions (I), Table.McKenzie_Param.Minimal_Complete_Cost_Delta, Config));

            elsif Trace_McKenzie > Extra then
               Put_Line
                 (Super.Trace.all, Super.Tree.all, Super.Stream (Parser_Index), "Minimal_Complete_Actions: drop " &
                    Image (Actions (I), Descriptor) & " not minimal");
            end if;
         end loop;
      end Enqueue_Min_Actions;

   begin
      if Orig_Config.Stack.Depth = 1 then
         --  Get here with an empty source file, or a syntax error on the first
         --  token.
         return Token_ID_Arrays.Empty_Vector;

      elsif Orig_Config.Minimal_Complete_State = Done then
         if Trace_McKenzie > Extra then
            Put_Line
              (Super.Trace.all, Super.Tree.all, Super.Stream (Parser_Index), "Minimal_Complete_Actions: done");
         end if;
         return Token_ID_Arrays.Empty_Vector;
      end if;

      Enqueue_Min_Actions ("", Table.States (Orig_Config.Stack.Peek.State).Minimal_Complete_Actions, Orig_Config);

      loop
         exit when Is_Empty (Work);

         declare
            Item : Work_Item := Get (Work);
         begin
            if Trace_McKenzie > Extra then
               Put_Line
                 (Super.Trace.all, Super.Tree.all, Super.Stream (Parser_Index),
                  "Minimal_Complete_Actions: dequeue work item " &
                    Image (Item.Action, Descriptor));
            end if;

            case Item.Action.Verb is
            when Reduce =>
               --  Do a reduce, look at resulting state. Keep reducing until we can't
               --  anymore.
               declare
                  Reduce_Action : Reduce_Action_Rec := To_Reduce_Action (Item.Action);
                  Actions       : Minimal_Action_Arrays.Vector;
               begin
                  loop
                     Do_Reduce_1
                       ("Minimal_Complete_Actions", Super, Shared, Parser_Index, Local_Config_Heap, Item.Config,
                        Reduce_Action,
                        Do_Language_Fixes => False);

                     Actions := Table.States (Item.Config.Stack.Peek.State).Minimal_Complete_Actions;

                     case Actions.Length is
                     when 0 =>
                        if Trace_McKenzie > Detail then
                           Put_Line
                             (Super.Trace.all, Super.Tree.all, Super.Stream (Parser_Index),
                              "Minimal_Complete_Actions state" & Item.Config.Stack.Peek.State'Image &
                                " abandoned: no actions");
                        end if;
                        exit;
                     when 1 =>
                        case Actions (Actions.First_Index).Verb is
                        when Shift =>
                           Minimal_Do_Shift (Actions (Actions.First_Index), Item.Cost_Delta, Item.Config);
                           exit;
                        when Reduce =>
                           Reduce_Action := To_Reduce_Action (Actions (Actions.First_Index));
                        end case;

                     when others =>
                        Enqueue_Min_Actions ("multiple actions ", Actions, Item.Config);
                        exit;
                     end case;
                  end loop;
               end;

            when Shift =>
               Minimal_Do_Shift (Item.Action, Item.Cost_Delta, Item.Config);
            end case;
         end;
      end loop;

      if Inserted_Last = Inserted'First - 1 then
         --  Nothing inserted this round.
         if Orig_Config.Minimal_Complete_State = Active then
            Orig_Config.Minimal_Complete_State := Done;
         end if;
      end if;

      return To_Vector (Inserted (1 .. Inserted_Last));
   exception
   when Bad_Config =>
      return Token_ID_Arrays.Empty_Vector;
   end Insert_Minimal_Complete_Actions;

   procedure Insert_Matching_Begin
     (Super                 : not null access Base.Supervisor;
      Shared                : not null access Base.Shared;
      Parser_Index          : in              SAL.Base_Peek_Type;
      Config                : in              Configuration;
      Local_Config_Heap     : in out          Config_Heaps.Heap_Type;
      Matching_Begin_Tokens : in              Token_ID_Arrays.Vector)
   is
      Table      : Parse_Table renames Shared.Table.all;
      Descriptor : WisiToken.Descriptor renames Super.Tree.Descriptor.all;
   begin
      --  We don't check for insert = current token; that's either ok or a
      --  severe bug in Shared.Language_Matching_Begin_Tokens.

      if Config.Matching_Begin_Done then
         if Trace_McKenzie > Extra then
            Put_Line (Super.Trace.all, Super.Tree.all, Super.Stream (Parser_Index), "Matching_Begin abandoned: done");
         end if;
         return;
      end if;

      if Just_Pushed_Back_Or_Deleted (Super, Config, Matching_Begin_Tokens (Matching_Begin_Tokens.First_Index)) then
         if Trace_McKenzie > Extra then
            Put_Line
              (Super.Trace.all, Super.Tree.all, Super.Stream (Parser_Index), "Matching_Begin abandoned " &
                 Image (Matching_Begin_Tokens (Matching_Begin_Tokens.First_Index), Descriptor) & ": undo push_back");
         end if;
         return;
      end if;

      declare
         New_Config : Configuration := Config;
      begin
         for ID of Matching_Begin_Tokens loop
            Insert (Super.Tree.all, New_Config, ID);
         end loop;

         declare
            use Parse.Parse_Item_Arrays;
            Parse_Items : aliased Parse.Parse_Item_Arrays.Vector;
            Dummy : constant Boolean :=  Parse.Parse
              (Super, Shared, Parser_Index, Parse_Items, New_Config,
               Shared_Token_Goal => Syntax_Trees.Invalid_Node_Index,
               All_Conflicts     => True,
               Trace_Prefix      => "parse Matching_Begin");
         begin
            for I in First_Index (Parse_Items) .. Last_Index (Parse_Items) loop
               declare
                  Item : Parse.Parse_Item renames Parse.Parse_Item_Array_Refs.Variable_Ref (Parse_Items, I);
               begin
                  if Item.Parsed and Item.Config.Current_Insert_Delete = No_Insert_Delete then
                     Item.Config.Matching_Begin_Done := True;
                     Item.Config.Cost := Item.Config.Cost + Table.McKenzie_Param.Matching_Begin;
                     Item.Config.Strategy_Counts (Matching_Begin) := Item.Config.Strategy_Counts (Matching_Begin) + 1;
                     Item.Config.Error_Token    := Syntax_Trees.Invalid_Recover_Token;
                     Item.Config.Check_Status := (Label => WisiToken.Semantic_Checks.Ok);

                     if Trace_McKenzie > Detail then
                        Base.Put
                          ("Matching_Begin: insert " & Image (Matching_Begin_Tokens, Descriptor),
                           Super, Parser_Index, Item.Config);
                     end if;
                     Local_Config_Heap.Add (Item.Config);
                  else
                     if Trace_McKenzie > Detail then
                        Base.Put
                          ("Matching_Begin: abandon " & Image (Matching_Begin_Tokens, Descriptor) & ": parse fail",
                           Super, Parser_Index, Item.Config);
                     end if;
                  end if;
               end;
            end loop;
         end;
      end;
   exception
   when SAL.Container_Full =>
      --  From config_ops_sorted
      Super.Config_Full ("Minimal_Complete_Actions 3", Parser_Index);
   end Insert_Matching_Begin;

   procedure Try_Insert_Terminal
     (Super             : not null access Base.Supervisor;
      Shared            : not null access Base.Shared;
      Parser_Index      : in              SAL.Base_Peek_Type;
      Config            : in out          Configuration;
      Local_Config_Heap : in out          Config_Heaps.Heap_Type)
   is
      use all type WisiToken.Parse.LR.Parser.Language_Matching_Begin_Tokens_Access;
      Tokens                : Token_ID_Array_1_3;
      Matching_Begin_Tokens : Token_ID_Arrays.Vector;
      Forbid_Minimal_Insert : Boolean := False;

      Minimal_Inserted : Token_ID_Arrays.Vector;
   begin
      if Shared.Language_Matching_Begin_Tokens /= null then
         Parse.Current_Token_ID_Peek_3 (Super.Tree.all, Config, Tokens);

         Shared.Language_Matching_Begin_Tokens (Tokens, Config, Matching_Begin_Tokens, Forbid_Minimal_Insert);
      end if;

      if not Forbid_Minimal_Insert then
         --  See test_mckenzie_recover.adb Forbid_Minimal_Insert for rationale.
         Minimal_Inserted := Insert_Minimal_Complete_Actions
           (Super, Shared, Parser_Index, Config, Local_Config_Heap);
      end if;

      if Matching_Begin_Tokens.Length > 0 then
         Insert_Matching_Begin (Super, Shared, Parser_Index, Config, Local_Config_Heap, Matching_Begin_Tokens);
      end if;

      --  We always do all three; Insert_Minimal_Complete (unless
      --  Forbid_Minimal_Insert), Insert_Matching_Begin,
      --  Insert_From_Action_List; in general it's not possible to tell when
      --  one will be better (see test_mckenzie_recover.adb
      --  Always_Minimal_Complete, Always_Matching_Begin).
      --  Insert_From_Action_List does not insert the Minimal_Inserted tokens,
      --  and it will never insert the Matching_Begin_Tokens, so there is no
      --  duplication. Insert_From_Action_List will normally be more
      --  expensive.
      Insert_From_Action_List (Super, Shared, Parser_Index, Config, Minimal_Inserted, Local_Config_Heap);

      --  It is tempting to use the Goto_List to find nonterms to insert.
      --  But that can easily lead to error states, and it turns out to be
      --  not useful, especially if the grammar has been relaxed so most
      --  expressions and lists can be empty.

   exception
   when Bad_Config =>
      null;
   end Try_Insert_Terminal;

   procedure Try_Insert_Quote
     (Super             : not null access Base.Supervisor;
      Shared            : not null access Base.Shared;
      Parser_Index      : in              SAL.Base_Peek_Type;
      Config            : in out          Configuration;
      Local_Config_Heap : in out          Config_Heaps.Heap_Type)
   is
      --  FIXME: not updated to Config.Input_Stream

      use Config_Op_Arrays;
      use all type Parser.Language_String_ID_Set_Access;
      use WisiToken.Syntax_Trees;
      use Bounded_Streams;

      Tree        : Syntax_Trees.Tree renames Super.Tree.all;
      Descriptor  : WisiToken.Descriptor renames Super.Tree.Descriptor.all;
      Check_Limit : Syntax_Trees.Node_Index renames Shared.Table.McKenzie_Param.Check_Limit;

      Current_Line      : constant Line_Number_Type := Tree.Base_Token
        (Parse.Peek_Current_First_Shared_Terminal (Tree, Config)).Line;
      Lexer_Error_Token : Base_Token;

      function Recovered_Lexer_Error return Syntax_Trees.Terminal_Ref
      is begin
         --  We are assuming the list of lexer errors is short, so binary
         --  search would not be significantly faster.
         for Err of Shared.Wrapped_Lexer_Errors.all loop
            if Err.Recover_Token_Ref /= Syntax_Trees.Invalid_Stream_Node_Ref then
               Lexer_Error_Token := Tree.Base_Token (Err.Recover_Token_Ref.Node);

               if Lexer_Error_Token.Line = Current_Line then
                  return Err.Recover_Token_Ref;
               end if;
            end if;
         end loop;
         return Syntax_Trees.Invalid_Stream_Node_Ref;
      end Recovered_Lexer_Error;

      Lexer_Error_Token_Ref : constant Syntax_Trees.Terminal_Ref := Recovered_Lexer_Error;

      function String_ID_Set (String_ID : in Token_ID) return Token_ID_Set
      is begin
         if Shared.Language_String_ID_Set = null then
            return (String_ID .. String_ID => True);
         else
            return Shared.Language_String_ID_Set (Descriptor, String_ID);
         end if;
      end String_ID_Set;

      procedure Delete_All_Pushed_Back
        (Label          : in     String;
         Config         : in out Configuration;
         Max_Node_Index :    out Node_Index)
      --  Delete all tokens from Config.Input_Stream.
      --  Max_Node_Index is the last node_index deleted.
      is
         Stream    : Bounded_Streams.List renames Config.Input_Stream;
         To_Delete : Bounded_Streams.Cursor;
      begin
         Max_Node_Index := Invalid_Node_Index;
         loop
            exit when Length (Stream) = 0;

            if Tree.Child_Count (Stream (First (Stream))) > 0 then
               Parse.Breakdown (Tree, Config.Input_Stream);

            else
               To_Delete := First (Stream);

               declare
                  Node : constant Node_Access := Stream (To_Delete);
               begin
                  if Tree.Is_Shared (Node) then
                     if Is_Full (Config.Ops) then
                        Super.Config_Full ("insert quote 2 a " & Label, Parser_Index);
                        raise Bad_Config;
                     end if;

                     Append (Config.Ops, (Delete, Tree.ID (Node), Tree.Get_Node_Index (Node)));

                     Max_Node_Index := Tree.Get_Node_Index (Tree.Last_Shared_Terminal (Node));
                  end if;
               end;

               Delete (Config.Input_Stream, To_Delete);
            end if;
         end loop;
      end Delete_All_Pushed_Back;

      procedure Delete_Pushed_Back
        (Label          : in     String;
         Config         : in out Configuration;
         Target_Element : in out Bounded_Streams.Cursor;
         Target_Node    : in     Valid_Node_Access;
         Max_Node_Index :    out Node_Index)
      with Pre => Length (Config.Input_Stream) > 0
      --  Delete terminals First .. Target_Node - 1 in Config.Input_Stream;
      --  Target_Element must contain Target_Node. Max_Node_Index is the
      --  last node_index deleted. Target_Element is updated if Input_Stream
      --  is broken down to expose tokens.
      is
         Stream : Bounded_Streams.List renames Config.Input_Stream;

         procedure Delete_First
         is
            To_Delete : Bounded_Streams.Cursor := Stream.First;
         begin
            if Is_Full (Config.Ops) then
               Super.Config_Full ("insert quote 2 b " & Label, Parser_Index);
               raise Bad_Config;
            end if;

            Append
              (Config.Ops,
               (Delete,
                Tree.ID (Stream (To_Delete)),
                Tree.Get_Node_Index (Stream (To_Delete))));

            Max_Node_Index := Tree.Get_Node_Index
              (Tree.Last_Shared_Terminal (Stream (To_Delete)));

            Delete (Stream, To_Delete);
         end Delete_First;

      begin
         Max_Node_Index := Invalid_Node_Index;
         loop
            if Target_Element = Stream.First then
               exit when Target_Node = Stream (Stream.First);

               Parse.Breakdown (Tree, Stream);

               --  Find new Target_Element
               Target_Element := Stream.First;
               loop
                  exit when Tree.Is_Descendant_Of
                    (Root       => Stream (Target_Element),
                     Descendant => Target_Node);

                  Target_Element := Next (Stream, Target_Element);
               end loop;

               exit when Target_Element = Stream.First;
               Delete_First;
            else
               Delete_First;
            end if;
         end loop;
      end Delete_Pushed_Back;

      procedure Delete_Pushed_Back
        (Label             : in     String;
         Config            : in out Configuration;
         Target_Node_Index : in     Node_Index;
         Max_Node_Index    :    out Node_Index)
      --  Delete tokens from Config.Input_Stream to Target_Node_Index or
      --  end of Input_Stream, Max_Node_Index is the last node_index
      --  deleted; Invalid_Node_Index if none.
      is
         Stream         : Bounded_Streams.List renames Config.Input_Stream;
         Target_Element : Bounded_Streams.Cursor := First (Stream);
         Target_Node    : Node_Access := Invalid_Node_Access;
      begin
         if not Has_Element (Target_Element) then
            Max_Node_Index := Invalid_Node_Index;
            return;
         end if;

         if Tree.Get_Node_Index (Stream (Target_Element)) > Target_Node_Index then
            --  This would mean we are trying to delete a token that is still in Config.Stack.
            if Debug_Mode then
               raise SAL.Programmer_Error;
            else
               raise Bad_Config;
            end if;
         end if;

         if Tree.Get_Node_Index (Stream (Last (Stream))) < Target_Node_Index then
            --  Target_Node_Index is not in Config.Input_Stream; it's in
            --  Tree.Shared_Stream. Delete all of Config.Input_Stream.
            Delete_All_Pushed_Back (Label, Config, Max_Node_Index);
            return;
         end if;

         --  Find Target_Node
         Target_Node := Parse.First_Shared_Terminal (Tree, Stream);
         loop
            exit when Tree.Get_Node_Index (Target_Node) = Target_Node_Index;

            Parse.Next_Shared_Terminal (Tree, Stream, Target_Element, Target_Node);
         end loop;

         Delete_Pushed_Back (Label, Config, Target_Element, Target_Node, Max_Node_Index);
      end Delete_Pushed_Back;

      procedure String_Literal_In_Stack
        (Label             : in     String;
         Config            : in out Configuration;
         Matching          : in     SAL.Peek_Type;
         String_Literal_ID : in     Token_ID)
      --  Matching is the peek index of a token in Config.Stack
      --  containing a string literal. Push back thru that token, then
      --  delete all tokens after the string literal to Config current
      --  token.
      is
         String_Lit_Element : Bounded_Streams.Cursor := Last (Config.Input_Stream);
         String_Lit_Node    : Node_Access;
         Max_Deleted        : Node_Index;
         pragma Unreferenced (Max_Deleted);
      begin
         if not Has_Space (Config.Ops, Ada.Containers.Count_Type (Matching)) then
            Super.Config_Full ("insert quote 1 " & Label, Parser_Index);
            raise Bad_Config;
         end if;
         for I in 1 .. Matching loop
            if not Push_Back_Valid (Tree, Config, Super.Parser_State (Parser_Index).Resume_Token_Goal)
            then
               --  Probably pushing back thru a previously inserted token
               raise Bad_Config;
            end if;
            Do_Push_Back (Tree, Config);
         end loop;

         --  Search the pushed_back tokens for the last string literal.
         if String_Lit_Element = No_Element then
            String_Lit_Element := Last (Config.Input_Stream);
            String_Lit_Node    := Tree.Last_Shared_Terminal (Config.Input_Stream (String_Lit_Element));
         else
            String_Lit_Node := Tree.First_Shared_Terminal (Config.Input_Stream (String_Lit_Element));
            Parse.Prev_Shared_Terminal (Tree, Config.Input_Stream, String_Lit_Element, String_Lit_Node);
         end if;
         loop
            exit when Tree.ID (String_Lit_Node) = String_Literal_ID;

            Parse.Prev_Shared_Terminal (Tree, Config.Input_Stream, String_Lit_Element, String_Lit_Node);
         end loop;

         --  Delete pushed_back tokens to the string literal; keep the tokens
         --  before it.
         Delete_Pushed_Back (Label, Config, String_Lit_Element, String_Lit_Node, Max_Deleted);

         --  Parse the pushed back tokens before the string literal so
         --  Config matches Ops.
         declare
            Parse_Items : aliased Parse.Parse_Item_Arrays.Vector;
         begin
            if Parse.Parse
              (Super, Shared, Parser_Index, Parse_Items, Config,
               Shared_Token_Goal => Tree.Get_Node_Index (String_Lit_Node),
               All_Conflicts     => False,
               Trace_Prefix      => "insert quote parse pushback " & Label)
            then
               --  The tokens parsed without error. We don't care if any conflicts
               --  were encountered; they were enqueued the first time this was
               --  parsed.
               Config := Parse.Parse_Item_Array_Refs.Constant_Ref (Parse_Items, 1).Config;
               Append
                 (Config.Ops,
                  (Fast_Forward, Tree.Get_Node_Index (Parse.Peek_Current_First_Shared_Terminal (Tree, Config))));
            else
               raise SAL.Programmer_Error;
            end if;
         exception
         when Bad_Config =>
            raise SAL.Programmer_Error;
         end;
      end String_Literal_In_Stack;

      procedure Push_Back_Tokens
        (Full_Label            : in     String;
         Config                : in out Configuration;
         Min_Pushed_Back_Index :    out Syntax_Trees.Node_Index)
      --  Push back stack top; if it is empty, push back the next stack token.
      --
      --  Min_Pushed_Back_Index is token_index (first_shared_terminal (pushed back token)).
      is
         Item  : Recover_Stack_Item;
         First : Node_Access;
      begin
         Min_Pushed_Back_Index := Invalid_Node_Index;
         loop
            if not Push_Back_Valid (Tree, Config, Super.Parser_State (Parser_Index).Resume_Token_Goal)
            then
               --  Probably pushing back thru a previously inserted token
               raise Bad_Config;
            end if;

            if Is_Full (Config.Ops) then
               Super.Config_Full (Full_Label, Parser_Index);
               raise Bad_Config;
            end if;

            Item  := Config.Stack.Peek;
            First := Tree.First_Shared_Terminal (Tree.First_Terminal (Item.Token));

            Do_Push_Back (Tree, Config);

            if First /= Invalid_Node_Access then
               Min_Pushed_Back_Index := Tree.Get_Node_Index (First);
               exit;
            end if;
         end loop;
      end Push_Back_Tokens;

      procedure Delete_Shared_Stream
        (Label       : in     String;
         Config      : in out Configuration;
         First, Last : in     Syntax_Trees.Node_Index)
      --  Delete tokens First .. Last from Tree Shared_Stream.
      --  Config.Current_Shared_Token must be in First .. Last + 1. Leave
      --  Current_Shared_Token at Last + 1.
      is
         Index : Terminal_Ref := Config.Current_Shared_Token;

         procedure Find_First
         is begin
            if First = Tree.Get_Node_Index (Index.Node) then
               return;

            elsif First < Tree.Get_Node_Index (Index.Node) then
               loop
                  exit when Tree.Get_Node_Index (Index.Node) = First;
                  Tree.Prev_Shared_Terminal (Index);
               end loop;

            else
               raise Bad_Config;
            end if;
         end Find_First;

      begin
         if not Has_Space (Config.Ops, Ada.Containers.Count_Type (Last - First + 1)) then
            Super.Config_Full ("insert quote 3 " & Label, Parser_Index);
            raise Bad_Config;
         end if;

         Find_First;

         for I in First .. Last loop
            if not (Tree.Label (Index.Node) in Terminal_Label) then
               --  It is exceedingly unlikely that the words in a real user string
               --  will match a grammar production (unless we are writing a code
               --  generate like WisiToken.Output_Ada, sigh). So we just abandon
               --  this. FIXME: handle nonterms
               raise Bad_Config;
            end if;

            Append
              (Config.Ops,
               (Delete, Tree.ID (Index.Node),
                Tree.Get_Node_Index (Index.Node)));

            Tree.Next_Shared_Terminal (Index);
         end loop;
         Config.Current_Shared_Token := Index;
      end Delete_Shared_Stream;

      procedure Finish
        (Label       : in     String;
         Config      : in out Configuration;
         First, Last : in     Node_Index)
      --  Delete  tokens First .. Last from Config.Input_Stream and/or Tree Shared_Stream.
      --  Either First - 1 or Last + 1 should be a String_Literal.
      --  Config.Current_Shared_Token must be in First .. Last + 1. Leave
      --  Current_Shared_Token at Last + 1.
      is
         Adj_First : constant Node_Index := (if First = Invalid_Node_Index then Last else First);
         Adj_Last  : constant Node_Index := (if Last = Invalid_Node_Index then First else Last);

         Last_Deleted : Node_Index;
      begin
         if Adj_Last = Invalid_Node_Index or Adj_First = Invalid_Node_Index then
            raise Bad_Config;
         elsif Adj_Last < Adj_First then
            raise Bad_Config;
         end if;

         if Length (Config.Input_Stream) > 0 then
            Delete_Pushed_Back (Label, Config, Target_Node_Index => Last + 1, Max_Node_Index => Last_Deleted);
         end if;

         if Last_Deleted = Adj_Last then
            --  First .. Last deleted from input_stream.
            null;
         else
            Delete_Shared_Stream
              (Label, Config,
               First =>
                 (if Last_Deleted = Invalid_Node_Index
                  then Adj_First
                  else Last_Deleted + 1),
               Last => Last);
         end if;

         Config.Error_Token  := Syntax_Trees.Invalid_Recover_Token;
         Config.Check_Status := (Label => WisiToken.Semantic_Checks.Ok);

         --  This is a guess, so we give it a nominal cost
         Config.Cost := Config.Cost + 1;

         --  Let explore do insert after these deletes.
         Append (Config.Ops, (Fast_Forward, Tree.Get_Node_Index (Config.Current_Shared_Token.Node)));

         if Config.Resume_Token_Goal - Check_Limit <
           Tree.Get_Node_Index (Config.Current_Shared_Token.Node)
         then
            Config.Resume_Token_Goal := Tree.Get_Node_Index (Config.Current_Shared_Token.Node) +
              Check_Limit;
            if Trace_McKenzie > Extra then
               Put_Line
                 (Super.Trace.all, Tree, Super.Stream (Parser_Index), "resume_token_goal:" &
                    Config.Resume_Token_Goal'Image);
            end if;
         end if;

         Config.Strategy_Counts (String_Quote) := Config.Strategy_Counts (String_Quote) + 1;

         if Trace_McKenzie > Detail then
            Base.Put ("insert quote " & Label & " ", Super, Parser_Index, Config);
         end if;
      end Finish;

   begin
      --  When the lexer finds an unbalanced quote, it inserts a virtual
      --  balancing quote at the same character position as the unbalanced
      --  quote, returning an empty string literal token there. The parser
      --  does not see that as an error; it encounters a syntax error
      --  before, at, or after that string literal.
      --
      --  Here we assume the parse error in Config.Error_Token is due to
      --  putting the balancing quote in the wrong place (although we do
      --  check that; see test_mckenzie_recover.adb String_Quote_6), and
      --  attempt to find a better place to put the balancing quote. Then
      --  all tokens from the balancing quote to the unbalanced quote are
      --  now part of a string literal, so delete them, leaving just the
      --  string literal created by Lexer error recovery.

      --  First we check to see if there is an unbalanced quote in the
      --  current line; if not, just return. Some lexer errors are for other
      --  unrecognized characters; see ada_mode-recover_bad_char.adb.
      --
      --  An alternate strategy is to treat the lexer error as a parse error
      --  immediately, but that complicates the parse logic.

      Config.String_Quote_Checked := Current_Line;

      if Lexer_Error_Token_Ref = Syntax_Trees.Invalid_Stream_Node_Ref or else
        Lexer_Error_Token.ID not in Descriptor.String_1_ID | Descriptor.String_2_ID
      then
         return;
      end if;

      --  It is not possible to tell where the best place to put the
      --  balancing quote is, so we always try all reasonable places.

      if Lexer_Error_Token.Byte_Region.First = Config.Error_Token.Byte_Region.First then
         --  The parse error token is the string literal at the lexer error.
         --
         --  case a: Insert the balancing quote somewhere before the error
         --  point. There is no way to tell how far back to put the balancing
         --  quote, so we just do one non-empty token. See
         --  test_mckenzie_recover.adb String_Quote_0. So far we have not found
         --  a test case for more than one token.
         declare
            New_Config            : Configuration := Config;
            Min_Pushed_Back_Index : Syntax_Trees.Node_Index;
         begin
            Push_Back_Tokens ("insert quote 4 a", New_Config, Min_Pushed_Back_Index);
            Finish
              ("a", New_Config,
               First => Min_Pushed_Back_Index,
               Last  => Tree.Get_Node_Index (Tree.Prev_Shared_Terminal (Config.Current_Shared_Token).Node));
            Local_Config_Heap.Add (New_Config);
         end;

         --  Note that it is not reasonable to insert a quote after the error
         --  in this case. If that were the right solution, the parser error
         --  token would not be the lexer repaired string literal, since a
         --  string literal would be legal here.

      elsif Lexer_Error_Token.Byte_Region.First < Config.Error_Token.Byte_Region.First then
         --  The unbalanced quote is before the parse error token; see
         --  test_mckenzie_recover.adb String_Quote_2.
         --
         --  The missing quote belongs after the parse error token, before or
         --  at the end of the current line; try inserting it at the end of
         --  the current line.
         --
         --  The lexer repaired string literal may be in a reduced token on the
         --  stack.

         declare
            Matching : SAL.Peek_Type := 1;
         begin
            Find_Descendant_ID
              (Tree, Config, Lexer_Error_Token.ID,
               String_ID_Set (Lexer_Error_Token.ID), Matching);

            if Matching = Config.Stack.Depth then
               --  String literal is in a virtual nonterm; it is not from the lexer
               --  error, so abandon this.
               if Trace_McKenzie > Detail then
                  Put_Line
                    (Super.Trace.all, Tree, Super.Stream (Parser_Index),
                     "insert quote b abandon; string literal in virtual");
               end if;
               return;
            end if;

            declare
               New_Config : Configuration := Config;
            begin
               String_Literal_In_Stack ("b", New_Config, Matching, Lexer_Error_Token.ID);

               Finish
                 ("b", New_Config,
                  First => Syntax_Trees.Get_Node_Index (Config.Current_Shared_Token.Node),
                  Last  => Syntax_Trees.Get_Node_Index
                    (Tree.Prev_Shared_Terminal (Shared.Line_Begin_Token.all (Current_Line + 1)).Node));
               Local_Config_Heap.Add (New_Config);
            end;
         end;

      else
         --  The unbalanced quote is after the parse error token.

         --  case c: Assume a missing quote belongs immediately before the current token.
         --  See test_mckenzie_recover.adb String_Quote_3.
         declare
            New_Config : Configuration := Config;
         begin
            Finish
              ("c", New_Config,
               First => Syntax_Trees.Get_Node_Index (Parse.Peek_Current_First_Shared_Terminal (Tree, New_Config)),
               Last  => Syntax_Trees.Get_Node_Index (Tree.Prev_Shared_Terminal (Lexer_Error_Token_Ref).Node));
            Local_Config_Heap.Add (New_Config);
         end;

         --  case d: Assume a missing quote belongs somewhere farther before
         --  the current token; try one non-empty (as in case a above). See
         --  test_mckenzie_recover.adb String_Quote_4, String_Quote_6.
         declare
            New_Config            : Configuration := Config;
            Min_Pushed_Back_Index : Syntax_Trees.Node_Index;
         begin
            Push_Back_Tokens ("insert quote 5 d", New_Config, Min_Pushed_Back_Index);
            Finish
              ("d", New_Config,
               First => Min_Pushed_Back_Index,
               Last  => Syntax_Trees.Get_Node_Index (Tree.Prev_Shared_Terminal (Lexer_Error_Token_Ref).Node));
            Local_Config_Heap.Add (New_Config);
         exception
         when SAL.Container_Empty =>
            --  From Stack.Pop
            null;
         when Bad_Config =>
            null;
         end;

         --  case e: Assume the actual error is an extra quote that terminates
         --  an intended string literal early, in which case there is a token
         --  on the stack containing the string literal that should be extended
         --  to the found quote. See test_mckenzie_recover.adb String_Quote_1.
         declare
            Matching : SAL.Peek_Type := 1;
         begin
            --  Lexer_Error_Token is a string literal; find a matching one.
            Find_Descendant_ID
              (Tree, Config, Lexer_Error_Token.ID,
               String_ID_Set (Lexer_Error_Token.ID), Matching);

            if Matching = Config.Stack.Depth then
               --  No matching string literal, so this case does not apply.
               null;
            else
               declare
                  New_Config : Configuration := Config;
               begin
                  String_Literal_In_Stack ("e", New_Config, Matching, Lexer_Error_Token.ID);

                  Finish
                    ("e", New_Config,
                     First => Syntax_Trees.Get_Node_Index (Config.Current_Shared_Token.Node),
                     Last  => Syntax_Trees.Get_Node_Index (Lexer_Error_Token_Ref.Node));
                  Local_Config_Heap.Add (New_Config);
               end;
            end if;
         end;
      end if;
   exception
   when Bad_Config =>
      null;
   end Try_Insert_Quote;

   procedure Try_Delete_Input
     (Super             : not null access Base.Supervisor;
      Shared            : not null access Base.Shared;
      Parser_Index      : in              SAL.Base_Peek_Type;
      Config            : in              Configuration;
      Local_Config_Heap : in out          Config_Heaps.Heap_Type)
   is
      --  Try deleting (= skipping) the current shared input token.

      use Config_Op_Arrays, Config_Op_Array_Refs;
      EOF_ID      : constant Token_ID                := Super.Tree.Descriptor.EOI_ID;
      Check_Limit : constant Syntax_Trees.Node_Index := Shared.Table.McKenzie_Param.Check_Limit;

      McKenzie_Param : McKenzie_Param_Type renames Shared.Table.McKenzie_Param;

      Next_Node       : constant Syntax_Trees.Valid_Node_Access := Parse.Peek_Current_First_Shared_Terminal
        (Super.Tree.all, Config);
      Next_Node_Index : constant Syntax_Trees.Node_Index        := Super.Tree.Get_Node_Index (Next_Node);
      Next_ID         : constant Token_ID                       := Super.Tree.ID (Next_Node);
   begin
      if Next_ID /= EOF_ID and then
         --  can't delete EOF
         (Length (Config.Ops) = 0 or else
           --  Don't delete an ID we just inserted; waste of time, leads to infinite loop
           (not Equal (Constant_Ref (Config.Ops, Last_Index (Config.Ops)), (Insert, Next_ID, Next_Node_Index))))
      then
         declare
            New_Config : Configuration := Config;

            function Matching_Push_Back return Boolean
            is begin
               for I in reverse First_Index (New_Config.Ops) .. Last_Index (New_Config.Ops) loop
                  declare
                     Op : Config_Op renames Config_Op_Array_Refs.Variable_Ref (New_Config.Ops, I).Element.all;
                  begin
                     exit when not (Op.Op in Undo_Reduce | Push_Back | Delete);
                     if Op = (Push_Back, Next_ID, Next_Node_Index) then
                        return True;
                     end if;
                  end;
               end loop;
               return False;
            end Matching_Push_Back;
         begin
            New_Config.Error_Token    := Syntax_Trees.Invalid_Recover_Token;
            New_Config.Check_Status   := (Label => WisiToken.Semantic_Checks.Ok);

            New_Config.Cost := New_Config.Cost + McKenzie_Param.Delete (Next_ID);
            New_Config.Strategy_Counts (Delete) := Config.Strategy_Counts (Delete) + 1;

            if Matching_Push_Back then
               --  We are deleting a push_back; cancel the push_back cost, to make
               --  this the same as plain deleting.
               New_Config.Cost := Natural'Max (Natural'First, New_Config.Cost - McKenzie_Param.Push_Back (Next_ID));
            end if;

            if Is_Full (New_Config.Ops) then
               Super.Config_Full ("delete", Parser_Index);
               raise Bad_Config;
            else
               Append (New_Config.Ops, (Delete, Next_ID, Next_Node_Index));
            end if;

            Parse.Do_Delete (Super.Tree.all, New_Config);

            declare --  FIXME: merge into Do_Delete?
               New_Next_Node_Index : constant Syntax_Trees.Node_Index := Super.Tree.Get_Node_Index
                 (Parse.Peek_Current_First_Shared_Terminal (Super.Tree.all, New_Config));
            begin
               if New_Config.Resume_Token_Goal - Check_Limit < New_Next_Node_Index then
                  New_Config.Resume_Token_Goal := New_Next_Node_Index + Check_Limit;
               end if;
            end;

            Local_Config_Heap.Add (New_Config);

            if Trace_McKenzie > Detail then
               Base.Put
                 ("delete " & Image (Next_ID, Super.Tree.Descriptor.all), Super, Parser_Index, New_Config);
            end if;
         end;
      end if;
   end Try_Delete_Input;

   procedure Process_One
     (Super         : not null access Base.Supervisor;
      Shared        : not null access Base.Shared;
      Config_Status : out             Base.Config_Status)
   is
      --  Get one config from Super, check to see if it is a viable
      --  solution. If not, enqueue variations to check.

      use all type Base.Config_Status;
      use all type Parser.Language_Fixes_Access;
      use all type Semantic_Checks.Check_Status_Label;

      Trace      : WisiToken.Trace'Class renames Super.Trace.all;
      Descriptor : WisiToken.Descriptor renames Super.Tree.Descriptor.all;
      Table      : Parse_Table renames Shared.Table.all;

      Parser_Index : SAL.Base_Peek_Type;
      Config       : Configuration;

      Local_Config_Heap : Config_Heaps.Heap_Type;
      --  We collect all the variants to enqueue, then deliver them all at
      --  once to Super, to minimizes task interactions.
   begin
      Super.Get (Parser_Index, Config, Config_Status);

      if Config_Status = All_Done then
         return;
      end if;

      if Trace_McKenzie > Detail then
         Base.Put ("dequeue", Super, Parser_Index, Config);
      end if;

      --  Fast_Forward; parse Insert, Delete in Config.Ops that have not
      --  been parsed yet. 'parse' here means adjusting Config.Stack and
      --  Current_Terminal_Index. Code in this file always parses when
      --  adding ops to Config (except as noted); Language_Fixes should use
      --  McKenzie_Recover.Insert, Delete instead.
      if Config.Current_Insert_Delete = 1 then
         --  Config is from Language_Fixes.

         Fast_Forward (Super, Shared, Parser_Index, Local_Config_Heap, Config);
         Super.Put (Parser_Index, Local_Config_Heap);
         return;
      end if;

      pragma Assert (Config.Current_Insert_Delete = 0);
      --  Config.Current_Insert_Delete > 0 is a programming error.

      if Syntax_Trees.ID (Config.Error_Token) /= Invalid_Token_ID then
         if Shared.Language_Fixes = null then
            null;
         else
            Shared.Language_Fixes
              (Trace, Shared.Lexer, Super.Stream (Parser_Index), Shared.Table.all, Super.Tree.all, Local_Config_Heap,
               Config);

            --  The solutions enqueued by Language_Fixes should be lower cost than
            --  others (typically 0), so they will be checked first.
         end if;

         if Config.Check_Status.Label = Ok then
            --  Parse table Error action.
            --
            --  We don't clear Config.Error_Token here, because
            --  Language_Use_Minimal_Complete_Actions needs it. We only clear it
            --  when a parse results in no error (or a different error), or a
            --  push_back moves the Current_Token.
            null;

         else
            --  Assume "ignore check error" is a viable solution. But give it a
            --  cost, so a solution provided by Language_Fixes is preferred.

            declare
               New_State : Unknown_State_Index;
            begin
               Config.Cost := Config.Cost + Table.McKenzie_Param.Ignore_Check_Fail;
               Config.Strategy_Counts (Ignore_Error) := Config.Strategy_Counts (Ignore_Error) + 1;

               declare
                  use Config_Op_Arrays, Config_Op_Array_Refs;
                  Last : constant SAL.Base_Peek_Type := Last_Index (Config.Ops);
               begin
                  if Last /= SAL.Invalid_Peek_Index and then
                    Constant_Ref (Config.Ops, Last).Op = Undo_Reduce and then
                    Constant_Ref (Config.Ops, Last).Nonterm = Syntax_Trees.ID (Config.Error_Token)
                  then
                     --  We are ignoring this undo_reduce.
                     Delete_Last (Config.Ops);
                  end if;
               end;

               --  finish reduce.
               Config.Stack.Pop (SAL.Base_Peek_Type (Config.Check_Token_Count));

               New_State := Goto_For (Table, Config.Stack.Peek.State, Syntax_Trees.ID (Config.Error_Token));

               if New_State = Unknown_State then
                  if Config.Stack.Depth = 1 then
                     --  Stack is empty, and we did not get Accept; really bad syntax got
                     --  us here; abandon this config. See ada_mode-recover_bad_char.adb.
                     Super.Put (Parser_Index, Local_Config_Heap);
                     return;
                  else
                     raise SAL.Programmer_Error with
                       "process_one found test case for new_state = Unknown; old state " &
                       Trimmed_Image (Config.Stack.Peek.State) & " nonterm " & Image
                         (Syntax_Trees.ID (Config.Error_Token), Super.Tree.Descriptor.all);
                  end if;
               end if;

               Config.Stack.Push ((New_State, Config.Error_Token));

               --  We _don't_ clear Check_Status and Error_Token here; Check needs
               --  them, and sets them as appropriate.

               if Trace_McKenzie > Detail then
                  Base.Put ("ignore check error and continue", Super, Parser_Index, Config);
               end if;
            end;
         end if;
      end if;

      --  Call Check to see if this config succeeds.
      case Check (Super, Shared, Parser_Index, Config, Local_Config_Heap) is
      when Success =>
         Super.Success (Parser_Index, Config, Local_Config_Heap);
         return;

      when Abandon =>
         Super.Put (Parser_Index, Local_Config_Heap);
         return;

      when Continue =>
         null;

      end case;

      if Trace_McKenzie > Detail then
         Base.Put ("continuing", Super, Parser_Index, Config);
         if Trace_McKenzie > Extra then
            Put_Line
              (Trace, Super.Tree.all, Super.Stream (Parser_Index), "stack: " & LR.Image (Config.Stack, Super.Tree.all));
         end if;
      end if;

      --  Grouping these operations (push_back, delete, insert) ensures that
      --  there are no duplicate solutions found. We reset the grouping
      --  after each fast_forward.
      --
      --  We do delete before insert so Insert_Matching_Begin can operate on
      --  the new next token, before Fast_Forwarding past it.
      --
      --  All possible permutations will be explored.

      pragma Assert (Config.Stack.Depth > 0);

      Try_Insert_Terminal (Super, Shared, Parser_Index, Config, Local_Config_Heap);

      if Push_Back_Valid (Super.Tree.all, Config, Super.Parser_State (Parser_Index).Resume_Token_Goal) and then
        (not Syntax_Trees.Is_Empty_Nonterm (Config.Stack.Peek.Token, Super.Tree.Descriptor.all) and
           --  We only allow Push_Back of empty nonterm from Language_Fixes;
           --  otherwise it is usually redundant with Undo_Reduce.
           not Check_Reduce_To_Start (Super, Shared, Parser_Index, Config))
           --  If Config reduces to the start nonterm, there's no point in Push_Back or Undo_Reduce.
      then
         Try_Push_Back (Super, Shared, Parser_Index, Config, Local_Config_Heap);
      end if;

      if Undo_Reduce_Valid (Super.Tree.all, Config) then
         Try_Undo_Reduce (Super, Shared, Parser_Index, Config, Local_Config_Heap);
      end if;

      if None_Since_FF (Config.Ops, Insert) then
         Try_Delete_Input (Super, Shared, Parser_Index, Config, Local_Config_Heap);
      end if;

      --  This is run once per input line, independent of what other ops
      --  have been done.
      if Config.Check_Status.Label = Ok and
        (Descriptor.String_1_ID /= Invalid_Token_ID or Descriptor.String_2_ID /= Invalid_Token_ID) and
        (Config.String_Quote_Checked = Invalid_Line_Number or else Config.String_Quote_Checked <
           Super.Tree.Base_Token (Parse.Peek_Current_First_Shared_Terminal (Super.Tree.all, Config)).Line)
      then
         --  See if there is a mismatched quote. The solution is to delete
         --  tokens, nominally replacing them with an expanded string literal.
         --  So we try this when it is ok to try delete.
         if None_Since_FF (Config.Ops, Insert) then
            Try_Insert_Quote (Super, Shared, Parser_Index, Config, Local_Config_Heap);
         end if;
      end if;

      Super.Put (Parser_Index, Local_Config_Heap);
   exception
   when Bad_Config =>
      --  Just abandon this config; tell Super we are done.
      Super.Put (Parser_Index, Local_Config_Heap);

   when E : others =>
      Super.Put (Parser_Index, Local_Config_Heap);
      if Debug_Mode then
         raise;
      elsif Trace_McKenzie > Outline then
         Put_Line
           (Super.Trace.all, Super.Tree.all, Super.Stream (Parser_Index),
            "Process_One: unhandled exception " & Ada.Exceptions.Exception_Name (E) & ": " &
              Ada.Exceptions.Exception_Message (E));
      end if;
   end Process_One;

end WisiToken.Parse.LR.McKenzie_Recover.Explore;
