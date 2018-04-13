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

with WisiToken.LR.McKenzie_Recover.Parse;
package body WisiToken.LR.McKenzie_Recover.Explore is

   procedure Do_Shift
     (Super             : not null access Base.Supervisor;
      Shared            : not null access Base.Shared_Lookahead;
      Parser_Index      : in              SAL.Peek_Type;
      Local_Config_Heap : in out          Config_Heaps.Heap_Type;
      Config            : in out          Configuration;
      State             : in              State_Index;
      ID                : in              Token_ID;
      Cost_Delta        : in              Integer)
   is
      use all type SAL.Base_Peek_Type;
      McKenzie_Param : McKenzie_Param_Type renames Shared.Shared_Parser.Table.McKenzie_Param;

      Op : constant Config_Op := (Insert, ID, Config.Current_Shared_Token);
   begin
      if Config.Ops_Insert_Point = Config_Op_Arrays.No_Index then
         Config.Ops.Append (Op);
      else
         Config.Ops.Insert (Op, Before => Config.Ops_Insert_Point);
         Config.Inserted.Insert (ID, Before => Config.Current_Inserted);
         Config.Current_Inserted := Config.Current_Inserted + 1;
      end if;

      Config.Cost := Config.Cost + McKenzie_Param.Insert (ID) + Cost_Delta;

      Config.Stack.Push ((State, Syntax_Trees.Invalid_Node_Index, (ID, Virtual => True, others => <>)));
      if Trace_McKenzie > Detail then
         Base.Put ("insert " & Image (ID, Super.Trace.Descriptor.all), Super, Shared, Parser_Index, Config);
      end if;

      Local_Config_Heap.Add (Config);
   end Do_Shift;

   function Do_Reduce_1
     (Super             : not null access Base.Supervisor;
      Shared            : not null access Base.Shared_Lookahead;
      Parser_Index      : in              SAL.Peek_Type;
      Local_Config_Heap : in out          Config_Heaps.Heap_Type;
      Config            : in out          Configuration;
      Action            : in              Reduce_Action_Rec)
     return Non_Success_Status
   is
      --  Perform Action on Config, setting Config.Check_Status. If that is
      --  not Ok, call Semantic_Check_Fixes (which may enqueue configs),
      --  return Abandon. Otherwise return Continue.
      use all type Semantic_Checks.Check_Status_Label;

      Table     : Parse_Table renames Shared.Shared_Parser.Table.all;
      Nonterm   : Recover_Token;
      New_State : State_Index;
   begin
      Config.Check_Status := Parse.Reduce_Stack (Shared, Config.Stack, Action, Nonterm, Default_Virtual => True);
      case Config.Check_Status.Label is
      when Ok =>
         null;

      when Semantic_Checks.Error =>
         Config.Error_Token       := Nonterm;
         Config.Check_Token_Count := Action.Token_Count;

         if Shared.Shared_Parser.Language_Fixes = null then
            --  No fixes available; abandon Config.
            return Abandon;
         else
            case Shared.Shared_Parser.Language_Fixes
              (Super.Trace.all, Shared.Shared_Parser.Lexer, Super.Label (Parser_Index),
               Shared.Shared_Parser.Terminals, Super.Parser_State (Parser_Index).Tree, Local_Config_Heap,
               Config)
            is
            when Continue =>
               --  "ignore error" is viable; continue with Config.
               --  Finish the reduce.
               Config.Stack.Pop (SAL.Base_Peek_Type (Config.Check_Token_Count));
               Config.Error_Token.ID := Invalid_Token_ID;
               Config.Check_Status   := (Label => Ok);

            when Abandon =>
               return Abandon;
            end case;
         end if;
      end case;

      New_State := Goto_For (Table, Config.Stack (1).State, Action.LHS);

      Config.Stack.Push ((New_State, Syntax_Trees.Invalid_Node_Index, Nonterm));
      return Continue;
   end Do_Reduce_1;

   procedure Do_Reduce_2
     (Super             : not null access Base.Supervisor;
      Shared            : not null access Base.Shared_Lookahead;
      Parser_Index      : in              SAL.Peek_Type;
      Local_Config_Heap : in out          Config_Heaps.Heap_Type;
      Config            : in out          Configuration;
      Inserted_ID       : in              Token_ID;
      Cost_Delta        : in              Integer)
   is
      --  Perform reduce actions until shift Inserted_Token; if all succeed,
      --  add the final configuration to the heap. If a conflict is
      --  encountered, process the other action the same way. If a semantic
      --  check fails, enqueue possible solutions. For parse table error
      --  actions, just return.

      use all type Semantic_Checks.Check_Status_Label;

      Table       : Parse_Table renames Shared.Shared_Parser.Table.all;
      Next_Action : Parse_Action_Node_Ptr;
   begin
      Next_Action := Action_For (Table, Config.Stack (1).State, Inserted_ID);

      if Next_Action.Next /= null then
         --  There is a conflict; create a new config to shift or reduce.
         declare
            New_Config : Configuration := Config;
            Action     : Parse_Action_Rec renames Next_Action.Next.Item;
         begin
            case Action.Verb is
            when Shift =>
               Do_Shift
                 (Super, Shared, Parser_Index, Local_Config_Heap, New_Config, Action.State, Inserted_ID, Cost_Delta);

            when Reduce =>
               case Do_Reduce_1 (Super, Shared, Parser_Index, Local_Config_Heap, New_Config, Action) is
               when Abandon =>
                  null;
               when Continue =>
                  Do_Reduce_2 (Super, Shared, Parser_Index, Local_Config_Heap, New_Config, Inserted_ID, Cost_Delta);
               end case;

            when Accept_It =>
               raise Programmer_Error with "found test case for Do_Reduce Accept_It conflict";

            when Error =>
               null;
            end case;
         end;

         --  There can be only one conflict.
      end if;

      case Next_Action.Item.Verb is
      when Shift =>
         Do_Shift
           (Super, Shared, Parser_Index, Local_Config_Heap, Config, Next_Action.Item.State, Inserted_ID, Cost_Delta);

      when Reduce =>
         case Do_Reduce_1 (Super, Shared, Parser_Index, Local_Config_Heap, Config, Next_Action.Item) is
         when Abandon =>
            null;
         when Continue =>
            Do_Reduce_2 (Super, Shared, Parser_Index, Local_Config_Heap, Config, Inserted_ID, Cost_Delta);
         end case;

      when Accept_It =>
         raise Programmer_Error with "found test case for Do_Reduce Accept_It";

      when Error =>
         null;
      end case;

   end Do_Reduce_2;

   function Fast_Forward
     (Super                  : not null access Base.Supervisor;
      Shared                 : not null access Base.Shared_Lookahead;
      Parser_Index           : in              SAL.Base_Peek_Type;
      Config                 : in out          Configuration;
      Post_Fast_Forward_Fail : in out          Boolean)
     return Non_Success_Status
   is
      --  Apply the ops in Config; they were inserted by Language_Fixes.
      --  Return Abandon if Config should be abandoned, otherwise Continue.

      use all type SAL.Base_Peek_Type;
      use all type Ada.Containers.Count_Type;
      use all type Semantic_Checks.Check_Status_Label;

      Parse_Items : Parse.Parse_Item_Arrays.Vector;
   begin
      if Parse.Parse
        (Super, Shared, Parser_Index, Parse_Items, Config,
         Shared_Token_Goal => Invalid_Token_Index,
         Trace_Prefix      => "fast_forward")
      then
         --  The tokens inserted by Language_Fixes parsed without error, so
         --  continue with the parsed config.
         if Parse_Items.Length = 1 then
            Config := Parse_Items (1).Config;
            Config.Ops.Append ((Fast_Forward, Config.Current_Shared_Token));

            Config.Ops_Insert_Point := Config_Op_Arrays.No_Index;
         else
            --  FIXME: figure out how to deal with this; need a test case
            raise Programmer_Error with "fast_forward returned multiple configs";
         end if;

      else
         --  This indicates that Language_Fixes did not fix all the
         --  problems; see test_mckenzie_recover Two_Missing_Ends. We hope it
         --  made progress, so we try to keep going.
         declare
            Good_Item_Index : Natural := 0;
            Good_Item_Count : Natural := 0;
         begin
            for I in Parse_Items.First_Index .. Parse_Items.Last_Index loop
               declare
                  Parsed_Config    : Configuration renames Parse_Items (I).Config;
                  Insert_Ops_Count : SAL.Base_Peek_Type := 0;
               begin
                  if Parsed_Config.Current_Inserted = No_Inserted then
                     --  Any tokens inserted by Language_Fixes were consumed, so we
                     --  can continue with the parsed config.
                     Parsed_Config.Ops_Insert_Point := Config_Op_Arrays.No_Index;

                     Good_Item_Index := I;
                     Good_Item_Count := Good_Item_Count + 1;

                  elsif Config.Check_Status.Label /= Ok then
                     --  The tokens inserted by Language_Fixes caused this, so we assume it
                     --  can't fix it now.
                     null;

                  else
                     --  We need new insertions to be made at the current input point in
                     --  Config.Inserted, not at Config.Current_Shared_Token. Similarly,
                     --  insert new Ops at the correct point in Config.Ops. We don't try
                     --  this more than once. FIXME: why not?
                     if Parsed_Config.Ops_Insert_Point = Config_Op_Arrays.No_Index then

                        Good_Item_Index := I;
                        Good_Item_Count := Good_Item_Count + 1;

                        Post_Fast_Forward_Fail := True;
                        Insert_Ops_Count := SAL.Base_Peek_Type (Parsed_Config.Inserted.Length);
                        for I in reverse Parsed_Config.Ops.First_Index .. Parsed_Config.Ops.Last_Index loop
                           if Parsed_Config.Ops (I).Op = Insert then
                              if Insert_Ops_Count = Parsed_Config.Current_Inserted then
                                 Parsed_Config.Current_Shared_Token := Parsed_Config.Ops (I).Token_Index;
                                 Parsed_Config.Ops_Insert_Point := I;
                                 exit;
                              end if;
                              Insert_Ops_Count := Insert_Ops_Count - 1;
                           end if;
                        end loop;
                        if Parsed_Config.Ops_Insert_Point = Config_Op_Arrays.No_Index then
                           raise Programmer_Error;
                        end if;
                     end if;
                  end if;
               end;
            end loop;

            if Good_Item_Count = 0 then
               --  Nothing more to do; abandon config.
               return Abandon;

            elsif Good_Item_Count = 1 then
               Config := Parse_Items (Good_Item_Index).Config;
               Parse_Items.Clear;
            else
               --  FIXME: figure out how to deal with this; need a test case
               raise Programmer_Error with "fast_forward returned multiple configs";
            end if;
         end;
      end if;
      return Continue;
   end Fast_Forward;

   function Check
     (Super             : not null access Base.Supervisor;
      Shared            : not null access Base.Shared_Lookahead;
      Parser_Index      : in              SAL.Base_Peek_Type;
      Config            : in out          Configuration;
      Local_Config_Heap : in out          Config_Heaps.Heap_Type)
     return Check_Status
   is
      use all type Ada.Containers.Count_Type;
      use all type Syntax_Trees.Node_Index;
      use all type Semantic_Checks.Check_Status_Label;

      Parse_Items : Parse.Parse_Item_Arrays.Vector;
   begin
      if Parse.Parse
        (Super, Shared, Parser_Index, Parse_Items, Config, Shared.Shared_Parser.Resume_Token_Goal, "check")
      then
         return Success;
      end if;

      --  Enqueue failed Parse_Items so Language_Fixes can try to fix them.
      declare
         Parse_Error_Found : Boolean := False;
      begin
         for Item of Parse_Items loop
            if Item.Parsed then
               if Item.Config.Error_Token.ID /= Invalid_Token_ID and Item.Config.Check_Status.Label = Ok then
                  Parse_Error_Found := True;

                  if Item.Shift_Count = 0 or
                    ((Item.Config.Ops.Length > 0 and then
                        Item.Config.Ops (Item.Config.Ops.Last_Index).Op in Undo_Reduce | Push_Back) and
                       Item.Config.Current_Shared_Token = Config.Current_Shared_Token)
                  then
                     --  (Item.config.ops is empty on the very first Check). This is the
                     --  same error Config originally found; report it in Config, so
                     --  Language_Constrain_Terminals can see it.
                     Config.Error_Token  := Item.Config.Error_Token;
                     Config.Check_Status := (Label => Ok);
                  end if;
               end if;

               if Item.Shift_Count > 0 and then
                 (Item.Config.Check_Status.Label /= Ok or
                    (Item.Config.Error_Token.ID /= Invalid_Token_ID and then
                       Item.Config.Ops (Item.Config.Ops.Last_Index).Op in Insert | Delete | Fast_Forward))
               then
                  --  Some progress was made; let Language_Fixes try to fix the new
                  --  error.
                  --
                  --  This is abandoning the original location of the error, which may
                  --  not be entirely fixed. So we increase the cost. See
                  --  test_mckenzie_recover Loop_Bounds.
                  Item.Config.Cost := Item.Config.Cost + 1;
                  Item.Config.Ops.Append ((Fast_Forward, Item.Config.Current_Shared_Token));

                  Local_Config_Heap.Add (Item.Config);
                  if Trace_McKenzie > Detail then
                     Base.Put ("for Language_Fixes ", Super, Shared, Parser_Index, Item.Config);
                  end if;

               end if;
            end if;
         end loop;

         if Parse_Error_Found then
            return Continue;
         else
            --  Failed due to Semantic_Check
            if Shared.Shared_Parser.Language_Fixes = null then
               --  Only fix is to ignore the error
               return Continue;
            else
               --  Assume Language_Fixes handles this, not Explore.
               return Abandon;
            end if;
         end if;
      end;
   end Check;

   procedure Try_Push_Back
     (Super             : not null access Base.Supervisor;
      Shared            : not null access Base.Shared_Lookahead;
      Parser_Index      : in              SAL.Base_Peek_Type;
      Config            : in out          Configuration;
      Local_Config_Heap : in out          Config_Heaps.Heap_Type)
   is
      Trace          : WisiToken.Trace'Class renames Super.Trace.all;
      McKenzie_Param : McKenzie_Param_Type renames Shared.Shared_Parser.Table.McKenzie_Param;

      Token : constant Recover_Token := Config.Stack (1).Token;
   begin
      --  Try pushing back the stack top, to allow insert and other
      --  operations at that point.
      --
      --  Since we are not actually changing the source text, it is tempting
      --  to give this operation zero cost. But then we keep doing push_back
      --  forever, making no progress. So we give it a cost.

      if not Token.Virtual then
         --  If Virtual, this is from earlier in this recover session; no point
         --  in trying to redo it.

         declare
            New_Config : constant Configuration_Access := Local_Config_Heap.Add (Config);
         begin
            New_Config.Error_Token.ID := Invalid_Token_ID;
            New_Config.Check_Status   := (Label => WisiToken.Semantic_Checks.Ok);

            New_Config.Stack.Pop;

            if Token.Min_Terminal_Index = Invalid_Token_Index then
               --  Token is empty; Config.current_shared_token does not change, no
               --  cost increase.
               New_Config.Ops.Append ((Push_Back, Token.ID, New_Config.Current_Shared_Token));
            else
               New_Config.Cost := New_Config.Cost + McKenzie_Param.Push_Back (Token.ID);
               New_Config.Ops.Append ((Push_Back, Token.ID, Token.Min_Terminal_Index));
               New_Config.Current_Shared_Token := Token.Min_Terminal_Index;
            end if;

            if Trace_McKenzie > Detail then
               Base.Put ("push_back " & Image (Token.ID, Trace.Descriptor.all), Super, Shared,
                         Parser_Index, New_Config.all);
            end if;
         end;
      end if;
   end Try_Push_Back;

   procedure Try_Insert_Terminal
     (Super             : not null access Base.Supervisor;
      Shared            : not null access Base.Shared_Lookahead;
      Parser_Index      : in              SAL.Base_Peek_Type;
      Config            : in              Configuration;
      Local_Config_Heap : in out          Config_Heaps.Heap_Type)
   is
      use all type Ada.Containers.Count_Type;

      Table  : Parse_Table renames Shared.Shared_Parser.Table.all;
      EOF_ID : Token_ID renames Super.Trace.Descriptor.EOF_ID;

      Valid_Insert : constant Token_ID_Set (Table.First_Terminal .. Table.Last_Terminal) :=
        (if Shared.Shared_Parser.Language_Constrain_Terminals = null
         then (Table.First_Terminal .. Table.Last_Terminal => True)
         else Shared.Shared_Parser.Language_Constrain_Terminals
           (Super.Trace.all, Super.Label (Parser_Index), Table, Config));

      Cost_Delta : constant Integer :=
        (if Valid_Insert = (Table.First_Terminal .. Table.Last_Terminal => True)
         then 0 else -1);
   begin
      --  Find terminal insertions to try; loop over input actions for the
      --  current state.
      --
      --  If this config does not pass Check, we want to make other changes
      --  to it, eventually finding a config that passes. So we perform any
      --  needed reductions and one shift, so the config is in a consistent
      --  state, and enqueue the result. If there are any conflicts or
      --  semantic check fails encountered, they create other configs to
      --  enqueue.
      declare
         I : Action_List_Iterator := First (Table.States (Config.Stack (1).State));

         Cached_Config : Configuration;
         Cached_Action : Reduce_Action_Rec;
         Cached_Status : Non_Success_Status;
         --  Most of the time, all the reductions in a state are the same. So
         --  we cache the first result. This includes one reduction; if an
         --  associated semantic check failed, this does not include the fixes.
      begin
         loop
            exit when I.Is_Done;

            declare
               ID     : constant Token_ID := I.Symbol;
               Action : Parse_Action_Rec renames I.Action;
            begin
               if ID /= EOF_ID and then --  can't insert eof
                  (ID /= Invalid_Token_ID and then Valid_Insert (ID)) and then -- invalid when Verb = Error
                 (Config.Ops.Length = 0 or else -- don't insert an id we just pushed back.
                    Config.Ops (Config.Ops.Last_Index) /= (Push_Back, ID, Config.Current_Shared_Token))
               then
                  case Action.Verb is
                  when Shift =>
                     declare
                        New_Config : Configuration := Config;
                     begin
                        New_Config.Error_Token.ID := Invalid_Token_ID;
                        New_Config.Check_Status   := (Label => WisiToken.Semantic_Checks.Ok);

                        Do_Shift
                          (Super, Shared, Parser_Index, Local_Config_Heap, New_Config, Action.State, ID, Cost_Delta);
                     end;

                  when Reduce =>
                     if not Equal (Action, Cached_Action) then
                        declare
                           New_Config : Configuration := Config;
                        begin
                           Cached_Status := Do_Reduce_1
                             (Super, Shared, Parser_Index, Local_Config_Heap, New_Config, Action);
                           Cached_Config := New_Config;
                           Cached_Action := Action;

                           if Cached_Status = Continue then
                              Do_Reduce_2 (Super, Shared, Parser_Index, Local_Config_Heap, New_Config, ID, Cost_Delta);
                           end if;
                        end;

                     else
                        if Cached_Status = Continue then
                           declare
                              New_Config : Configuration := Cached_Config;
                           begin
                              Do_Reduce_2 (Super, Shared, Parser_Index, Local_Config_Heap, New_Config, ID, Cost_Delta);
                           end;
                        end if;
                     end if;

                  when Accept_It =>
                     raise Programmer_Error with "found test case for Process_One Accept_It";

                  when Error =>
                     null;
                  end case;
               end if;
            end;
            I.Next;
         end loop;
      end;

      --  It is tempting to use the Goto_List to find nonterms to insert.
      --  But that can easily lead to error states, and it turns out to be
      --  not useful.
   end Try_Insert_Terminal;

   procedure Try_Delete_Input
     (Super             : not null access Base.Supervisor;
      Shared            : not null access Base.Shared_Lookahead;
      Parser_Index      : in              SAL.Base_Peek_Type;
      Config            : in out          Configuration;
      Local_Config_Heap : in out          Config_Heaps.Heap_Type)
   is
      --  Try deleting (= skipping) the current shared input token.
      Trace  : WisiToken.Trace'Class renames Super.Trace.all;
      EOF_ID : Token_ID renames Trace.Descriptor.EOF_ID;

      McKenzie_Param : McKenzie_Param_Type renames Shared.Shared_Parser.Table.McKenzie_Param;

      ID : constant Token_ID := Shared.Token (Config.Current_Shared_Token).ID;
   begin
      if ID /= EOF_ID then
         --  can't delete EOF
         declare
            New_Config : constant Configuration_Access := Local_Config_Heap.Add (Config);
         begin
            New_Config.Cost := New_Config.Cost + McKenzie_Param.Delete (ID);

            if Match_Since_FF (Config.Ops, (Push_Back, ID, Config.Current_Shared_Token))
            then
               --  We are deleting a push_back; cancel the push_back cost, to make
               --  this the same as plain deleting.
               New_Config.Cost := New_Config.Cost - McKenzie_Param.Push_Back (ID);
            end if;

            New_Config.Ops.Append ((Delete, ID, Config.Current_Shared_Token));
            New_Config.Current_Shared_Token := Shared.Get_Token (New_Config.Current_Shared_Token + 1);

            if Trace_McKenzie > Detail then
               Base.Put
                 ("delete " & Image (ID, Trace.Descriptor.all), Super, Shared, Parser_Index, New_Config.all);
            end if;
         end;
      end if;
   end Try_Delete_Input;

   procedure Process_One
     (Super         : not null access Base.Supervisor;
      Shared        : not null access Base.Shared_Lookahead;
      Config_Status : out             Base.Config_Status)
   is
      --  Get one config from Super, check to see if it is a viable
      --  solution. If not, enqueue variations to check.

      use all type Ada.Containers.Count_Type;
      use all type SAL.Base_Peek_Type;
      use all type Semantic_Checks.Check_Status_Label;
      use all type Base.Config_Status;

      Trace : WisiToken.Trace'Class renames Super.Trace.all;
      Table : Parse_Table renames Shared.Shared_Parser.Table.all;

      Parser_Index : SAL.Base_Peek_Type;
      Config       : Configuration;

      Local_Config_Heap : Config_Heaps.Heap_Type;
      --  We collect all the variants to enqueue, then deliver them all at
      --  once to Super, to minimizes task interactions.

      Post_Fast_Forward_Fail : Boolean := False;
   begin
      Super.Get (Parser_Index, Config, Config_Status);

      if Config_Status = All_Done then
         return;
      end if;

      if Trace_McKenzie > Extra then
         Base.Put ("dequeue", Super, Shared, Parser_Index, Config);
         Put_Line (Trace, Super.Label (Parser_Index), "stack: " & Image (Config.Stack, Trace.Descriptor.all));
      end if;

      if Config.Current_Inserted /= No_Inserted then
         --  This Config was enqueued by a previous Language_Fixes.
         pragma Assert (Config.Error_Token.ID = Invalid_Token_ID and Config.Check_Status.Label = Ok);

         case Fast_Forward (Super, Shared, Parser_Index, Config, Post_Fast_Forward_Fail) is
         when Abandon =>
            --  We know Local_Config_Heap is empty; just tell
            --  Super we are done working.
            Super.Put (Parser_Index, Local_Config_Heap);
            return;
         when Continue =>
            --  We don't increase cost for this Fast_Forward, since it is due to a
            --  Language_Fixes.
            null;
         end case;
      end if;

      if Config.Error_Token.ID /= Invalid_Token_ID then
         if Shared.Shared_Parser.Language_Fixes = null then
            --  No fixes available, so abandon Config, but enqueue
            --  Local_Config_Heap.
            Super.Put (Parser_Index, Local_Config_Heap);
            return;
         else
            case Shared.Shared_Parser.Language_Fixes
              (Trace, Shared.Shared_Parser.Lexer, Super.Label (Parser_Index),
               Shared.Shared_Parser.Terminals, Super.Parser_State (Parser_Index).Tree, Local_Config_Heap,
               Config)
            is
            when Continue =>
               if Config.Check_Status.Label = Ok then
                  --  Parse table Error action; try other Config changes.
                  --
                  --  We don't clear Config.Error_Token here, because Try_Insert calls
                  --  Language_Constrain_Terminals, which needs it. We only clear it
                  --  when a parse results in no error (or a different error), or a
                  --  push_back moves the Current_Token.
                  null;

               else
                  --  "ignore check error" is a viable solution, so continue with Config;

                  declare
                     use all type Semantic_Checks.Semantic_Check;
                     use all type Semantic_Checks.Check_Status;
                     New_State : Unknown_State_Index;
                  begin
                     --  finish reduce.
                     Config.Stack.Pop (SAL.Base_Peek_Type (Config.Check_Token_Count));

                     New_State := Goto_For (Table, Config.Stack (1).State, Config.Error_Token.ID);

                     if New_State = Unknown_State then
                        if Config.Stack.Depth = 1 then
                           --  Stack is empty, and we did not get Accept; really bad syntax got
                           --  us here; abandon this config. See ada_mode-recover_bad_char.adb.
                           Super.Put (Parser_Index, Local_Config_Heap);
                           return;
                        else
                           raise Programmer_Error with
                             "process_one found test case for new_state = Unknown; old state " &
                             Image (Config.Stack (1).State) & " nonterm " & Image
                               (Config.Error_Token.ID, Trace.Descriptor.all);
                        end if;
                     end if;

                     Config.Stack.Push ((New_State, Syntax_Trees.Invalid_Node_Index, Config.Error_Token));

                     --  We must clear Check_Status here, so if this config comes back
                     --  here, we don't try to reduce the stack again. We also clear
                     --  Error_Token, so this doesn't look like a parse error.
                     Config.Check_Status := (Label => Ok);

                     Config.Error_Token.ID := Invalid_Token_ID;
                  end;
               end if;

            when Abandon =>
               Super.Put (Parser_Index, Local_Config_Heap);
               return;
            end case;
         end if;
      end if;

      if not Post_Fast_Forward_Fail then
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
      end if;

      if Trace_McKenzie > Detail then
         Base.Put ("continuing", Super, Shared, Parser_Index, Config);
         if Trace_McKenzie > Extra then
            Put_Line (Trace, Super.Label (Parser_Index), "stack: " & Image (Config.Stack, Trace.Descriptor.all));
         end if;
      end if;

      --  Grouping these operations ensures that there are no duplicate
      --  solutions found. We reset the grouping after each fast_forward.
      --
      --  All possible permutations will be explored.

      if (not Post_Fast_Forward_Fail) and
        None_Since_FF (Config.Ops, Delete) and
        None_Since_FF (Config.Ops, Insert) and
        Config.Stack.Depth > 1 -- can't delete the first state
      then
         Try_Push_Back (Super, Shared, Parser_Index, Config, Local_Config_Heap);
      end if;

      if None_Since_FF (Config.Ops, Delete) then
         Try_Insert_Terminal (Super, Shared, Parser_Index, Config, Local_Config_Heap);
      end if;

      if Config.Current_Inserted = No_Inserted then
         Try_Delete_Input (Super, Shared, Parser_Index, Config, Local_Config_Heap);
      end if;

      Super.Put (Parser_Index, Local_Config_Heap);
   end Process_One;

end WisiToken.LR.McKenzie_Recover.Explore;
