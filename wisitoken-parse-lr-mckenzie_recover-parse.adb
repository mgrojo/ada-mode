--  Abstract :
--
--  See spec
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

package body WisiToken.Parse.LR.McKenzie_Recover.Parse is

   procedure Compute_Nonterm
     (Tree                     : in     Syntax_Trees.Tree;
      ID                       : in     Token_ID;
      Stack                    : in     Recover_Stacks.Stack;
      Tokens                   : in out Syntax_Trees.Recover_Token_Array;
      Nonterm                  :    out Syntax_Trees.Virtual_Recover_Token;
      Default_Contains_Virtual : in     Boolean)
   is
      use Syntax_Trees;

      First_Terminal_Set : Boolean := False;
   begin
      Nonterm :=
        (Virtual                   => True,
         ID                        => ID,
         Contains_Virtual_Terminal => (if Tokens'Length = 0 then Default_Contains_Virtual else False),
         others                    => <>);

      for I in Tokens'Range loop
         Tokens (I) := Stack.Peek (Tokens'Last - I + 1).Token;
      end loop;

      for T of Tokens loop
         Nonterm.Contains_Virtual_Terminal := @ or Contains_Virtual_Terminal (T);

         if Nonterm.Byte_Region.First > Byte_Region (T).First then
            Nonterm.Byte_Region.First := Byte_Region (T).First;
         end if;

         if Nonterm.Byte_Region.Last < Byte_Region (T).Last then
            Nonterm.Byte_Region.Last := Byte_Region (T).Last;
         end if;

         if not First_Terminal_Set then
            if Tree.First_Terminal (T) /= Syntax_Trees.Invalid_Node_Access then
               First_Terminal_Set     := True;
               Nonterm.First_Terminal := Tree.First_Terminal (T);
            end if;
         end if;
      end loop;
   end Compute_Nonterm;

   function Reduce_Stack
     (Super                    : not null access Base.Supervisor;
      Shared                   : not null access Base.Shared;
      Stack                    : in out          Recover_Stacks.Stack;
      Action                   : in              Reduce_Action_Rec;
      Nonterm                  :    out          Syntax_Trees.Recover_Token;
      Default_Contains_Virtual : in              Boolean)
     return Semantic_Checks.Check_Status
   is
      use all type Semantic_Checks.Semantic_Check;
      use all type Semantic_Checks.Check_Status_Label;

      Last   : constant SAL.Base_Peek_Type := SAL.Base_Peek_Type (Action.Token_Count);
      Tokens : Syntax_Trees.Recover_Token_Array (1 .. Last);
   begin
      if Stack.Depth <= Last then
         raise Bad_Config;
      end if;

      Compute_Nonterm (Super.Tree.all, Action.Production.LHS, Stack, Tokens, Nonterm, Default_Contains_Virtual);

      if Action.Check = null then
         --  Now we can pop the stack.
         Stack.Pop (SAL.Base_Peek_Type (Action.Token_Count));
         return (Label => Ok);
      else
         return Status : constant Semantic_Checks.Check_Status :=
           Action.Check (Shared.Lexer, Nonterm, Tokens, Recover_Active => True)
         do
            if Status.Label = Ok then
               Stack.Pop (SAL.Base_Peek_Type (Action.Token_Count));
            end if;
         end return;
      end if;
   end Reduce_Stack;

   procedure Breakdown
     (Tree   : in     Syntax_Trees.Tree;
      Stream : in out Bounded_Streams.List)
   is
      use Bounded_Streams;
      use Syntax_Trees;

      --  Derived from Syntax_Trees.Left_Breakdown. We do not delete virtual
      --  terminals, to allow insert before, delete.

      Cur       : Cursor            := Stream.First;
      Node      : Valid_Node_Access := Stream (Cur);
      Next_Node : Node_Access;
   begin
      loop
         Next_Node := Invalid_Node_Access;

         for I in reverse 2 .. Tree.Child_Count (Node) loop
            declare
               Child : constant Valid_Node_Access := Tree.Child (Node, I);
            begin
               if Tree.Child_Count (Child) > 0 then
                  Next_Node := Tree.Child (Child, I);
               end if;

               Cur := Stream.Insert (Element => Child, Before => Cur);

               --  We don't do Tree.Clear_Parent (Child) here, because we are not
               --  editing the syntax tree. If this config succeeds,
               --  Tree.Left_Breakdown will be called.
            end;
         end loop;

         declare
            Child : constant Valid_Node_Access := Tree.Child (Node, 1);
         begin
            if Tree.Child_Count (Child) > 0 then
               Node := Child;

            elsif Tree.Label (Child) in Terminal_Label then
               Node := Child;

               Stream.Insert (Element => Node, Before => Cur);

               declare
                  To_Delete : Cursor := Cur;
               begin
                  Stream.Delete (To_Delete);
               end;
               exit;
            else
               --  Node is an empty nonterm. Note that Next_Node cannot be null; the
               --  precondition asserts that Input_Stream.First was not empty.
               Node := Next_Node;
            end if;
         end;
      end loop;
   end Breakdown;

   function Delete_Current_Applies
     (Tree   : in Syntax_Trees.Tree;
      Config : in Configuration)
     return Boolean
   is
      use Config_Op_Arrays;
      use Config_Op_Array_Refs;
      use Syntax_Trees;
   begin
      if Config.Current_Shared_Token = Invalid_Stream_Node_Ref then
         --  Happens with really bad syntax; see test_mckenzie_recover.adb Error_4.
         return False;
      end if;

      declare
         Next_Node : constant Valid_Node_Access := Peek_Current_First_Shared_Terminal (Tree, Config);
      begin
         return Config.Current_Insert_Delete /= No_Insert_Delete and then
           Token_Index (Constant_Ref (Config.Insert_Delete, Config.Current_Insert_Delete)) =
           Get_Node_Index (Next_Node);
      end;
   end Delete_Current_Applies;

   function Peek_Current_Token_ID
     (Tree   : in Syntax_Trees.Tree;
      Config : in Configuration)
     return Token_ID
   is
      use Config_Op_Arrays;
      use Config_Op_Array_Refs;
   begin
      if Config.Current_Insert_Delete /= No_Insert_Delete and then
        (declare
            Next_Shared_Node : constant Syntax_Trees.Valid_Node_Access :=
              Peek_Current_First_Shared_Terminal (Tree, Config);
         begin
            Token_Index (Constant_Ref (Config.Insert_Delete, Config.Current_Insert_Delete)) =
              Tree.Get_Node_Index (Next_Shared_Node))
      then
         declare
            Op : Insert_Delete_Op renames Constant_Ref (Config.Insert_Delete, Config.Current_Insert_Delete);
         begin
            case Insert_Delete_Op_Label (Op.Op) is
            when Insert =>
               return ID (Op);

            when Delete =>
               raise SAL.Programmer_Error; --  Precondition ensures this cannot happen.
            end case;
         end;
      else
         return Tree.ID (Peek_Current_Element_Node (Tree, Config));
      end if;
   end Peek_Current_Token_ID;

   procedure Current_Token_ID_Peek_3
     (Tree         : in     Syntax_Trees.Tree;
      Config       : in     Configuration;
      Tokens       :    out Token_ID_Array_1_3)
   --  Return the current token from Config in Tokens (1). Return the two
   --  following tokens in Tokens (2 .. 3).
   is
      use Config_Op_Arrays;
      use Config_Op_Array_Refs;
      use Syntax_Trees;

      --  We can't use Parse.Get_Current_Token, Parse.Next_Token, because we
      --  are not allowed to modify Config. In particular, we cannot apply
      --  Breakdown to Config.Input_Stream on the second and third tokens,
      --  which is required if Delete applies to those tokens.
      --
      --  We can extract the first three IDs without modifying anything.
      --
      --  Fast_Forward applies the Delete to the current token, so we should
      --  not have to here, but rather than rely on that and special case it
      --  here, we handle all three in the same way.

      Tokens_Last                  : Integer            := 0;
      Current_Insert_Delete        : SAL.Base_Peek_Type := Config.Current_Insert_Delete;
      Current_Input_Stream_Element : Bounded_Streams.Cursor;
      Current_Input_Stream_Node    : Node_Access;
      Input_Stream_Parents         : Syntax_Trees.Node_Stacks.Stack;
      Current_Shared_Token         : Terminal_Ref;
      Inc_Shared_Token             : Boolean            := True;
   begin
      Peek_Shared_Start
        (Tree, Config, Current_Input_Stream_Element, Current_Input_Stream_Node, Input_Stream_Parents,
         Current_Shared_Token);

      loop -- three tokens, Op = Delete
         declare
            Next_Node : constant Valid_Node_Access := Peek_Shared_Terminal
              (Current_Input_Stream_Node, Current_Shared_Token);
         begin
            if Current_Insert_Delete /= No_Insert_Delete and then
              Token_Index (Constant_Ref (Config.Insert_Delete, Current_Insert_Delete)) =
              Tree.Get_Node_Index (Next_Node)
            then
               Inc_Shared_Token     := False;
               declare
                  Op : Insert_Delete_Op renames Constant_Ref (Config.Insert_Delete, Current_Insert_Delete);
               begin
                  case Insert_Delete_Op_Label (Op.Op) is
                  when Insert =>
                     Tokens_Last          := @ + 1;
                     Tokens (Tokens_Last) := ID (Op);

                  when Delete =>
                     Peek_Next_Shared_Terminal
                       (Tree, Config, Current_Input_Stream_Element, Current_Input_Stream_Node, Input_Stream_Parents,
                        Current_Shared_Token);
                  end case;

                  Current_Insert_Delete := @ + 1;

                  if Current_Insert_Delete > Last_Index (Config.Insert_Delete) then
                     Current_Insert_Delete := No_Insert_Delete;
                  end if;
               end;
            else
               Inc_Shared_Token     := True;
               Tokens_Last          := @ + 1;
               Tokens (Tokens_Last) := Tree.ID (Next_Node);
            end if;
         end;

         exit when Tokens (Tokens_Last) = Tree.Descriptor.EOI_ID or Tokens_Last = 3;

         if Inc_Shared_Token then
            Peek_Next_Shared_Terminal
              (Tree, Config, Current_Input_Stream_Element, Current_Input_Stream_Node, Input_Stream_Parents,
               Current_Shared_Token);
         end if;
      end loop;

      for I in Tokens_Last + 1 .. 3 loop
         Tokens (I) := Invalid_Token_ID;
      end loop;
   end Current_Token_ID_Peek_3;

   function Peek_Current_Element_Node
     (Tree   : in Syntax_Trees.Tree;
      Config : in Configuration)
     return Syntax_Trees.Valid_Node_Access
   is
      use all type Bounded_Streams.Cursor;
   begin
      return
        (if Config.Input_Stream.First = Bounded_Streams.No_Element
         then Tree.Get_Node (Tree.Shared_Stream, Config.Current_Shared_Token.Element)
         else Config.Input_Stream (Config.Input_Stream.First));
   end Peek_Current_Element_Node;

   function Peek_Current_First_Terminal
     (Tree   : in Syntax_Trees.Tree;
      Config : in Configuration)
     return Syntax_Trees.Valid_Node_Access
   is
      use Bounded_Streams;
      use Syntax_Trees;
   begin
      if Config.Input_Stream.First = No_Element then
         return Tree.First_Terminal (Tree.Shared_Stream, Config.Current_Shared_Token.Element).Node;

      else
         declare
            Result : constant Node_Access := First_Terminal (Tree, Config.Input_Stream);
         begin
            if Result /= Invalid_Node_Access then
               return Result;
            else
               return Tree.First_Terminal (Tree.Shared_Stream, Config.Current_Shared_Token.Element).Node;
            end if;
         end;
      end if;
   end Peek_Current_First_Terminal;

   function Peek_Current_First_Shared_Terminal
     (Tree   : in Syntax_Trees.Tree;
      Config : in Configuration)
     return Syntax_Trees.Valid_Node_Access
   is
      use Bounded_Streams;
      use Syntax_Trees;
      Parents : Node_Stacks.Stack;
   begin
      if Config.Input_Stream.First = No_Element then
         return Tree.First_Shared_Terminal (Tree.Shared_Stream, Config.Current_Shared_Token.Element, Parents).Node;

      else
         declare
            Result : constant Node_Access := First_Shared_Terminal (Tree, Config.Input_Stream, Parents);
         begin
            if Result /= Invalid_Node_Access then
               return Result;
            else
               return Tree.First_Shared_Terminal
                 (Tree.Shared_Stream, Config.Current_Shared_Token.Element, Parents).Node;
            end if;
         end;
      end if;
   end Peek_Current_First_Shared_Terminal;

   procedure Peek_Shared_Start
     (Tree                         : in     Syntax_Trees.Tree;
      Config                       : in     Configuration;
      Current_Input_Stream_Element :    out Bounded_Streams.Cursor;
      Current_Input_Stream_Node    :    out Syntax_Trees.Node_Access;
      Input_Stream_Parents         : in out Syntax_Trees.Node_Stacks.Stack;
      Current_Shared_Token         :    out Syntax_Trees.Terminal_Ref)
   is begin
      Current_Input_Stream_Element := Config.Input_Stream.First;
      Current_Input_Stream_Node    := First_Shared_Terminal (Tree, Config.Input_Stream, Input_Stream_Parents);
      Current_Shared_Token         := Config.Current_Shared_Token;
   end Peek_Shared_Start;

   function Peek_Shared_Terminal
     (Current_Input_Stream_Node : in Syntax_Trees.Node_Access;
      Current_Shared_Token      : in Syntax_Trees.Terminal_Ref)
     return Syntax_Trees.Node_Access
   is begin
      if Current_Input_Stream_Node = Syntax_Trees.Invalid_Node_Access then
         return Current_Shared_Token.Node;

      else
         return Current_Input_Stream_Node;
      end if;
   end Peek_Shared_Terminal;

   procedure Peek_Next_Shared_Terminal
     (Tree                         : in     Syntax_Trees.Tree;
      Config                       : in     Configuration;
      Current_Input_Stream_Element : in out Bounded_Streams.Cursor;
      Current_Input_Stream_Node    : in out Syntax_Trees.Node_Access;
      Input_Stream_Parents         : in out Syntax_Trees.Node_Stacks.Stack;
      Current_Shared_Token         : in out Syntax_Trees.Terminal_Ref)
   is
      use Syntax_Trees;
   begin
      if Current_Input_Stream_Node = Invalid_Node_Access then
         Tree.Next_Shared_Terminal (Current_Shared_Token);

      else
         Next_Shared_Terminal
           (Tree, Config.Input_Stream, Current_Input_Stream_Element, Current_Input_Stream_Node, Input_Stream_Parents);
      end if;
   end Peek_Next_Shared_Terminal;

   function First_Terminal
     (Tree   : in Syntax_Trees.Tree;
      Stream : in Bounded_Streams.List)
     return Syntax_Trees.Node_Access
   is
      use Bounded_Streams;
      use Syntax_Trees;
      Cur  : Cursor      := Stream.First;
      Node : Node_Access := Invalid_Node_Access;
   begin
      loop
         exit when not Has_Element (Cur);

         Node := Tree.First_Terminal (Stream (Cur));
         exit when Node /= Invalid_Node_Access;

         Stream.Next (Cur);
      end loop;
      return Node;
   end First_Terminal;

   function First_Shared_Terminal
     (Tree           : in     Syntax_Trees.Tree;
      Stream         : in     Bounded_Streams.List;
      Stream_Parents : in out Syntax_Trees.Node_Stacks.Stack)
     return Syntax_Trees.Node_Access
   is
      use Bounded_Streams;
      use Syntax_Trees;
      Cur  : Cursor      := Stream.First;
      Node : Node_Access := Invalid_Node_Access;
   begin
      Outer :
      loop
         exit Outer when not Has_Element (Cur);

         Node := Tree.First_Terminal (Stream (Cur), Stream_Parents);

         Inner :
         loop
            exit Inner when Node = Invalid_Node_Access;
            exit Outer when Tree.Get_Node_Index (Node) > 0;

            Node := Tree.Next_Terminal (Stream_Parents, Node);
         end loop Inner;

         Stream.Next (Cur);
      end loop Outer;
      return Node;
   end First_Shared_Terminal;

   procedure Next_Shared_Terminal
     (Tree         : in     Syntax_Trees.Tree;
      Stream       : in     Bounded_Streams.List;
      Element_Node : in out Bounded_Streams.Cursor;
      Node         : in out Syntax_Trees.Node_Access;
      Parents      : in out Syntax_Trees.Node_Stacks.Stack)
   is
      use Bounded_Streams;
   begin
      if Tree.Has_Parent (Node) then
         Node := Tree.Next_Shared_Terminal (Node);
      else
         Node := Tree.Next_Shared_Terminal (Parents, Node);
      end if;

      loop
         exit when Node /= Syntax_Trees.Invalid_Node_Access;
         Element_Node := Next (Stream, Element_Node);
         if Element_Node = No_Element then
            Node := Syntax_Trees.Invalid_Node_Access;
            exit;
         end if;
         Node := Tree.First_Shared_Terminal (Element (Stream, Element_Node), Parents);
      end loop;
   end Next_Shared_Terminal;

   procedure Prev_Shared_Terminal
     (Tree         : in     Syntax_Trees.Tree;
      Stream       : in     Bounded_Streams.List;
      Element_Node : in out Bounded_Streams.Cursor;
      Node         : in out Syntax_Trees.Node_Access;
      Parents      : in out Syntax_Trees.Node_Stacks.Stack)
   is
      use Bounded_Streams;
   begin
      if Tree.Has_Parent (Node) then
         Node := Tree.Prev_Shared_Terminal (Node);
      else
         Node := Tree.Prev_Shared_Terminal (Parents, Node);
      end if;

      loop
         exit when Node /= Syntax_Trees.Invalid_Node_Access;
         Element_Node := Previous (Stream, Element_Node);
         if Element_Node = No_Element then
            Node := Syntax_Trees.Invalid_Node_Access;
            exit;
         end if;
         Node := Tree.Last_Shared_Terminal (Element (Stream, Element_Node), Parents);
      end loop;
   end Prev_Shared_Terminal;

   procedure Do_Delete
     (Tree   : in     Syntax_Trees.Tree;
      Config : in out Configuration)
   is
      use all type Bounded_Streams.Cursor;
   begin
      if Config.Input_Stream.First = Bounded_Streams.No_Element then
         Tree.Next_Shared_Terminal (Config.Current_Shared_Token);

      else
         loop
            declare
               use Syntax_Trees;

               Next_Node : constant Valid_Node_Access := Config.Input_Stream (Config.Input_Stream.First);
            begin
               exit when Tree.Label (Next_Node) in Terminal_Label;

               if Tree.First_Terminal (Next_Node) = Invalid_Node_Access then
                  --  Next_Node = Input_Stream.First is an empty nonterm.
                  Config.Input_Stream.Delete_First;
               else
                  Breakdown (Tree, Config.Input_Stream);
               end if;
            end;
         end loop;

         Config.Input_Stream.Delete_First;
      end if;
   end Do_Delete;

   function Get_Current_Token
     (Tree                    : in     Syntax_Trees.Tree;
      Config                  : in out Configuration;
      Inc_Shared_Stream_Token :    out Boolean;
      Inc_Input_Stream_Token  :    out Boolean)
     return Syntax_Trees.Recover_Token
   --  Return the current token from Config. If a Delete op applies,
   --  Config is updated to reflect the delete. Otherwise Config is not
   --  changed; calling Get_Current_Token again on (a copy of) Config
   --  will return the same token as this call.
   --
   --  Use Peek_Current_Token_ID if Config may not change at all.
   --
   --  Inc_*_Token are for Next_Token.
   --
   --  No precondition; raises Bad_Config for invalid situations.
   is
      use Config_Op_Arrays;
      use Config_Op_Array_Refs;
      use Syntax_Trees;
      use all type Bounded_Streams.Cursor;
   begin
      if Config.Current_Shared_Token = Syntax_Trees.Invalid_Stream_Node_Ref then
         --  Happens with really bad syntax; see test_mckenzie_recover.adb Error_4.
         raise Bad_Config;
      end if;

      loop -- Op = Delete requires loop
         if Config.Current_Insert_Delete /= No_Insert_Delete and then
           (declare
               Current_Shared_Node : constant Syntax_Trees.Valid_Node_Access := Peek_Current_First_Shared_Terminal
                 (Tree, Config);
            begin
               Token_Index (Constant_Ref (Config.Insert_Delete, Config.Current_Insert_Delete)) =
                 Tree.Get_Node_Index (Current_Shared_Node))
         then
            declare
               Op : Insert_Delete_Op renames Constant_Ref (Config.Insert_Delete, Config.Current_Insert_Delete);
            begin
               case Insert_Delete_Op_Label (Op.Op) is
               when Insert =>
                  Inc_Shared_Stream_Token := False;
                  Inc_Input_Stream_Token  := False;
                  return (Virtual                   => True,
                          ID                        => ID (Op),
                          Contains_Virtual_Terminal => True,
                          others                    => <>);

               when Delete =>
                  pragma Assert (Is_Terminal (Op.Del_ID, Tree.Descriptor.all), "IMPROVEME: allow delete nonterm");

                  Do_Delete (Tree, Config);

                  Config.Current_Insert_Delete := @ + 1;

                  if Config.Current_Insert_Delete > Last_Index (Config.Insert_Delete) then
                     Config.Current_Insert_Delete := No_Insert_Delete;
                     Clear (Config.Insert_Delete);
                  end if;
               end case;
            end;

         elsif Config.Input_Stream.First /= Bounded_Streams.No_Element then
            Inc_Shared_Stream_Token := False;
            Inc_Input_Stream_Token  := True;
            return Tree.Get_Recover_Token (Config.Input_Stream (Config.Input_Stream.First));

         else
            Inc_Shared_Stream_Token := True;
            return Tree.Get_Recover_Token (Config.Current_Shared_Token);
         end if;
      end loop;
   end Get_Current_Token;

   procedure Next_Token
     (Tree                    : in     Syntax_Trees.Tree;
      Config                  : in out Configuration;
      Inc_Shared_Stream_Token : in     Boolean;
      Inc_Input_Stream_Token  : in     Boolean)
   --  Increment the appropriate "current token" index in Config.
   --  Inc_*_Token are from Get_Current_Token.
   is
      use Config_Op_Arrays, Config_Op_Array_Refs;
      use all type Bounded_Streams.Cursor;
   begin
      if Last_Index (Config.Insert_Delete) > 0 and then
        Config.Current_Insert_Delete = Last_Index (Config.Insert_Delete)
      then
         Config.Current_Insert_Delete := No_Insert_Delete;
         Clear (Config.Insert_Delete);
      else
         declare
            Parents : Syntax_Trees.Node_Stacks.Stack;
            Next_Node : constant Syntax_Trees.Valid_Node_Access :=
              (if Config.Input_Stream.First /= Bounded_Streams.No_Element
               then First_Shared_Terminal (Tree, Config.Input_Stream, Parents)
               else Config.Current_Shared_Token.Node);
         begin
            if Config.Current_Insert_Delete /= No_Insert_Delete and then
              Token_Index (Constant_Ref (Config.Insert_Delete, Config.Current_Insert_Delete + 1)) =
              Tree.Get_Node_Index (Next_Node)
            then
               Config.Current_Insert_Delete := @ + 1;

            else
               if Config.Input_Stream.First = Bounded_Streams.No_Element then
                  if Inc_Shared_Stream_Token then
                     Tree.Stream_Next (Config.Current_Shared_Token);
                  end if;
               else
                  if Inc_Input_Stream_Token then
                     Config.Input_Stream.Delete_First;
                  end if;
               end if;
            end if;
         end;
      end if;
   end Next_Token;

   function Parse_One_Item
     (Super             :         not null access Base.Supervisor;
      Shared            :         not null access Base.Shared;
      Parser_Index      :         in              SAL.Peek_Type;
      Parse_Items       : aliased in out          Parse_Item_Arrays.Vector;
      Parse_Item_Index  :         in              Positive;
      Shared_Token_Goal :         in              Syntax_Trees.Node_Index;
      Trace_Prefix      :         in              String)
     return Boolean
   --  Perform parse actions on Parse_Items (Parse_Item_Index), until it
   --  encounters an error (return False) or Shared_Token_Goal is shifted
   --  (return True).
   --
   --  We return Boolean, not Check_Status, because Abandon and Continue
   --  are up to the caller.
   --
   --  If any actions have conflicts, append the conflict configs and actions to
   --  Parse_Items.

   is
      use Parse_Item_Arrays;
      use Config_Op_Arrays;
      use all type Semantic_Checks.Check_Status_Label;

      Trace      : WisiToken.Trace'Class renames Super.Trace.all;
      Descriptor : WisiToken.Descriptor renames Super.Tree.Descriptor.all;
      Table      : Parse_Table renames Shared.Table.all;

      Item       : Parse_Item renames Parse_Item_Array_Refs.Variable_Ref
        (Parse_Items, Parse_Item_Index).Element.all;
      Config     : Configuration renames Item.Config;
      Action_Cur : Parse_Action_Node_Ptr renames Item.Action;
      Action     : Parse_Action_Rec;

      Inc_Shared_Stream_Token : Boolean;
      Inc_Input_Stream_Token  : Boolean;
      Current_Token           : Syntax_Trees.Recover_Token := Get_Current_Token
        (Super.Tree.all, Config, Inc_Shared_Stream_Token, Inc_Input_Stream_Token);

      New_State : Unknown_State_Index;
      Success   : Boolean := True;

      procedure Get_Action
      is
         --  We use the incremental parse algorithm even if the main parse is
         --  batch, because Push_Back places whole nonterms on
         --  Config.Input_Stream.
         --
         --  Same logic as in Parser.Parse.Get_Action, but this
         --  operates on Config.

         Current_State : constant State_Index := Config.Stack.Peek.State;

         First_In_Current : Syntax_Trees.Node_Access;
      begin
         loop --  Skip empty nonterms, handle Breakdown
            if Is_Terminal (Syntax_Trees.ID (Current_Token), Descriptor) then
               Action_Cur := Action_For (Table, Current_State, Syntax_Trees.ID (Current_Token));
               Action     := Action_Cur.Item;
               return;
            else
               --  nonterminal.
               declare
                  New_State : constant Unknown_State_Index := Goto_For
                    (Table, Current_State, Syntax_Trees.ID (Current_Token));
               begin
                  if New_State /= Unknown_State then
                     Action_Cur := null;
                     Action     :=
                       (Verb       => Shift,
                        Production => Invalid_Production_ID,
                        State      => New_State);
                     return;
                  else
                     First_In_Current := Super.Tree.First_Terminal (Current_Token);

                     if First_In_Current = Syntax_Trees.Invalid_Node_Access then
                        --  Current_Token is an empty nonterm; skip it.
                        Next_Token (Super.Tree.all, Config, Inc_Shared_Stream_Token, Inc_Input_Stream_Token);

                        Current_Token := Get_Current_Token
                          (Super.Tree.all, Config, Inc_Shared_Stream_Token, Inc_Input_Stream_Token);
                     else
                        pragma Assert
                          (Config.Input_Stream.Length > 0,
                           "FIXME: mckenzie_recover.parse handle nonterm in Shared_Stream");

                        --  FIXME: don't need breakdown if action on first_terminal is reduce;
                        --  see wisitoken-parse-lr-parser.adb Get_Action
                        Breakdown (Super.Tree.all, Config.Input_Stream);

                        Current_Token := Get_Current_Token
                          (Super.Tree.all, Config, Inc_Shared_Stream_Token, Inc_Input_Stream_Token);
                     end if;
                  end if;
               end;
            end if;
         end loop;
      end Get_Action;

   begin
      if Trace_McKenzie > Detail then
         if Trace_McKenzie > Extra then
            Put_Line (Trace, Trace_Prefix & ": stack: " & LR.Image (Config.Stack, Super.Tree.all));
            if Config.Current_Insert_Delete /= No_Insert_Delete then
               Put_Line (Trace, Super.Tree.all, Super.Stream (Parser_Index), Trace_Prefix & ": Insert_Delete: " &
                           Image (Config.Insert_Delete, Descriptor));
            end if;
         end if;

         if Shared_Token_Goal /= Syntax_Trees.Invalid_Node_Index then
            Put_Line (Trace, Super.Tree.all, Super.Stream (Parser_Index), Trace_Prefix & ": Shared_Token_Goal :" &
                        Shared_Token_Goal'Image);
         end if;
      end if;

      Item.Parsed := True;

      if Action_Cur = null then
         --  Item is original Config; else Item is from a conflict
         Get_Action;
      else
         Action := Action_Cur.Item;
      end if;

      loop
         declare
            Conflict : constant Parse_Action_Node_Ptr := (if Action_Cur = null then null else Action_Cur.Next);
         begin
            --  We don't loop on Conflict here; if Conflict.Next is non null, it
            --  will be enqueued when Conflict is parsed.
            if Conflict /= null then
               if Is_Full (Parse_Items) then
                  if Trace_McKenzie > Outline then
                     Put_Line (Trace, Super.Tree.all, Super.Stream (Parser_Index),
                               Trace_Prefix & ": too many conflicts; abandoning");
                     raise Bad_Config;
                  end if;
               else
                  if Trace_McKenzie > Detail then
                     Put_Line
                       (Trace, Super.Tree.all, Super.Stream (Parser_Index), Trace_Prefix & ":" & State_Index'Image
                          (Config.Stack.Peek.State) & ": add conflict " &
                          Image (Conflict.Item, Descriptor));
                  end if;

                  Append (Parse_Items, (Config, Conflict, Parsed => False, Shift_Count => Item.Shift_Count));
               end if;
            end if;
         end;

         if Trace_McKenzie > Extra then
            Put_Line
              (Trace, Super.Tree.all, Super.Stream (Parser_Index), Trace_Prefix & ":" &
                 Config.Stack.Peek.State'Image &
                 ":" & Syntax_Trees.Image (Super.Tree.all, Current_Token) &
                 " : " & Image (Action, Descriptor) &
                 (if Action.Verb = Reduce
                  then " via" & Config.Stack.Peek (SAL.Peek_Type (Action.Token_Count + 1)).State'Image
                  else ""));
         end if;

         case Action.Verb is
         when Shift =>
            Item.Shift_Count := Item.Shift_Count + 1;

            Config.Stack.Push ((Action.State, Current_Token));

            Next_Token (Super.Tree.all, Config, Inc_Shared_Stream_Token, Inc_Input_Stream_Token);
            Current_Token := Get_Current_Token
              (Super.Tree.all, Config, Inc_Shared_Stream_Token, Inc_Input_Stream_Token);

         when Reduce =>
            declare
               Nonterm : Syntax_Trees.Recover_Token;
            begin
               Config.Check_Status := Reduce_Stack
                 (Super, Shared, Config.Stack, Action, Nonterm,
                  Default_Contains_Virtual => Config.Current_Insert_Delete /= No_Insert_Delete);

               case Config.Check_Status.Label is
               when Ok =>
                  New_State := Config.Stack.Peek.State;
                  New_State := Goto_For (Table, New_State, Action.Production.LHS);

                  if New_State = Unknown_State then
                     --  Most likely from an inappropriate language fix.
                     if Trace_McKenzie > Outline then
                        Base.Put (Trace_Prefix & ": Unknown_State: ", Super, Parser_Index, Config);
                        Put_Line
                          (Trace, Trace_Prefix & ": stack: " & LR.Image (Config.Stack, Super.Tree.all));
                     end if;

                     --  We can't just return False here; user must abandon this config.
                     raise Bad_Config;
                  end if;

                  Config.Stack.Push ((New_State, Nonterm));

               when Semantic_Checks.Error =>
                  Config.Error_Token       := Nonterm;
                  Config.Check_Token_Count := Action.Token_Count;
                  Success                  := False;
               end case;
            end;

         when Error =>

            Config.Error_Token :=
              (ID          => Syntax_Trees.ID (Current_Token),
               Byte_Region => Syntax_Trees.Byte_Region (Current_Token),
               others      => <>);
            Success            := False;

         when Accept_It =>
            null;
         end case;

         exit when not Success or
           Action.Verb = Accept_It or
           (if Shared_Token_Goal = Syntax_Trees.Invalid_Node_Index
            then Length (Config.Insert_Delete) = 0
            else Super.Tree.Get_Node_Index (Peek_Current_First_Shared_Terminal (Super.Tree.all, Config)) >
              Shared_Token_Goal);

         Get_Action;
      end loop;

      return Success;
   end Parse_One_Item;

   function Parse
     (Super             :         not null access Base.Supervisor;
      Shared            :         not null access Base.Shared;
      Parser_Index      :         in              SAL.Peek_Type;
      Parse_Items       : aliased    out          Parse_Item_Arrays.Vector;
      Config            :         in              Configuration;
      Shared_Token_Goal :         in              Syntax_Trees.Node_Index;
      All_Conflicts     :         in              Boolean;
      Trace_Prefix      :         in              String)
     return Boolean
   is
      use Parse_Item_Arrays;
      Trace : WisiToken.Trace'Class renames Super.Trace.all;

      Last_Parsed : Natural;
      Success     : Boolean;
   begin
      Clear (Parse_Items);
      Append (Parse_Items, (Config, Action => null, Parsed => False, Shift_Count => 0));

      --  Clear any errors; so they reflect the parse result.
      declare
         Config : Configuration renames Parse_Item_Array_Refs.Variable_Ref
           (Parse_Items, First_Index (Parse_Items)).Config;
      begin
         Config.Error_Token  := Syntax_Trees.Invalid_Recover_Token;
         Config.Check_Status := (Label => Semantic_Checks.Ok);
      end;

      Last_Parsed := First_Index (Parse_Items);
      loop
         --  Loop over initial config and any conflicts.
         Success := Parse_One_Item
           (Super, Shared, Parser_Index, Parse_Items, Last_Parsed, Shared_Token_Goal, Trace_Prefix);

         exit when Last_Index (Parse_Items) = Last_Parsed;

         exit when Success and not All_Conflicts;

         Last_Parsed := Last_Parsed + 1;
         if Trace_McKenzie > Detail then
            Put_Line (Trace, Super.Tree.all, Super.Stream (Parser_Index), Trace_Prefix & ": parse conflict");
         end if;
      end loop;

      return Success;
   end Parse;

end WisiToken.Parse.LR.McKenzie_Recover.Parse;
