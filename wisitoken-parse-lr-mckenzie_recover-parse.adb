--  Abstract :
--
--  See spec
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
         Nonterm.Contains_Virtual_Terminal := @ or Tree.Contains_Virtual_Terminal (T);

         if Nonterm.Byte_Region.First > Tree.Byte_Region (T).First then
            Nonterm.Byte_Region.First := Tree.Byte_Region (T).First;
         end if;

         if Nonterm.Byte_Region.Last < Tree.Byte_Region (T).Last then
            Nonterm.Byte_Region.Last := Tree.Byte_Region (T).Last;
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
      Stack                    : in out          Recover_Stacks.Stack;
      Action                   : in              Reduce_Action_Rec;
      Nonterm                  :    out          Syntax_Trees.Recover_Token;
      Default_Contains_Virtual : in              Boolean)
     return In_Parse_Actions.Status
   is
      use all type In_Parse_Actions.In_Parse_Action;
      use all type In_Parse_Actions.Status_Label;

      Last   : constant SAL.Base_Peek_Type := SAL.Base_Peek_Type (Action.Token_Count);
      Tokens : Syntax_Trees.Recover_Token_Array (1 .. Last);
   begin
      if Stack.Depth <= Last then
         raise Bad_Config;
      end if;

      Compute_Nonterm (Super.Tree.all, Action.Production.LHS, Stack, Tokens, Nonterm, Default_Contains_Virtual);

      if Action.In_Parse_Action = null then
         --  Now we can pop the stack.
         Stack.Pop (SAL.Base_Peek_Type (Action.Token_Count));
         return (Label => Ok);
      else
         return Status : constant In_Parse_Actions.Status :=
           Action.In_Parse_Action (Super.Tree.all, Nonterm, Tokens, Recover_Active => True)
         do
            if Status.Label = Ok then
               Stack.Pop (SAL.Base_Peek_Type (Action.Token_Count));
            end if;
         end return;
      end if;
   end Reduce_Stack;

   procedure Left_Breakdown
     (Tree   : in     Syntax_Trees.Tree;
      Stream : in out Bounded_Streams.List)
   is
      use Bounded_Streams;
      use Syntax_Trees;

      --  Derived from Syntax_Trees.Left_Breakdown. We do not delete virtual
      --  terminals, to allow insert before, delete.

      Cur       : Cursor            := Stream.First;
      To_Delete : Cursor            := Cur;
      Node      : Valid_Node_Access := Stream (Cur);
      Next_I    : Positive_Index_Type;
   begin
      loop
         Next_I := Positive_Index_Type'Last;

         for I in reverse 2 .. Tree.Child_Count (Node) loop
            declare
               Child : constant Valid_Node_Access := Tree.Child (Node, I);
            begin
               if Tree.Child_Count (Child) > 0 or Tree.Label (Child) in Terminal_Label then
                  Next_I := I;
               end if;

               Cur := Stream.Insert (Element => Child, Before => Cur);

               --  We don't do Tree.Clear_Parent (Child) here, because we are not
               --  editing the syntax tree. If this config succeeds,
               --  Tree.Left_Breakdown will be called.
            end;
         end loop;

         declare
            Child_1 : constant Valid_Node_Access := Tree.Child (Node, 1);
         begin
            if Tree.Child_Count (Child_1) > 0 or Tree.Label (Child_1) in Terminal_Label then
               Next_I := 1;
            else
               --  Node is an empty nonterm. First non_empty is in Node.Children (Next_I);
               if Next_I > 2 then
                  --  Delete other empty nonterms that were added to the stream.
                  for I in 2 .. Next_I - 1 loop
                     declare
                        To_Delete : Cursor := Cur;
                     begin
                        Stream.Next (Cur);
                        Stream.Delete (To_Delete);
                     end;
                  end loop;
               end if;
               pragma Assert (Stream.Element (Cur) = Tree.Child (Node, Next_I));
            end if;
         end;

         Node := Tree.Child (Node, Next_I);

         if Tree.Label (Node) in Terminal_Label then
            if Next_I = 1 then
               Stream.Insert (Element => Node, Before => Cur);
            else
               --  already inserted above
               null;
            end if;

            Stream.Delete (To_Delete);
            exit;
         end if;
      end loop;
   end Left_Breakdown;

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
         Next_Node : constant Node_Access := Peek_Current_First_Shared_Terminal
           (Tree, Config, Following_Element => False);
      begin
         return Next_Node /= Invalid_Node_Access and then
           Config.Current_Insert_Delete /= No_Insert_Delete and then
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
     (Tree   :         in     Syntax_Trees.Tree;
      Config : aliased in     Configuration;
      Tokens :            out Token_ID_Array_1_3)
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

      Tokens_Last           : Integer            := 0;
      Current_Insert_Delete : SAL.Base_Peek_Type := Config.Current_Insert_Delete;
      Peek_State            : Peek_Shared_State  := Peek_Shared_Start (Tree, Config);
      Inc_Shared_Token      : Boolean            := True;
   begin
      loop -- three tokens, Op = Delete
         declare
            Next_Node : constant Valid_Node_Access := Peek_Shared_Terminal (Peek_State);
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
                     Peek_Next_Shared_Terminal (Tree, Peek_State);
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

         exit when Tokens (Tokens_Last) = Tree.Lexer.Descriptor.EOI_ID or Tokens_Last = 3;

         if Inc_Shared_Token then
            Peek_Next_Shared_Terminal (Tree, Peek_State);
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
      if Config.Input_Stream.First /= No_Element then
         declare
            Result : constant Node_Access := First_Terminal (Tree, Config.Input_Stream);
         begin
            if Result /= Invalid_Node_Access then
               return Result;
            end if;
         end;
      end if;

      --  Always finds EOI.
      return Tree.First_Terminal (Config.Current_Shared_Token).Node;
   end Peek_Current_First_Terminal;

   function Peek_Current_First_Shared_Terminal
     (Tree              : in Syntax_Trees.Tree;
      Config            : in Configuration;
      Following_Element : in Boolean := True)
     return Syntax_Trees.Node_Access
   is
      use Bounded_Streams;
      use Syntax_Trees;
   begin
      if Config.Input_Stream.First /= No_Element then
         declare
            Ref : Config_Stream_Parents (Config.Input_Stream'Access);
         begin
            First_Shared_Terminal (Tree, Ref);
            if Ref.Node /= Invalid_Node_Access then
               return Ref.Node;
            end if;
         end;
      end if;

      if Config.Current_Shared_Token.Node = Invalid_Node_Access and Following_Element then
         declare
            Temp : Stream_Node_Parents := Tree.To_Stream_Node_Parents (Config.Current_Shared_Token);
         begin
            Tree.Next_Shared_Terminal (Temp);
            return Temp.Ref.Node;
         end;
      else
         return Config.Current_Shared_Token.Node;
      end if;
   end Peek_Current_First_Shared_Terminal;

   procedure First_Shared_Terminal
     (Tree : in     Syntax_Trees.Tree;
      Ref  : in out Config_Stream_Parents)
   is
      use Bounded_Streams;
      use Syntax_Trees;
   begin
      Ref.Element := Ref.Stream.First;
      Ref.Node    := Invalid_Node_Access;

      loop
         exit when not Has_Element (Ref.Element);

         Ref.Node := Tree.First_Shared_Terminal (Ref.Stream.Element (Ref.Element), Ref.Parents);

         exit when Ref.Node /= Invalid_Node_Access;

         Ref.Stream.Next (Ref.Element);
      end loop;
   end First_Shared_Terminal;

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

   procedure First_Terminal
     (Tree : in     Syntax_Trees.Tree;
      Ref  : in out Config_Stream_Parents)
   is
      use Bounded_Streams;
      use Syntax_Trees;
   begin
      Ref.Element := Ref.Stream.First;
      loop
         exit when not Has_Element (Ref.Element);

         Ref.Node := Tree.First_Terminal (Ref.Stream.all (Ref.Element));
         exit when Ref.Node /= Invalid_Node_Access;

         Ref.Stream.Next (Ref.Element);
      end loop;
   end First_Terminal;

   procedure Last_Shared_Terminal
     (Tree : in     Syntax_Trees.Tree;
      Ref  : in out Config_Stream_Parents)
   is
      use Bounded_Streams;
      use Syntax_Trees;
   begin
      Ref.Element := Ref.Stream.Last;
      Ref.Node    := Invalid_Node_Access;

      loop
         exit when not Has_Element (Ref.Element);

         Ref.Node := Tree.Last_Shared_Terminal (Ref.Stream.all (Ref.Element), Ref.Parents);
         exit when Ref.Node /= Invalid_Node_Access;

         Ref.Stream.Previous (Ref.Element);
      end loop;
   end Last_Shared_Terminal;

   function First_Input_Shared_Terminal
     (Tree   :         in Syntax_Trees.Tree;
      Config : aliased in Configuration)
     return Syntax_Trees.Node_Access
   is
      Ref : Config_Stream_Parents (Config.Input_Stream'Access);
   begin
      First_Shared_Terminal (Tree, Ref);
      return Ref.Node;
   end First_Input_Shared_Terminal;

   procedure Next_Shared_Terminal
     (Tree : in     Syntax_Trees.Tree;
      Ref  : in out Config_Stream_Parents)
   is
      use Bounded_Streams;
   begin
      Tree.Next_Shared_Terminal (Ref.Node, Ref.Parents);

      loop
         exit when Ref.Node /= Syntax_Trees.Invalid_Node_Access;
         Ref.Element := Ref.Stream.Next (Ref.Element);
         if Ref.Element = No_Element then
            Ref.Node := Syntax_Trees.Invalid_Node_Access;
            exit;
         end if;
         Ref.Node := Tree.First_Shared_Terminal (Ref.Stream.all (Ref.Element), Ref.Parents);
      end loop;
   end Next_Shared_Terminal;

   procedure Prev_Shared_Terminal
     (Tree : in     Syntax_Trees.Tree;
      Ref  : in out Config_Stream_Parents)
   is
      use Bounded_Streams;
   begin
      Tree.Prev_Shared_Terminal (Ref.Node, Ref.Parents);

      loop
         exit when Ref.Node /= Syntax_Trees.Invalid_Node_Access;
         Ref.Element := Ref.Stream.Previous (Ref.Element);
         if Ref.Element = No_Element then
            Ref.Node := Syntax_Trees.Invalid_Node_Access;
            exit;
         end if;
         Ref.Node := Tree.Last_Shared_Terminal (Ref.Stream.all (Ref.Element), Ref.Parents);
      end loop;
   end Prev_Shared_Terminal;

   procedure Do_Delete
     (Tree   : in     Syntax_Trees.Tree;
      Config : in out Configuration)
   is
      use Syntax_Trees;
      use all type Bounded_Streams.Cursor;
   begin
      if Config.Input_Stream.First = Bounded_Streams.No_Element then
         if Tree.Label (Config.Current_Shared_Token.Element) in Terminal_Label then
            Tree.Stream_Next (Config.Current_Shared_Token, Rooted => False);
            return;
         else
            --  Current_Shared_Token needs Breakdown; move it to Config.Input_Stream.
            Config.Input_Stream.Append
              (Tree.Get_Node (Config.Current_Shared_Token.Stream, Config.Current_Shared_Token.Element));
         end if;
      end if;

      loop
         declare
            Next_Node : constant Valid_Node_Access := Config.Input_Stream (Config.Input_Stream.First);
         begin
            exit when Tree.Label (Next_Node) in Terminal_Label;

            Left_Breakdown (Tree, Config.Input_Stream);
         end;
      end loop;

      Config.Input_Stream.Delete_First;
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
                  pragma Assert (Is_Terminal (Op.Del_ID, Tree.Lexer.Descriptor.all), "IMPROVEME: allow delete nonterm");

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
     (Tree                    :         in     Syntax_Trees.Tree;
      Config                  : aliased in out Configuration;
      Inc_Shared_Stream_Token :         in     Boolean;
      Inc_Input_Stream_Token  :         in     Boolean)
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
            Next_Node : constant Syntax_Trees.Valid_Node_Access :=
              (if Config.Input_Stream.First /= Bounded_Streams.No_Element
               then First_Input_Shared_Terminal (Tree, Config)
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
                     Tree.Stream_Next (Config.Current_Shared_Token, Rooted => False);
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
   --  We return Boolean, not Status, because Abandon and Continue
   --  are up to the caller.
   --
   --  If any actions have conflicts, append the conflict configs and actions to
   --  Parse_Items.

   is
      use Parse_Item_Arrays;
      use Config_Op_Arrays;
      use all type In_Parse_Actions.Status_Label;

      Trace      : WisiToken.Trace'Class renames Super.Trace.all;
      Tree       : WisiToken.Syntax_Trees.Tree renames Super.Tree.all;
      Descriptor : WisiToken.Descriptor renames Super.Tree.Lexer.Descriptor.all;
      Table      : Parse_Table renames Shared.Table.all;

      Item       : Parse_Item renames Parse_Item_Array_Refs.Variable_Ref
        (Parse_Items, Parse_Item_Index).Element.all;
      Config     : Configuration renames Item.Config;
      Action_Cur : Parse_Action_Node_Ptr renames Item.Action;
      Action     : Parse_Action_Rec;

      Inc_Shared_Stream_Token : Boolean;
      Inc_Input_Stream_Token  : Boolean;
      Current_Token           : Syntax_Trees.Recover_Token := Get_Current_Token
        (Tree, Config, Inc_Shared_Stream_Token, Inc_Input_Stream_Token);

      New_State : Unknown_State_Index;
      Success   : Boolean := True;

      procedure Get_Action
      is
         --  We use the incremental parse algorithm even if the main parse is
         --  batch, because Push_Back places whole nonterms on
         --  Config.Input_Stream.
         --
         --  Same logic as in Parser.Get_Action, but this
         --  operates on Config.

         Current_State : constant State_Index := Config.Stack.Peek.State;

         First_In_Current : Syntax_Trees.Node_Access;
      begin
         loop --  Skip empty nonterms
            if Is_Terminal (Tree.ID (Current_Token), Descriptor) then
               Action_Cur := Action_For (Table, Current_State, Tree.ID (Current_Token));
               Action     := Action_Cur.Item;
               return;
            else
               --  nonterminal.
               declare
                  New_State : constant Unknown_State_Index := Goto_For
                    (Table, Current_State, Tree.ID (Current_Token));

                  Dummy : Ada.Containers.Count_Type;
                  pragma Unreferenced (Dummy);
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
                        Next_Token (Tree, Config, Inc_Shared_Stream_Token, Inc_Input_Stream_Token);

                        Current_Token := Get_Current_Token
                          (Tree, Config, Inc_Shared_Stream_Token, Inc_Input_Stream_Token);
                     else
                        Action_Cur := Action_For (Table, Current_State, Tree.ID (First_In_Current));
                        Action     := Action_Cur.Item;

                        case Action.Verb is
                        when Shift =>
                           if Config.Input_Stream.Length = 0 then
                              --  Current_Token is from Shared_Stream. We can't do Breakdown in
                              --  Shared_Stream; that might invalidate other Config.Current_Token.
                              --  So add token to Config.Input_Stream, then breakdown.
                              Config.Input_Stream.Append (Current_Token.Element_Node);
                              Tree.Stream_Next (Config.Current_Shared_Token, Rooted => False);
                           end if;

                           Left_Breakdown (Tree, Config.Input_Stream);

                           Current_Token := Get_Current_Token
                             (Tree, Config, Inc_Shared_Stream_Token, Inc_Input_Stream_Token);

                           if Trace_McKenzie > Extra then
                              Trace.Put_Line
                                (Trace_Prefix & ": breakdown; input_stream: " & LR.Image
                                   (Config.Input_Stream, Tree));
                              Trace.Put_Line (" ... current_token: " & Super.Tree.Image (Current_Token));
                           end if;
                           return;

                        when Accept_It | Reduce =>
                           return;

                        when Error =>
                           --  We don't do Undo_Reduce here; Explore will do that with an appropriate cost.
                           return;
                        end case;
                     end if;
                  end if;
               end;
            end if;
         end loop;
      end Get_Action;

   begin
      if Trace_McKenzie > Detail then
         if Trace_McKenzie > Extra then
            Put_Line (Trace, Trace_Prefix & ": stack: " & LR.Image (Config.Stack, Tree));
            if Config.Current_Insert_Delete /= No_Insert_Delete then
               Put_Line (Trace, Tree, Super.Stream (Parser_Index), Trace_Prefix & ": Insert_Delete: " &
                           Image (Config.Insert_Delete, Descriptor));
            end if;
            if Config.Input_Stream.Length > 0 then
               Put_Line (Trace, Tree, Super.Stream (Parser_Index), Trace_Prefix & ": input_stream: " &
                           LR.Image (Config.Input_Stream, Tree));
            end if;
         end if;

         if Shared_Token_Goal /= Syntax_Trees.Invalid_Node_Index then
            Put_Line (Trace, Tree, Super.Stream (Parser_Index), Trace_Prefix & ": Shared_Token_Goal :" &
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
                     Put_Line (Trace, Tree, Super.Stream (Parser_Index),
                               Trace_Prefix & ": too many conflicts; abandoning");
                     raise Bad_Config;
                  end if;
               else
                  if Trace_McKenzie > Detail then
                     Put_Line
                       (Trace, Tree, Super.Stream (Parser_Index), Trace_Prefix & ":" & State_Index'Image
                          (Config.Stack.Peek.State) & ": add conflict " &
                          Image (Conflict.Item, Descriptor));
                  end if;

                  Append (Parse_Items, (Config, Conflict, Parsed => False, Shift_Count => Item.Shift_Count));
               end if;
            end if;
         end;

         if Trace_McKenzie > Extra then
            Put_Line
              (Trace, Tree, Super.Stream (Parser_Index), Trace_Prefix & ":" &
                 Config.Stack.Peek.State'Image &
                 ":" & Syntax_Trees.Image (Tree, Current_Token) &
                 " : " & Image (Action, Descriptor) &
                 (if Action.Verb = Reduce
                  then " via" & Config.Stack.Peek (SAL.Peek_Type (Action.Token_Count + 1)).State'Image
                  else ""));
         end if;

         case Action.Verb is
         when Shift =>
            Item.Shift_Count := Item.Shift_Count + 1;

            Config.Stack.Push ((Action.State, Current_Token));

            Next_Token (Tree, Config, Inc_Shared_Stream_Token, Inc_Input_Stream_Token);
            Current_Token := Get_Current_Token
              (Tree, Config, Inc_Shared_Stream_Token, Inc_Input_Stream_Token);

         when Reduce =>
            declare
               Nonterm : Syntax_Trees.Recover_Token;
            begin
               Config.User_Parse_Action_Status := Reduce_Stack
                 (Super, Config.Stack, Action, Nonterm,
                  Default_Contains_Virtual => Config.Current_Insert_Delete /= No_Insert_Delete);

               case Config.User_Parse_Action_Status.Label is
               when Ok =>
                  New_State := Config.Stack.Peek.State;
                  New_State := Goto_For (Table, New_State, Action.Production.LHS);

                  if New_State = Unknown_State then
                     --  Most likely from an inappropriate language fix.
                     if Trace_McKenzie > Outline then
                        Base.Put (Trace_Prefix & ": Unknown_State: ", Super, Parser_Index, Config);
                        Put_Line
                          (Trace, Tree, Super.Stream (Parser_Index), Trace_Prefix & ": stack: " &
                             LR.Image (Config.Stack, Tree));
                     end if;

                     --  We can't just return False here; user must abandon this config.
                     raise Bad_Config;
                  end if;

                  Config.Stack.Push ((New_State, Nonterm));

               when In_Parse_Actions.Error =>
                  Config.Error_Token                   := Nonterm;
                  Config.User_Parse_Action_Token_Count := Action.Token_Count;
                  Success                              := False;
               end case;
            end;

         when Error =>

            Config.Error_Token :=
              (ID          => Tree.ID (Current_Token),
               Byte_Region => Tree.Byte_Region (Current_Token),
               others      => <>);
            Success            := False;

         when Accept_It =>
            null;
         end case;

         exit when not Success or
           Action.Verb = Accept_It or
           (if Shared_Token_Goal = Syntax_Trees.Invalid_Node_Index
            then Length (Config.Insert_Delete) = 0
            else Super.Tree.Get_Node_Index (Peek_Current_First_Shared_Terminal (Tree, Config)) > Shared_Token_Goal);

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
         Config.Error_Token              := Syntax_Trees.Invalid_Recover_Token;
         Config.User_Parse_Action_Status := (Label => In_Parse_Actions.Ok);
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
