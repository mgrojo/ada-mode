--  Abstract :
--
--  See spec
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

package body WisiToken.LR.McKenzie_Recover.Parse is

   procedure Compute_Nonterm
     (ID              : in     Token_ID;
      Stack           : in     Recover_Stacks.Stack;
      Tokens          : in out Recover_Token_Array;
      Nonterm         :    out Recover_Token;
      Default_Virtual : in     Boolean)
   is
      use all type Ada.Containers.Count_Type;
      use all type SAL.Base_Peek_Type;

      Min_Terminal_Index_Set : Boolean := False;
   begin
      Nonterm :=
        (ID      => ID,
         Virtual => (if Tokens'Length = 0 then Default_Virtual else False),
         others  => <>);

      for I in Tokens'Range loop
         Tokens (I) := Stack (Tokens'Last - I + 1).Token;
      end loop;

      for T of Tokens loop
         Nonterm.Virtual := Nonterm.Virtual or T.Virtual;

         if Nonterm.Byte_Region.First > T.Byte_Region.First then
            Nonterm.Byte_Region.First := T.Byte_Region.First;
         end if;

         if Nonterm.Byte_Region.Last < T.Byte_Region.Last then
            Nonterm.Byte_Region.Last := T.Byte_Region.Last;
         end if;

         if not Min_Terminal_Index_Set then
            if T.Min_Terminal_Index /= Invalid_Token_Index then
               Min_Terminal_Index_Set     := True;
               Nonterm.Min_Terminal_Index := T.Min_Terminal_Index;
            end if;
         end if;
      end loop;
   end Compute_Nonterm;

   function Reduce_Stack
     (Shared          : not null access Base.Shared;
      Stack           : in out          Recover_Stacks.Stack;
      Action          : in              Reduce_Action_Rec;
      Nonterm         :    out          Recover_Token;
      Default_Virtual : in              Boolean)
     return Semantic_Checks.Check_Status
   is
      use all type SAL.Base_Peek_Type;
      use all type Semantic_Checks.Semantic_Check;
      use all type Semantic_Checks.Check_Status_Label;

      Last   : constant SAL.Base_Peek_Type := SAL.Base_Peek_Type (Action.Token_Count);
      Tokens : Recover_Token_Array (1 .. Last);
   begin
      Compute_Nonterm (Action.LHS, Stack, Tokens, Nonterm, Default_Virtual);

      if Action.Check = null then
         --  Now we can pop the stack.
         Stack.Pop (SAL.Base_Peek_Type (Action.Token_Count));
         return (Label => Ok);
      else
         return Status : constant Semantic_Checks.Check_Status :=
           Action.Check (Shared.Lexer, Nonterm, Tokens)
         do
            if Status.Label = Ok then
               Stack.Pop (SAL.Base_Peek_Type (Action.Token_Count));
            end if;
         end return;
      end if;
   end Reduce_Stack;

   function Parse_One_Item
     (Super             : not null access Base.Supervisor;
      Shared            : not null access Base.Shared;
      Parser_Index      : in              SAL.Peek_Type;
      Parse_Items       : in out          Parse_Item_Arrays.Vector;
      Parse_Item_Index  : in              Positive;
      Shared_Token_Goal : in              Base_Token_Index;
      Trace_Prefix      : in              String)
     return Boolean
   is
      --  Perform parse actions on Parse_Items (Parse_Item_Index), until one
      --  fails (return False) or Shared_Token_Goal is shifted (return
      --  True).
      --
      --  We return Boolean, not Check_Status, because Abandon and Continue
      --  are up to the caller.
      --
      --  If any actions have a conflict, append the conflict config and action to
      --  Parse_Items.

      use all type Ada.Containers.Count_Type;
      use all type SAL.Base_Peek_Type;
      use all type Semantic_Checks.Check_Status_Label;

      Trace      : WisiToken.Trace'Class renames Super.Trace.all;
      Descriptor : WisiToken.Descriptor renames Super.Trace.Descriptor.all;
      Table      : Parse_Table renames Shared.Table.all;

      Item   : Parse_Item renames Parse_Items (Parse_Item_Index);
      Config : Configuration renames Item.Config;
      Action : Parse_Action_Node_Ptr renames Item.Action;

      Current_Token : Base_Token :=
        (if Config.Current_Inserted = No_Inserted
         then
            Shared.Terminals.all (Config.Current_Shared_Token)
         else
           (ID     => Config.Inserted (Config.Current_Inserted),
            others => <>));

      New_State : Unknown_State_Index;
      Success   : Boolean := True;

   begin
      if Trace_McKenzie > Detail then
         Base.Put (Trace_Prefix & ": " & Image (Current_Token, Descriptor), Super, Shared, Parser_Index, Config);
      end if;

      Item.Parsed := True;

      if Action = null then
         Action := Action_For (Table, Config.Stack (1).State, Current_Token.ID);
      end if;

      loop
         if Action.Next /= null then
            if Parse_Items.Is_Full then
               if Trace_McKenzie > Detail then
                  Put_Line (Trace, Super.Label (Parser_Index), Trace_Prefix & ": too many conflicts; abandoning");
               end if;
            else
               if Trace_McKenzie > Detail then
                  Put_Line
                    (Trace, Super.Label (Parser_Index), Trace_Prefix & ":" & State_Index'Image
                       (Config.Stack.Peek.State) & ": add conflict " &
                       Image (Action.Next.Item, Descriptor));
               end if;

               Parse_Items.Append ((Config, Action.Next, Parsed => False, Shift_Count => 0));
            end if;
         end if;

         if Trace_McKenzie > Extra then
            Put_Line
              (Trace, Super.Label (Parser_Index), Trace_Prefix & ":" & State_Index'Image (Config.Stack.Peek.State) &
                 " : " & Image (Current_Token, Descriptor) &
                 " : " & Image (Action.Item, Descriptor));
         end if;

         case Action.Item.Verb is
         when Shift =>
            Item.Shift_Count := Item.Shift_Count + 1;

            Config.Stack.Push
              ((Action.Item.State,
                Syntax_Trees.Invalid_Node_Index,
                (Current_Token.ID,
                 Byte_Region        => Current_Token.Byte_Region,
                 Min_Terminal_Index =>
                   (if Config.Current_Inserted = No_Inserted
                    then Config.Current_Shared_Token
                    else Invalid_Token_Index),
                 Name              => Null_Buffer_Region,
                 Virtual           => Config.Current_Inserted /= No_Inserted)));

            if Config.Inserted.Last_Index > 0 and Config.Current_Inserted = Config.Inserted.Last_Index then
               Config.Current_Inserted := No_Inserted;
               Config.Inserted.Clear;

               Current_Token := Shared.Terminals.all (Config.Current_Shared_Token);

            elsif Config.Current_Inserted /= No_Inserted then
               Config.Current_Inserted := Config.Current_Inserted + 1;

               Current_Token :=
                 (ID     => Config.Inserted (Config.Current_Inserted),
                  others => <>);

            else
               Config.Current_Shared_Token := Config.Current_Shared_Token + 1;

               Current_Token := Shared.Terminals.all (Config.Current_Shared_Token);
            end if;

         when Reduce =>
            declare
               Nonterm : Recover_Token;
            begin
               Config.Check_Status := Reduce_Stack
                 (Shared, Config.Stack, Action.Item, Nonterm,
                  Default_Virtual => Config.Current_Inserted /= No_Inserted);

               case Config.Check_Status.Label is
               when Ok =>
                  New_State := Config.Stack.Peek.State;
                  New_State := Goto_For (Table, New_State, Action.Item.LHS);

                  Config.Stack.Push ((New_State, Syntax_Trees.Invalid_Node_Index, Nonterm));

               when Semantic_Checks.Error =>
                  Config.Error_Token       := Nonterm;
                  Config.Check_Token_Count := Action.Item.Token_Count;
                  Success                  := False;
               end case;
            end;

         when Error =>

            Config.Error_Token :=
              (ID          => Current_Token.ID,
               Byte_Region => Current_Token.Byte_Region,
               others      => <>);
            Success            := False;

         when Accept_It =>
            null;
         end case;

         exit when not Success or
           Action.Item.Verb = Accept_It or
           (if Shared_Token_Goal = Invalid_Token_Index
            then Config.Inserted.Length = 0
            else Config.Current_Shared_Token > Shared_Token_Goal);

         Action := Action_For (Table, Config.Stack (1).State, Current_Token.ID);
      end loop;

      return Success;
   end Parse_One_Item;

   function Parse
     (Super             : not null access Base.Supervisor;
      Shared            : not null access Base.Shared;
      Parser_Index      : in              SAL.Peek_Type;
      Parse_Items       :    out          Parse_Item_Arrays.Vector;
      Config            : in              Configuration;
      Shared_Token_Goal : in              Base_Token_Index;
      All_Conflicts     : in              Boolean;
      Trace_Prefix      : in              String)
     return Boolean
   is
      use all type Ada.Containers.Count_Type;

      Trace : WisiToken.Trace'Class renames Super.Trace.all;

      Last_Index : Positive;
      Success    : Boolean;
   begin
      Parse_Items.Clear;
      Parse_Items.Append ((Config, Action => null, Parsed => False, Shift_Count => 0));

      --  Clear any errors; so they reflect the parse result.
      Parse_Items (Parse_Items.First_Index).Config.Error_Token.ID := Invalid_Token_ID;
      Parse_Items (Parse_Items.First_Index).Config.Check_Status   := (Label => Semantic_Checks.Ok);

      loop
         --  Loop over initial config and any conflicts.
         Last_Index := Parse_Items.Last_Index;

         Success := Parse_One_Item
           (Super, Shared, Parser_Index, Parse_Items, Last_Index, Shared_Token_Goal, Trace_Prefix);

         exit when Parse_Items.Last_Index = Last_Index;

         exit when Success and not All_Conflicts;

         if Trace_McKenzie > Detail then
            Put_Line (Trace, Super.Label (Parser_Index), Trace_Prefix & ": parse conflict");
         end if;
      end loop;

      return Success;
   end Parse;

end WisiToken.LR.McKenzie_Recover.Parse;
