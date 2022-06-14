--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2022 Free Software Foundation All Rights Reserved.
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

package body WisiToken.Parse.Packrat.Parser is

   function Image (Item : in Recover_Op_Nodes; Tree : in Syntax_Trees.Tree) return String
   is
      use WisiToken.Syntax_Trees;
   begin
      return
        "(" & Image (Item.Op) & ", " &
        Image (Item.ID, Tree.Lexer.Descriptor.all) &
        (if Item.Node = Invalid_Node_Access
         then ""
         else Tree.Image (Item.Node)) & "," &
        Item.Pos'Image & "," &
        Item.Error_Pos'Image & ")";
   end Image;

   overriding procedure Finalize (Object : in out Parser)
   is begin
      --  Derivs holds Stream_Index references to Tree, so Derivs must be
      --  cleared before Tree is finalized.
      for Parser_State of Object.Packrat_Parsers loop
         WisiToken.Parse.Packrat.Clear (Parser_State.Derivs);
         Parser_State.Result := No_Result_Memo;
      end loop;

      WisiToken.Parse.Parser.Finalize (WisiToken.Parse.Parser.Parser (Object));
   end Finalize;

   function Packrat_Parser
     (Parser       : in out Packrat.Parser.Parser;
      LR_Stream_ID : in     Syntax_Trees.Stream_ID)
     return Parser_State_Lists.Cursor
   is
      use Parser_State_Lists;

      Descriptor : WisiToken.Descriptor renames Parser.Tree.Lexer.Descriptor.all;
      Result     : Cursor;
   begin
      for Cur in Parser.Packrat_Parsers.Iterate loop
         if Parser.Packrat_Parsers (Cur).LR_Stream_Label = Syntax_Trees.Label (LR_Stream_ID) then
            Result := Cur;
         end if;
      end loop;

      if Result = No_Element then
         Result := Parser.Packrat_Parsers.Append
           ((First_Nonterminal => Descriptor.First_Nonterminal,
             Last_Nonterminal  => Descriptor.Last_Nonterminal,
             LR_Stream_Label   => Syntax_Trees.Label (LR_Stream_ID),
             Packrat_Label     => Parser.Next_Packrat_Label,
             Derivs            => (Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal => <>),
             others            => <>));

         Parser.Next_Packrat_Label := @ + 1;
      end if;

      return Result;
   end Packrat_Parser;

   function LR_Parser
     (Parser          : in out Packrat.Parser.Parser;
      LR_Stream_Label : in     Syntax_Trees.Stream_Label)
     return LR.Parser_Lists.Parser_Node_Access
   is
      use LR.Parser_Lists;
   begin
      for Cur in Parser.Parsers.Iterate loop
         if Syntax_Trees.Label (Parser.Parsers (Cur).Stream) = LR_Stream_Label then
            return Cur;
         end if;
      end loop;

      return No_Element;
   end LR_Parser;

   function Delete_Valid
     (Parser       : in Packrat.Parser.Parser;
      Parser_State : in Packrat.Parser.Parser_State;
      Pos          : in Syntax_Trees.Stream_Index)
     return Boolean
   is
      use Recover_Op_Nodes_Trees;
      Found : constant Cursor := Parser_State.Insert_Delete.Find
        (Parser.Tree.Get_Node_Index (Parser.Tree.Shared_Stream, Pos));
   begin
      return Found /= No_Element and then
        (for some Op of Parser_State.Insert_Delete.Constant_Ref (Found) => Op.Op = Delete);
   end Delete_Valid;

   function Has_Input
     (Parser       : in Packrat.Parser.Parser;
      Parser_State : in Packrat.Parser.Parser_State;
      Pos          : in Syntax_Trees.Stream_Index)
     return Boolean
   is
      use Recover_Op_Nodes_Trees;
      Found : constant Cursor := Parser_State.Insert_Delete.Find
        (Parser.Tree.Get_Node_Index (Parser.Tree.Shared_Stream, Pos));
   begin
      return Found /= No_Element and then
        (for some Op of Parser_State.Insert_Delete.Constant_Ref (Found) => Op.Op = Insert);
   end Has_Input;

   function Input_Op
     (Parser       : in Packrat.Parser.Parser;
      Parser_State : in Packrat.Parser.Parser_State;
      Pos          : in Syntax_Trees.Stream_Index;
      Prev_Node    : in Syntax_Trees.Node_Access)
     return ID_Node_Type
   is
      use all type WisiToken.Syntax_Trees.Node_Access;
      use Recover_Op_Nodes_Lists;

      List : Recover_Op_Nodes_Lists.List renames Parser_State.Insert_Delete.Constant_Ref
        (Parser.Tree.Get_Node_Index (Parser.Tree.Shared_Stream, Pos));

      Cur      : Cursor := List.Last;
      Prev_Cur : Cursor := Previous (Cur);
   begin
      return Result : ID_Node_Type do
         if List (Cur).Node = Prev_Node then
            return;

         else
            loop
               if Prev_Cur = No_Element or else
                 (List (Prev_Cur).Op = Delete or else
                    List (Prev_Cur).Node = Prev_Node)
               then
                  declare
                     Op : Recover_Op_Nodes renames Constant_Ref (Cur);
                  begin
                     Result := (ID => Op.ID, Node => Op.Node);
                  end;
                  exit;

               end if;
               Cur      := Prev_Cur;
               Prev_Cur := Previous (Cur);
            end loop;
         end if;
      end return;
   end Input_Op;

   function Max_Examined_Pos (Shared_Parser : in out Parser) return Syntax_Trees.Node_Index
   is
      use Syntax_Trees;
   begin
      return Result : Node_Index := 0 -- SOI
      do
         for Parser_State of Shared_Parser.Packrat_Parsers loop
            if Parser_State.Max_Examined_Pos = Invalid_Node_Index then
               null;
            elsif Parser_State.Max_Examined_Pos > Result then
               Result := Parser_State.Max_Examined_Pos;
            end if;
         end loop;
      end return;
   end Max_Examined_Pos;

   procedure Packrat_Parse
     (Shared_Parser : in out Parser;
      Log_File      : in     Ada.Text_IO.File_Type)
   is separate;

   procedure Finish_Parse (Parser : in out WisiToken.Parse.Packrat.Parser.Parser'Class)
   is
      use all type SAL.Base_Peek_Type;
      use WisiToken.Syntax_Trees;
      Tree  : Syntax_Trees.Tree renames Parser.Tree;
      Trace : WisiToken.Trace'Class renames Tree.Lexer.Trace.all;

      Success_Count : Integer := 0;
   begin
      if Trace_Time then
         Trace.Put_Clock ("finish parse");
      end if;

      --  First count successful parsers.
      for Parser_State of Parser.Packrat_Parsers loop
         if Parser_State.Result.State = Packrat.Success then
            Success_Count := @ + 1;
         end if;
      end loop;

      if Success_Count = 0 then
         --  preserve Derivs for error recover
         declare
            Result : Memo_Entry renames Parser.Packrat_Parsers (Parser.Packrat_Parsers.First).Result;
            Msg    : constant String := Tree.Error_Message
              (Ref     => (Tree.Shared_Stream,
                           Result.Max_Examined_Pos,
                           Tree.Get_Node (Tree.Shared_Stream, Result.Max_Examined_Pos)),
               Message => "parse failed");
         begin
            if Trace_Parse > Outline then
               Tree.Lexer.Trace.Put_Line (Msg);
            end if;

            --  If we raise Syntax_Error, the caller assumes syntax error
            --  information is in the tree; not true for packrat. FIXME: if at
            --  eoi, do derivs_to_parse_streams to put errors in tree.
            raise WisiToken.Parse_Error with Msg;
            --  FIXME packrat: add "expecting: ..." based on last nonterm?
         end;

      elsif Success_Count = 1 and Parser.Packrat_Parsers.Length = 1 then
         declare
            Parser_State : Packrat.Parser.Parser_State renames Parser.Packrat_Parsers (Parser.Packrat_Parsers.First);
         begin
            --  Copy recover counts for unit test.
            Parser.Recover_Enqueue_Count := Parser_State.Recover_Enqueue_Count;
            Parser.Recover_Check_Count   := Parser_State.Recover_Check_Count;

            Tree.Set_Root (Parser_State.Result.Result);
            Parser_State.Result := No_Result_Memo;
            Clear (Parser_State.Derivs);
         end;

         Tree.Finish_Parse;

         if Trace_Parse > Outline then
            Trace.Put_Line ("packrat parse succeed");
         end if;

      else
         --  test_mckenzie_recover.adb Packrat_Proc Error_3
              declare
                  use all type Parser_Lists.Cursor;
                  Error_Parser_Count : Integer := (if Shared_Parser.Tree.Lexer.Errors.Length > 0 then 1 else 0);

                  Recover_Cost           : Integer;
                  Min_Recover_Cost       : Integer                   := Integer'Last;
                  Recover_Ops_Length     : Ada.Containers.Count_Type;
                  Min_Recover_Ops_Length : Ada.Containers.Count_Type := Ada.Containers.Count_Type'Last;
                  Recover_Cur            : Parser_Lists.Cursor       := Current_Parser;
               begin
                  Current_Parser := Shared_Parser.Parsers.First;
                  loop
                     if Current_Parser.Verb = Accept_It then
                        if Current_Parser.State_Ref.Error_Count > 0 then
                           Error_Parser_Count := Error_Parser_Count + 1;
                        end if;
                        Current_Parser.Next;
                     else
                        declare
                           Temp  : Parser_Lists.Cursor := Current_Parser;
                        begin
                           Current_Parser.Next;
                           Shared_Parser.Parsers.Terminate_Parser
                             (Temp, Shared_Parser.Tree, "zombie", Trace);
                        end;
                     end if;
                     exit when Current_Parser.Is_Done;
                  end loop;

                  if Error_Parser_Count > 0 then
                     --  There was at least one error. We assume that caused the ambiguous
                     --  parse, and we pick the parser with the minimum cost and minimum
                     --  recover ops length (consistent with Duplicate_State) to allow the
                     --  parse to succeed. We terminate the other parsers so the remaining
                     --  parser can do Execute_Actions.
                     --
                     --  If there are multiple errors, this metric is not very meaningful.
                     --
                     --  Note all surviving parsers must have the same error count.
                     Current_Parser := Shared_Parser.Parsers.First;
                     loop
                        Recover_Cost := Current_Parser.State_Ref.Total_Recover_Cost;
                        if Recover_Cost < Min_Recover_Cost then
                           Min_Recover_Cost       := Recover_Cost;
                           Min_Recover_Ops_Length := Current_Parser.State_Ref.Max_Recover_Ops_Length;
                           Recover_Cur            := Current_Parser;

                        elsif Recover_Cost = Min_Recover_Cost then
                           Recover_Ops_Length := Current_Parser.State_Ref.Max_Recover_Ops_Length;
                           if Recover_Ops_Length < Min_Recover_Ops_Length then
                              Min_Recover_Ops_Length := Recover_Ops_Length;
                              Recover_Cur    := Current_Parser;
                           end if;
                        end if;
                        Current_Parser.Next;
                        exit when Current_Parser.Is_Done;
                     end loop;

                     Current_Parser := Shared_Parser.Parsers.First;
                     loop
                        if Current_Parser = Recover_Cur then
                           Current_Parser.Next;
                        else
                           declare
                              Temp  : Parser_Lists.Cursor := Current_Parser;
                           begin
                              Current_Parser.Next;
                              Shared_Parser.Parsers.Terminate_Parser
                                (Temp, Shared_Parser.Tree,
                                 (if Recover_Cost = Min_Recover_Cost and then
                                    Recover_Ops_Length = Min_Recover_Ops_Length
                                  then "random"
                                  else "recover cost/min length"),
                                 Trace);
                           end;
                        end if;
                        exit when Current_Parser.Is_Done;
                     end loop;

                     exit Main_Loop;

                  else
                     --  There were no previous errors. We allow the parse to fail, on the
                     --  assumption that an otherwise correct input should not yield an
                     --  ambiguous parse.
                     Current_Parser := Shared_Parser.Parsers.First;
                     raise WisiToken.Parse_Error with Shared_Parser.Tree.Error_Message
                       (Shared_Parser.Tree.Current_Token (Current_Parser.Stream),
                        "Ambiguous parse:" & SAL.Base_Peek_Type'Image (Count) & " parsers active.");
                  end if;
               end;
            end if;
         end;
      end if;
   end Finish_Parse;

   procedure Print_Derivs
     (Derivs : in WisiToken.Parse.Packrat.Derivs;
      Tree   : in WisiToken.Syntax_Trees.Tree)
   is
      Trace  : WisiToken.Trace'Class renames Tree.Lexer.Trace.all;
      Count  : Integer := 0;
   begin
      for Nonterm in Derivs'Range loop
         for Pos in Derivs (Nonterm).First_Index .. Derivs (Nonterm).Last_Index loop

            case Derivs (Nonterm)(Pos).State is
            when No_Result =>
               null;

            when Failure | Success =>
               Count := @ + 1;
               Trace.Put_Line (Packrat.Image (Derivs (Nonterm)(Pos), Nonterm, Pos, Tree));

            end case;
         end loop;
      end loop;
      Trace.Put_Line ("... failed + success count:" & Count'Image);
   end Print_Derivs;

   procedure Print_Derivs (Parser : in WisiToken.Parse.Packrat.Parser.Parser)
   is begin
      for Parser_State of Parser.Packrat_Parsers loop
         Parser.Tree.Lexer.Trace.Put_Line ("packrat" & Parser_State.Packrat_Label'Image);
         Print_Derivs (Parser_State.Derivs, Parser.Tree);
         Parser.Tree.Lexer.Trace.New_Line;
      end loop;
   end Print_Derivs;

end WisiToken.Parse.Packrat.Parser;
