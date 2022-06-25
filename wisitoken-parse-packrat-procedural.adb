--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018 - 2022 Free Software Foundation, Inc.
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

with WisiToken.In_Parse_Actions;
package body WisiToken.Parse.Packrat.Procedural is

   function Apply_Rule
     (Parser       : in out Procedural.Parser;
      Parser_State : in out Packrat.Parser.Parser_State;
      R            : in     Token_ID;
      Last_Pos     : in     Syntax_Trees.Stream_Index)
     return Memo_Entry
   with Post => Apply_Rule'Result.State in Failure .. Success;

   function Eval
     (Parser       : in out Procedural.Parser;
      Parser_State : in out Packrat.Parser.Parser_State;
      R            : in     Token_ID;
      Last_Pos     : in     Syntax_Trees.Stream_Index)
     return Memo_Entry
   with Post => Eval'Result.State in Failure .. Success;

   procedure Trace_Deriv_Success
     (Parser : in out Procedural.Parser;
      Pos    : in     WisiToken.Syntax_Trees.Stream_Index;
      Memo   : in     Success_Memo_Entry)
   is
      use Syntax_Trees;
      Tree : Syntax_Trees.Tree renames Parser.Tree;
   begin
      --  match LR parse trace with trace_parse=2
      Tree.Lexer.Trace.Put_Line
        ((if Trace_Packrat_McKenzie > Outline
          then Node_Index'Image (Tree.Get_Node_Index (Tree.Shared_Stream, Pos)) & "; "
          else "") &
           Tree.Image
             (Memo.Result,
              Children              => True,
              Node_Numbers          => Trace_Packrat_McKenzie > Outline,
              Terminal_Node_Numbers => True,
              RHS_Index             => True) &
           (if Trace_Packrat_McKenzie > Outline
            then " last_pos" & Node_Index'Image (Tree.Get_Node_Index (Tree.Shared_Stream, Memo.Last_Pos)) &
              " max_ex_pos" & Node_Index'Image (Tree.Get_Node_Index (Tree.Shared_Stream, Memo.Max_Examined_Pos))
            else ""));
   end Trace_Deriv_Success;

   ----------
   --  bodies

   function Eval
     (Parser       : in out Procedural.Parser;
      Parser_State : in out Packrat.Parser.Parser_State;
      R            : in     Token_ID;
      Last_Pos     : in     Syntax_Trees.Stream_Index)
     return Memo_Entry
   is
      use all type WisiToken.Syntax_Trees.Stream_Index;

      Tree       : Syntax_Trees.Tree renames Parser.Tree;
      Descriptor : WisiToken.Descriptor renames Tree.Lexer.Descriptor.all;
      Trace      : WisiToken.Trace'Class renames Tree.Lexer.Trace.all;

      subtype Terminal is Token_ID range Descriptor.First_Terminal .. Descriptor.Last_Terminal;

      Pos      : Syntax_Trees.Stream_Index := Last_Pos; --  last token parsed.
      Next_Pos : Syntax_Trees.Stream_Index := Tree.Stream_Next (Tree.Shared_Stream, Pos);

      Max_Examined_Pos : Syntax_Trees.Stream_Index := Last_Pos;

      procedure Update_Max_Examined_Pos (New_Pos : in Syntax_Trees.Stream_Index)
      is begin
         if Tree.Byte_Region
           (Tree.Get_Node (Tree.Shared_Stream, New_Pos), Trailing_Non_Grammar => False).First >
           Tree.Byte_Region
             (Tree.Get_Node (Tree.Shared_Stream, Max_Examined_Pos), Trailing_Non_Grammar => False).First
         then
            Max_Examined_Pos := New_Pos;
         end if;
      end Update_Max_Examined_Pos;

   begin
      if Trace_Parse > Extra then
         Trace.Put_Line ("eval: " & Image (R, Descriptor) & " @" & Image_Pos (Tree, Next_Pos));
      end if;

      for RHS_Index in Parser.Grammar (R).RHSs.First_Index .. Parser.Grammar (R).RHSs.Last_Index loop
         loop
            exit when not Parser.Delete_Valid (Parser_State, Next_Pos);
            Next_Pos := Tree.Stream_Next (Tree.Shared_Stream, Next_Pos);
         end loop;

         declare
            use all type Ada.Containers.Count_Type;
            RHS  : WisiToken.Productions.Right_Hand_Side renames Parser.Grammar (R).RHSs (RHS_Index);
            Memo : Memo_Entry; --  for temporary or intermediate results
         begin
            if RHS.Tokens.Length = 0 then
               return Result : constant Memo_Entry :=
                 (State            => Success,
                  Max_Examined_Pos => Max_Examined_Pos,
                  Result           => Tree.Add_Nonterm
                    (Production    => (R, RHS_Index),
                     Children      => (1 .. 0 => Syntax_Trees.Invalid_Node_Access),
                     Clear_Parents => False),
                  Last_Pos         => Pos)
               do
                  if Trace_Parse > Extra then
                     Trace.Put_Line (Image (Result, R, RHS_Index, Pos, Tree));
                  end if;
               end return;
            else
               declare
                  use all type WisiToken.Syntax_Trees.In_Parse_Actions.In_Parse_Action;
                  use all type WisiToken.Syntax_Trees.Node_Access;
                  Children : Syntax_Trees.Node_Access_Array
                    (SAL.Base_Peek_Type (RHS.Tokens.First_Index) .. SAL.Base_Peek_Type (RHS.Tokens.Last_Index));

                  In_Parse_Action : constant Syntax_Trees.In_Parse_Actions.In_Parse_Action :=
                    Parser.Get_In_Parse_Action ((R, RHS_Index));
               begin
                  for I in RHS.Tokens.First_Index .. RHS.Tokens.Last_Index loop
                     if Trace_Parse > Extra then
                        Trace.Put (Image (Production_ID'(R, RHS_Index), Descriptor) & "," & I'Image & ": ");
                     end if;

                     if RHS.Tokens (I) in Terminal then
                        if Next_Pos = Syntax_Trees.Invalid_Stream_Index then
                           --  We don't update Max_Examined_Pos here; it must already be EOI
                           pragma Assert (Tree.Get_Node (Tree.Shared_Stream, Max_Examined_Pos) = Tree.EOI);
                           goto Fail_RHS;

                        else
                           if Parser.Has_Input (Parser_State, Next_Pos) then
                              declare
                                 use all type SAL.Base_Peek_Type;

                                 Prev_Terminal : constant Syntax_Trees.Node_Access :=
                                   (if I = RHS.Tokens.First_Index then Syntax_Trees.Invalid_Node_Access
                                    else Tree.Last_Terminal (Children (SAL.Base_Peek_Type (I) - 1)));
                                 Input : Packrat.Parser.ID_Node_Type renames
                                   Parser.Input_Op (Parser_State, Next_Pos, Prev_Terminal);
                              begin
                                 if Input.ID = Invalid_Token_ID then
                                    --  Prev_terminal was last virtual input.
                                    null;

                                 elsif Input.ID = RHS.Tokens (I) then
                                    pragma Assert (Input.Node /= Syntax_Trees.Invalid_Node_Access);
                                    Children (SAL.Base_Peek_Type (I)) := Input.Node;
                                    if Trace_Parse > Extra then
                                       Trace.Put_Line (Tree.Image (Input.Node, Node_Numbers => True));
                                    end if;
                                 end if;
                              end;
                           end if;

                           if Children (SAL.Base_Peek_Type (I)) = Syntax_Trees.Invalid_Node_Access then
                              if Tree.ID (Next_Pos) = RHS.Tokens (I) then
                                 Pos := Next_Pos;
                                 Next_Pos := Tree.Stream_Next (Tree.Shared_Stream, Pos);
                                 Children (SAL.Base_Peek_Type (I)) := Tree.Get_Node (Tree.Shared_Stream, Pos);
                                 --  FIXME: why not Update_Max_Examined_Pos here?

                                 if Trace_Parse > Extra then
                                    Trace.Put_Line
                                      (Tree.Image (Children (SAL.Base_Peek_Type (I)), Node_Numbers => True));
                                 end if;

                              else
                                 Update_Max_Examined_Pos (Next_Pos);
                                 goto Fail_RHS;
                              end if;
                           end if;
                        end if;

                     else -- not Terminal
                        Memo := Apply_Rule (Parser, Parser_State, RHS.Tokens (I), Pos);
                        Update_Max_Examined_Pos (Memo.Max_Examined_Pos);

                        case Memo.State is
                        when Success =>
                           if Trace_Parse > Extra then
                              Trace.Put_Line
                                (Image (Production_ID'(R, RHS_Index), Descriptor) & "," & I'Image & ": " &
                                   Tree.Image (Memo.Result, Node_Numbers => True, RHS_Index => True));
                           end if;
                           Children (SAL.Base_Peek_Type (I)) := Memo.Result;
                           Pos := Memo.Last_Pos;
                           Next_Pos := Tree.Stream_Next (Tree.Shared_Stream, Pos);

                        when Failure =>
                           goto Fail_RHS;

                        when No_Result =>
                           raise SAL.Programmer_Error;
                        end case;
                     end if;
                  end loop;

                  return Result : constant Memo_Entry :=
                    (State            => Success,
                     Max_Examined_Pos => Max_Examined_Pos,
                     Result           => Parser.Tree.Add_Nonterm
                       (Production    => (R, RHS_Index),
                        Children      => Syntax_Trees.To_Valid_Node_Access (Children),
                        Clear_Parents => True),
                     --  We must be able to steal nodes from failed nonterms;
                     --  body_instantiation_conflict.wy.
                     Last_Pos         => Pos)
                  do
                     if Trace_Parse > Extra then
                        Trace.Put_Line (Image (Result, R, RHS_Index, Pos, Tree));
                     end if;

                     if In_Parse_Action = null then
                        null;

                     else
                        declare
                           Nonterm_Token : Syntax_Trees.Recover_Token := Parser.Tree.Get_Recover_Token (Result.Result);

                           Children_Token : constant Syntax_Trees.Recover_Token_Array :=
                             Parser.Tree.Children_Recover_Tokens (Result.Result);
                           Status         : constant Syntax_Trees.In_Parse_Actions.Status := In_Parse_Action
                             (Parser.Tree, Nonterm_Token, Children_Token, Recover_Active => False);
                        begin
                           if Trace_Parse > Extra then
                              Trace.Put_Line
                                ("in_parse_action " & In_Parse_Actions.Image (Status, Tree, Result.Result));
                           end if;

                           case Status.Label is
                           when Syntax_Trees.In_Parse_Actions.Ok =>
                              null;

                           when Syntax_Trees.In_Parse_Actions.Error =>
                              raise SAL.Not_Implemented with "packrat in_parse_actions fail";
                              --  FIXME: store the error somewhere, raise a different exception?
                              --  Parser.Tree.Add_Error_To_Stack_Top
                              --    (Parser_State.Stream,
                              --     In_Parse_Action_Error'
                              --       (Status       => Status,
                              --        Recover_Ops  => Recover_Op_Arrays.Empty_Vector,
                              --        Recover_Cost => 0),
                              --     Parser.User_Data);
                           end case;
                        end;
                     end if;
                  end return;

                  <<Fail_RHS>>
                  if Trace_Parse > Extra then
                     Trace.Put_Line
                       (Image (Production_ID'(R, RHS_Index), Descriptor) & " @" &
                          Image_Pos (Tree, Next_Pos) & ": fail");
                  end if;
                  Pos := Last_Pos;
                  Next_Pos := Tree.Stream_Next (Tree.Shared_Stream, Pos);
               end;
            end if;
         end;
      end loop;
      --  get here when all RHSs fail

      return (Failure, Max_Examined_Pos);
   end Eval;

   function Apply_Rule
     (Parser       : in out Procedural.Parser;
      Parser_State : in out Packrat.Parser.Parser_State;
      R            : in     Token_ID;
      Last_Pos     : in     Syntax_Trees.Stream_Index)
     return Memo_Entry
   is
      use all type WisiToken.Syntax_Trees.Stream_Index;
      use all type WisiToken.Syntax_Trees.Node_Index;

      Tree       : Syntax_Trees.Tree renames Parser.Tree;
      Descriptor : WisiToken.Descriptor renames Tree.Lexer.Descriptor.all;
      Trace      : WisiToken.Trace'Class renames Parser.Tree.Lexer.Trace.all;

      Pos       : Syntax_Trees.Stream_Index          := Last_Pos; --  last token parsed.
      Start_Pos : constant Syntax_Trees.Stream_Index := Tree.Stream_Next
        (Tree.Shared_Stream, Last_Pos);                         --  first token in current nonterm
      Memo      : Memo_Entry                         := Get_Deriv
        (Parser_State.Derivs, R, Tree.Get_Node_Index (Tree.Shared_Stream, Start_Pos));

      Pos_Recurse_Last : Syntax_Trees.Stream_Index := Last_Pos;
      Result_Recurse   : Memo_Entry;
   begin
      case Memo.State is
      when Success | Failure =>
         if Trace_Parse > Extra then
            Trace.Put_Line ("apply memo:" & Image (Memo, R, Start_Pos, Tree));
         end if;
         return Memo;

      when No_Result =>
         if Parser.Direct_Left_Recursive (R) /= null then
            Set_Deriv
              (Parser_State.Derivs, R, Tree.Get_Node_Index (Tree.Shared_Stream, Start_Pos),
               (Failure, Last_Pos));
         else
            Memo := Eval (Parser, Parser_State, R, Last_Pos);

            if Trace_Parse > Extra then
               Trace.Put_Line ("apply memo:" & Image (Memo, R, Start_Pos, Tree));

            elsif Trace_Parse > Detail and Memo.State = Success then
               --  Match LR parse trace detail
               Trace_Deriv_Success (Parser, Start_Pos, Memo);
            end if;
            Set_Deriv (Parser_State.Derivs, R, Tree.Get_Node_Index (Tree.Shared_Stream, Start_Pos), Memo);
            return Memo;
         end if;
      end case;

      if Trace_Parse > Extra then
         Trace.Put_Line ("apply recursive " & Image (R, Descriptor));
      end if;

      loop
         --  Production is like: list : list element | element
         --
         --  Each time around this loop starts at the same point, but
         --  accumulates more tokens in the first 'list'; it exits when
         --  'element' does not match the remaining input.
         Pos := Last_Pos;

         Result_Recurse := Eval (Parser, Parser_State, R, Pos);

         if Result_Recurse.State = Success then
            if Tree.Get_Node_Index (Tree.Shared_Stream, Result_Recurse.Last_Pos) >
              Tree.Get_Node_Index (Tree.Shared_Stream, Pos_Recurse_Last)
            then
               Set_Deriv (Parser_State.Derivs, R, Tree.Get_Node_Index (Tree.Shared_Stream, Start_Pos), Result_Recurse);
               Pos              := Result_Recurse.Last_Pos;
               Pos_Recurse_Last := Pos;

               if WisiToken.Trace_Parse > Detail then
                  Trace_Deriv_Success (Parser, Start_Pos, Result_Recurse);
                  if Trace_Parse > Extra then
                     Trace.Put_Line ("apply recursive continue");
                  end if;
               end if;
               --  continue looping

            elsif Result_Recurse.Last_Pos = Pos_Recurse_Last then
               if Parser.Tree.Is_Empty_Nonterm (Result_Recurse.Result) then
                  Set_Deriv
                    (Parser_State.Derivs, R, Tree.Get_Node_Index (Tree.Shared_Stream, Start_Pos), Result_Recurse);
               end if;
               exit;
            else
               --  Result_Recurse.Last_Pos < Pos_Recurse_Last
               exit;
            end if;
         else
            exit;
         end if;
      end loop;

      declare
         Result : Memo_Entry renames Parser_State.Derivs (R)(Tree.Get_Node_Index (Tree.Shared_Stream, Start_Pos));
      begin
         Result.Max_Examined_Pos := Result_Recurse.Max_Examined_Pos;
         if Trace_Parse > Extra then
            Trace.Put_Line
              ("apply recursive " & Image (R, Descriptor) & " end: " & Image (Result, R, Pos, Tree));
         end if;

         return Result;
      end;
   end Apply_Rule;

   ----------
   --  Public subprograms

   function Create
     (Grammar               : in WisiToken.Productions.Prod_Arrays.Vector;
      Direct_Left_Recursive : in Token_ID_Array_Token_ID_Set_Access;
      Start_ID              : in Token_ID;
      Lexer                 : in WisiToken.Lexer.Handle;
      Productions           : in WisiToken.Syntax_Trees.Production_Info_Trees.Vector;
      User_Data             : in WisiToken.Syntax_Trees.User_Data_Access)
     return Procedural.Parser
   is begin
      return Parser                   : Procedural.Parser (Grammar.First_Index, Grammar.Last_Index) do
         Parser.Tree.Lexer            := Lexer;
         Parser.Productions           := Productions;
         Parser.User_Data             := User_Data;
         Parser.Grammar               := Grammar;
         Parser.Start_ID              := Start_ID;
         Parser.Direct_Left_Recursive := Direct_Left_Recursive;
      end return;
   end Create;

   overriding
   procedure Packrat_Parse_No_Recover
     (Parser : in out Procedural.Parser;
      Resume : in     Boolean)
   is
      use all type Packrat.Parser.Parser_Label;
      use all type WisiToken.Syntax_Trees.User_Data_Access;
      Trace      : WisiToken.Trace'Class renames Parser.Tree.Lexer.Trace.all;
      Descriptor : WisiToken.Descriptor renames Parser.Tree.Lexer.Descriptor.all;

   begin
      if not Resume then
         if Trace_Time then
            Trace.Put_Clock ("start");
         end if;

         for Parser_State of Parser.Packrat_Parsers loop
            Clear (Parser_State.Derivs);
            Parser_State.Result := No_Result_Memo;
         end loop;
         Parser.Tree.Clear;
         Parser.Packrat_Parsers.Finalize;

         if Parser.User_Data /= null then
            Parser.User_Data.Reset;
         end if;
         Parser.Lex_All;

         Parser.Next_Packrat_Label := Packrat.Parser.Invalid_Parser_Label + 1;
         Parser.Packrat_Parsers.Append
           (Packrat.Parser.Parser_State'
              (First_Nonterminal => Descriptor.First_Nonterminal,
               Last_Nonterminal  => Descriptor.Last_Nonterminal,
               Packrat_Label     => Parser.Next_Packrat_Label,
               others            => <>));
         Parser.Next_Packrat_Label := @ + 1;
      end if;

      for Parser_State of Parser.Packrat_Parsers loop
         if Trace_Packrat_McKenzie > Outline then
            Parser.Tree.Lexer.Trace.New_Line;
            Parser.Tree.Lexer.Trace.Put_Line ("packrat parser" & Parser_State.Packrat_Label'Image);
         end if;

         Parser_State.Result := Apply_Rule
           (Parser, Parser_State, Parser.Start_ID,
            Parser.Tree.Stream_First (Parser.Tree.Shared_Stream, Skip_SOI => False));
      end loop;

      Parser.Finish_Parse;

   end Packrat_Parse_No_Recover;

end WisiToken.Parse.Packrat.Procedural;
