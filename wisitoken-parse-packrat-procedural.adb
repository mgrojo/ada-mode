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

package body WisiToken.Parse.Packrat.Procedural is

   function Apply_Rule
     (Parser   : in out Procedural.Parser;
      R        : in     Token_ID;
      Last_Pos : in     Syntax_Trees.Stream_Index)
     return Memo_Entry
   with Post => Apply_Rule'Result.State in Failure .. Success;

   function Eval
     (Parser   : in out Procedural.Parser;
      R        : in     Token_ID;
      Last_Pos : in     Syntax_Trees.Stream_Index)
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
     (Parser   : in out Procedural.Parser;
      R        : in     Token_ID;
      Last_Pos : in     Syntax_Trees.Stream_Index)
     return Memo_Entry
   is
      use all type WisiToken.Syntax_Trees.Stream_Index;

      Tree       : Syntax_Trees.Tree renames Parser.Tree;
      Descriptor : WisiToken.Descriptor renames Tree.Lexer.Descriptor.all;

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
      for RHS_Index in Parser.Grammar (R).RHSs.First_Index .. Parser.Grammar (R).RHSs.Last_Index loop
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
                     Parser.Tree.Lexer.Trace.Put_Line ("eval: " & Image (Result, R, Pos, Tree));
                  end if;
               end return;
            else
               declare
                  use all type WisiToken.Syntax_Trees.Node_Access;
                  Children : Syntax_Trees.Node_Access_Array
                    (SAL.Base_Peek_Type (RHS.Tokens.First_Index) .. SAL.Base_Peek_Type (RHS.Tokens.Last_Index));
               begin
                  for I in RHS.Tokens.First_Index .. RHS.Tokens.Last_Index loop
                     if RHS.Tokens (I) in Terminal then
                        if Next_Pos = Syntax_Trees.Invalid_Stream_Index then
                           --  We don't update Max_Examined_Pos here; it must already be EOI
                           pragma Assert (Tree.Get_Node (Tree.Shared_Stream, Max_Examined_Pos) = Tree.EOI);
                           goto Fail_RHS;

                        elsif Tree.ID (Next_Pos) = RHS.Tokens (I) then
                           Pos := Next_Pos;
                           Next_Pos := Tree.Stream_Next (Tree.Shared_Stream, Pos);
                           Children (SAL.Base_Peek_Type (I)) := Tree.Get_Node (Tree.Shared_Stream, Pos);
                        else
                           Update_Max_Examined_Pos (Next_Pos);
                           goto Fail_RHS;
                        end if;
                     else
                        Memo := Apply_Rule (Parser, RHS.Tokens (I), Pos);
                        Update_Max_Examined_Pos (Memo.Max_Examined_Pos);

                        case Memo.State is
                        when Success =>
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
                        Parser.Tree.Lexer.Trace.Put_Line ("eval: " & Image (Result, R, Pos, Tree));
                     end if;
                  end return;

                  <<Fail_RHS>>
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
     (Parser   : in out Procedural.Parser;
      R        : in     Token_ID;
      Last_Pos : in     Syntax_Trees.Stream_Index)
     return Memo_Entry
   is
      use all type WisiToken.Syntax_Trees.Stream_Index;
      use all type WisiToken.Syntax_Trees.Node_Index;

      Tree       : Syntax_Trees.Tree renames Parser.Tree;
      Descriptor : WisiToken.Descriptor renames Tree.Lexer.Descriptor.all;

      Pos       : Syntax_Trees.Stream_Index          := Last_Pos; --  last token parsed.
      Start_Pos : constant Syntax_Trees.Stream_Index := Tree.Stream_Next
        (Tree.Shared_Stream, Last_Pos);                         --  first token in current nonterm
      Memo      : Memo_Entry                         := Parser.Derivs (R)
        (Tree.Get_Node_Index (Tree.Shared_Stream, Start_Pos));

      Pos_Recurse_Last : Syntax_Trees.Stream_Index := Last_Pos;
      Result_Recurse   : Memo_Entry;
   begin
      case Memo.State is
      when Success | Failure =>
         if Trace_Parse > Extra then
            Parser.Tree.Lexer.Trace.Put_Line ("apply memo:" & Image (Memo, R, Start_Pos, Tree));
         end if;
         return Memo;

      when No_Result =>
         if Parser.Direct_Left_Recursive (R) /= null then
            Parser.Derivs (R).Replace_Element
              (Tree.Get_Node_Index (Tree.Shared_Stream, Start_Pos), (Failure, Last_Pos));
         else
            Memo := Eval (Parser, R, Last_Pos);

            if Trace_Parse > Extra then
               Parser.Tree.Lexer.Trace.Put_Line ("apply memo:" & Image (Memo, R, Start_Pos, Tree));

            elsif Trace_Parse > Detail and Memo.State = Success then
               --  Match LR parse trace detail
               Trace_Deriv_Success (Parser, Start_Pos, Memo);
            end if;
            Parser.Derivs (R).Replace_Element (Tree.Get_Node_Index (Tree.Shared_Stream, Start_Pos), Memo);
            return Memo;
         end if;
      end case;

      if Trace_Parse > Extra then
         Parser.Tree.Lexer.Trace.Put_Line ("apply recursive " & Image (R, Descriptor));
      end if;

      loop
         --  Production is like: list : list element | element
         --
         --  Each time around this loop starts at the same point, but
         --  accumulates more tokens in the first 'list'; it exits when
         --  'element' does not match the remaining input.
         Pos := Last_Pos;

         Result_Recurse := Eval (Parser, R, Pos);

         if Result_Recurse.State = Success then
            if Tree.Get_Node_Index (Tree.Shared_Stream, Result_Recurse.Last_Pos) >
              Tree.Get_Node_Index (Tree.Shared_Stream, Pos_Recurse_Last)
            then
               Parser.Derivs (R).Replace_Element
                 (Tree.Get_Node_Index (Tree.Shared_Stream, Start_Pos), Result_Recurse);
               Pos              := Result_Recurse.Last_Pos;
               Pos_Recurse_Last := Pos;

               if WisiToken.Trace_Parse > Detail then
                  Trace_Deriv_Success (Parser, Start_Pos, Result_Recurse);
                  if Trace_Parse > Extra then
                     Parser.Tree.Lexer.Trace.Put_Line ("apply recursive continue");
                  end if;
               end if;
               --  continue looping

            elsif Result_Recurse.Last_Pos = Pos_Recurse_Last then
               if Parser.Tree.Is_Empty_Nonterm (Result_Recurse.Result) then
                  Parser.Derivs (R).Replace_Element
                    (Tree.Get_Node_Index (Tree.Shared_Stream, Start_Pos), Result_Recurse);
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
         Result : Memo_Entry renames Parser.Derivs (R)(Tree.Get_Node_Index (Tree.Shared_Stream, Start_Pos));
      begin
         Result.Max_Examined_Pos := Result_Recurse.Max_Examined_Pos;
         if Trace_Parse > Extra then
            Parser.Tree.Lexer.Trace.Put_Line
              ("apply recursive " & Image (R, Descriptor) & " end: " & Image (Result, R, Pos, Tree));
         end if;

         return Result;
      end;
   end Apply_Rule;

   procedure Parse_Init (Parser : in out Procedural.Parser)
   is
      use all type WisiToken.Syntax_Trees.User_Data_Access;
      Descriptor : WisiToken.Descriptor renames Parser.Tree.Lexer.Descriptor.all;
   begin
      Clear (Parser.Derivs);

      Parser.Tree.Clear;

      if Parser.User_Data /= null then
         Parser.User_Data.Reset;
      end if;
      Parser.Lex_All;

      for Nonterm in Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal loop
         Parser.Derivs (Nonterm).Set_First_Last
           (Parser.Tree.Get_Node_Index
              (Parser.Tree.Shared_Stream, Parser.Tree.Stream_First (Parser.Tree.Shared_Stream, Skip_SOI => True)),
            Parser.Tree.Get_Node_Index
              (Parser.Tree.Shared_Stream, Parser.Tree.Stream_Last (Parser.Tree.Shared_Stream, Skip_EOI => False)));
      end loop;
   end Parse_Init;

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
      Trace : WisiToken.Trace'Class renames Parser.Tree.Lexer.Trace.all;

      Result : Memo_Entry;
   begin
      if not Resume then
         if Trace_Time then
            Trace.Put_Clock ("start");
         end if;

         Parse_Init (Parser);
      end if;

      Result := Apply_Rule
        (Parser, Parser.Start_ID,  Parser.Tree.Stream_First (Parser.Tree.Shared_Stream, Skip_SOI => False));

      Finish_Parse (Parser, Result);

   end Packrat_Parse_No_Recover;

end WisiToken.Parse.Packrat.Procedural;
