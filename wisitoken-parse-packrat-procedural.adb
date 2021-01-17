--  Abstract :
--
--  See spec.
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

package body WisiToken.Parse.Packrat.Procedural is

   function Apply_Rule
     (Parser   : in out Procedural.Parser;
      R        : in     Token_ID;
      Last_Pos : in     Syntax_Trees.Stream_Index)
     return Memo_Entry
   with Post => Apply_Rule'Result.State in Failure .. Success;

   function Eval
     (Parser    : in out Procedural.Parser;
      R         : in     Token_ID;
      Last_Pos  : in     Syntax_Trees.Stream_Index;
      Recursing : in     Boolean)
     return Memo_Entry
   with Post => Eval'Result.State in Failure .. Success;

   ----------
   --  bodies

   function Eval
     (Parser    : in out Procedural.Parser;
      R         : in     Token_ID;
      Last_Pos  : in     Syntax_Trees.Stream_Index;
      Recursing : in     Boolean)
     return Memo_Entry
   is
      use all type WisiToken.Syntax_Trees.Stream_Index;

      Descriptor : WisiToken.Descriptor renames Parser.Descriptor.all;
      Tree       : Syntax_Trees.Tree renames Parser.Tree;

      subtype Terminal is Token_ID range Descriptor.First_Terminal .. Descriptor.Last_Terminal;

      Pos      : Syntax_Trees.Stream_Index := Last_Pos; --  last token parsed.
      Next_Pos : Syntax_Trees.Stream_Index := Tree.Stream_Next (Tree.Shared_Stream, Pos);
   begin
      for RHS_Index in Parser.Grammar (R).RHSs.First_Index .. Parser.Grammar (R).RHSs.Last_Index loop
         declare
            use all type Ada.Containers.Count_Type;
            RHS  : WisiToken.Productions.Right_Hand_Side renames Parser.Grammar (R).RHSs (RHS_Index);
            Memo : Memo_Entry; --  for temporary or intermediate results
         begin
            if RHS.Tokens.Length = 0 then
               return
                 (State            => Success,
                  Result           => Tree.Add_Nonterm
                    (Production    => (R, RHS_Index),
                     Action        => RHS.Post_Parse_Action,
                     Children      => (1 .. 0 => Syntax_Trees.Invalid_Node_Access),
                     Clear_Parents => False),
                  Last_Pos         => Pos);
            else
               declare
                  Children : Syntax_Trees.Node_Access_Array
                    (SAL.Base_Peek_Type (RHS.Tokens.First_Index) .. SAL.Base_Peek_Type (RHS.Tokens.Last_Index));
               begin
                  for I in RHS.Tokens.First_Index .. RHS.Tokens.Last_Index loop
                     if RHS.Tokens (I) in Terminal then
                        if Next_Pos = Syntax_Trees.Invalid_Stream_Index then
                           goto Fail_RHS;

                        elsif Tree.ID (Tree.Shared_Stream, Next_Pos) = RHS.Tokens (I) then
                           Pos := Next_Pos;
                           Next_Pos := Tree.Stream_Next (Tree.Shared_Stream, Pos);
                           Children (SAL.Base_Peek_Type (I)) := Tree.Get_Node (Tree.Shared_Stream, Pos);
                        else
                           goto Fail_RHS;
                        end if;
                     else
                        Memo := Apply_Rule (Parser, RHS.Tokens (I), Pos);
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
                    (State              => Success,
                     Result             => Parser.Tree.Add_Nonterm
                       (Production      => (R, RHS_Index),
                        Action          => RHS.Post_Parse_Action,
                        Children        => Syntax_Trees.To_Valid_Node_Access (Children),
                        Default_Virtual => False,
                        Clear_Parents   => True),
                     --  We must be able to steal nodes from failed nonterms;
                     --  body_instantiation_conflict.wy.
                     Last_Pos           => Pos)
                  do
                     if Trace_Parse > Extra then
                        Parser.Trace.Put_Line
                          ("eval: " & Parser.Tree.Image (Root => Result.Result, Children => True));
                     end if;
                  end return;

                  <<Fail_RHS>>
                  if Recursing then
                     --  Only try the first RHS to extend recursion. Otherwise creates
                     --  second syntax tree node containing first terminal, which deletes
                     --  the first terminal from the previously memoized result.
                     exit;
                  end if;
                  Pos := Last_Pos;
                  Next_Pos := Tree.Stream_Next (Tree.Shared_Stream, Pos);
               end;
            end if;
         end;
      end loop;
      --  get here when all RHSs fail

      return (State => Failure);
   end Eval;

   function Apply_Rule
     (Parser   : in out Procedural.Parser;
      R        : in     Token_ID;
      Last_Pos : in     Syntax_Trees.Stream_Index)
     return Memo_Entry
   is
      use all type WisiToken.Syntax_Trees.Stream_Index;
      use all type WisiToken.Syntax_Trees.Node_Index;

      Descriptor : WisiToken.Descriptor renames Parser.Descriptor.all;
      Tree       : Syntax_Trees.Tree renames Parser.Tree;

      Pos       : Syntax_Trees.Stream_Index          := Last_Pos; --  last token parsed.
      Start_Pos : constant Syntax_Trees.Stream_Index := Tree.Stream_Next
        (Tree.Shared_Stream, Last_Pos);                         --  first token in current nonterm
      Memo      : Memo_Entry                         := Parser.Derivs (R)
        (Tree.Get_Node_Index (Tree.Shared_Stream, Start_Pos));

      Pos_Recurse_Last : Syntax_Trees.Stream_Index := Last_Pos;
      Recursing        : Boolean                   := False;
      Result_Recurse   : Memo_Entry;
   begin
      case Memo.State is
      when Success =>
         return Memo;

      when Failure =>
         return (State => Failure);

      when No_Result =>
         if Parser.Direct_Left_Recursive (R) then
            Parser.Derivs (R).Replace_Element
              (Tree.Get_Node_Index (Tree.Shared_Stream, Start_Pos), (State => Failure));
         else
            Memo := Eval (Parser, R, Last_Pos, Recursing => False);

            if (Trace_Parse > Detail and Memo.State = Success) or Trace_Parse > Extra then
               case Memo.State is
               when Success =>
                  Parser.Trace.Put_Line
                    (Parser.Tree.Image (Memo.Result, Children => True, Terminal_Node_Numbers => True));
               when Failure =>
                  Parser.Trace.Put_Line
                    (Image (R, Descriptor) & " failed at pos " & Image_Pos (Tree, Tree.Shared_Stream, Last_Pos));
               when No_Result =>
                  raise SAL.Programmer_Error;
               end case;
            end if;
            Parser.Derivs (R).Replace_Element (Tree.Get_Node_Index (Tree.Shared_Stream, Start_Pos), Memo);
            return Memo;
         end if;
      end case;

      loop
         --  Production is like: list : list element | element
         --
         --  Each time around this loop starts at the same point, but
         --  accumulates more tokens in the first 'list'; it exits when
         --  'element' does not match the remaining input.
         Pos := Last_Pos;

         Result_Recurse := Eval (Parser, R, Pos, Recursing);

         if Result_Recurse.State = Success then
            if Tree.Get_Node_Index (Tree.Shared_Stream, Result_Recurse.Last_Pos) >
              Tree.Get_Node_Index (Tree.Shared_Stream, Pos_Recurse_Last)
            then
               Parser.Derivs (R).Replace_Element
                 (Tree.Get_Node_Index (Tree.Shared_Stream, Start_Pos), Result_Recurse);
               Pos              := Result_Recurse.Last_Pos;
               Pos_Recurse_Last := Pos;
               Recursing        := True;

               if WisiToken.Trace_Parse > Detail then
                  Parser.Trace.Put_Line
                    (Parser.Tree.Image
                       (Result_Recurse.Result, Children => True, Terminal_Node_Numbers => True));
               end if;
               --  continue looping

            elsif Result_Recurse.Last_Pos = Pos_Recurse_Last then
               if Parser.Tree.Buffer_Region_Is_Empty (Result_Recurse.Result) then
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
      return Parser.Derivs (R)(Tree.Get_Node_Index (Tree.Shared_Stream, Start_Pos));
   end Apply_Rule;

   ----------
   --  Public subprograms

   function Create
     (Grammar               : in     WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor            : in     WisiToken.Descriptor_Access_Constant;
      Direct_Left_Recursive : in     Token_ID_Set;
      Start_ID              : in     Token_ID;
      Trace                 : access WisiToken.Trace'Class;
      Lexer                 :        WisiToken.Lexer.Handle;
      User_Data             :        WisiToken.Syntax_Trees.User_Data_Access)
     return Procedural.Parser
   is begin
      return Parser : Procedural.Parser (Descriptor, Grammar.First_Index, Grammar.Last_Index) do
         Parser.Trace                 := Trace;
         Parser.Lexer                 := Lexer;
         Parser.User_Data             := User_Data;
         Parser.Grammar               := Grammar;
         Parser.Start_ID              := Start_ID;
         Parser.Direct_Left_Recursive := Direct_Left_Recursive;
      end return;
   end Create;

   overriding procedure Parse
     (Parser : in out Procedural.Parser;
      Edits  : in     KMN_Lists.List := KMN_Lists.Empty_List)
   is
      use all type Ada.Containers.Count_Type;
      use all type WisiToken.Syntax_Trees.User_Data_Access;

      Descriptor : WisiToken.Descriptor renames Parser.Descriptor.all;

      Result : Memo_Entry;
   begin
      if Edits.Length > 0 then
         raise Parse_Error;
      end if;

      Parser.Tree.Clear;
      --  Creates Shared_Stream, but no parse stream; packrat does not
      --  use a parse stream.

      if Parser.User_Data /= null then
         Parser.User_Data.Reset;
      end if;
      Parser.Wrapped_Lexer_Errors.Clear;
      Parser.Lex_All;

      for Nonterm in Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal loop
         Parser.Derivs (Nonterm).Clear (Free_Memory => True);
         Parser.Derivs (Nonterm).Set_First_Last
           (Parser.Tree.Get_Node_Index
              (Parser.Tree.Shared_Stream, Parser.Tree.Stream_First (Parser.Tree.Shared_Stream)),
            Parser.Tree.Get_Node_Index
              (Parser.Tree.Shared_Stream, Parser.Tree.Stream_Last (Parser.Tree.Shared_Stream)));
      end loop;

      Result := Apply_Rule (Parser, Parser.Start_ID, Syntax_Trees.Invalid_Stream_Index);

      if Result.State /= Success then
         if Trace_Parse > Outline then
            Parser.Trace.Put_Line ("parse failed");
         end if;

         raise Syntax_Error with "parse failed"; --  FIXME: need better error message!
      else
         Parser.Tree.Set_Root (Result.Result);
      end if;
   end Parse;

end WisiToken.Parse.Packrat.Procedural;
