--  Froma a real editing session. Used to encounter an Assertion_Error in recover.
--EMACSCMD:(setq skip-recase-test t)

package body Ada_Mode.Incremental_Recover_05 is

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
                        Children        => Syntax_Trees.To_Valid_Node_Access (Children),
                        Clear_Parents   => True),
                     --  We must be able to steal nodes from failed nonterms;
                     --  body_instantiation_conflict.wy.
                     Last_Pos           => Pos)
                  do
                     if Trace_Parse > Extra then
                        Parser.Tree.Lexer.Trace.Put_Line
                          ("eval: " & Parser.Tree.Image (Root => Result.Result, Children => True));
                     end if;
                  end return;

               <<Fail_RHS>>
                  if Tree.Byte_Region
                    (Tree.Get_Node (Tree.Shared_Stream, Next_Pos), Trailing_Non_Grammar => False).First >
                    Tree.Byte_Region
                      (Tree.Get_Node (Tree.Shared_Stream, Max_Examined_Pos), Trailing_Non_Grammar => False).First
                  then
                     Max_Examined_Pos := Next_Pos;
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
            return Memo;

         when No_Result =>
            if Parser.Direct_Left_Recursive (R) then
               Parser.Derivs (R).Replace_Element
                 (Tree.Get_Node_Index (Tree.Shared_Stream, Start_Pos), (Failure, Last_Pos));
            else
               Memo := Eval (Parser, R, Last_Pos);

               if (Trace_Parse > Detail and Memo.State = Success) or Trace_Parse > Extra then
                  case Memo.State is
                     when Success =>
                        Parser.Tree.Lexer.Trace.Put_Line
                          (Parser.Tree.Image
                             (Memo.Result, Children => True, Terminal_Node_Numbers => True, RHS_Index => True));
                     when Failure =>
                        Parser.Tree.Lexer.Trace.Put_Line
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
                  Parser.Tree.Lexer.Trace.Put_Line
                    (Parser.Tree.Image
                       (Result_Recurse.Result, Children => True, Terminal_Node_Numbers => True, RHS_Index => True));
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
      return Parser.Derivs (R)(Tree.Get_Node_Index (Tree.Shared_Stream, Start_Pos));
   end Apply_Rule;

   ----------
   --  Public subprograms

   function Create
     (Grammar               : in WisiToken.Productions.Prod_Arrays.Vector;
      Direct_Left_Recursive : in Token_ID_Set;
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

   overriding procedure Parse
     (Parser     : in out Procedural.Parser;
      Log_File   : in     Ada.Text_IO.File_Type;
      Edits      : in     KMN_Lists.List := KMN_Lists.Empty_List;
      Pre_Edited : in     Boolean        := False)
   is
      pragma Unreferenced (Log_File, Pre_Edited);
      use all type Ada.Containers.Count_Type;
      use all type WisiToken.Syntax_Trees.User_Data_Access;

      Descriptor : WisiToken.Descriptor renames Parser.Tree.Lexer.Descriptor.all;

      Result : Memo_Entry;
   begin
      if Edits.Length > 0 then
         raise WisiToken.Parse_Error;
      end if;

      Parser.Tree.Clear;
      --  Creates Shared_Stream, but no parse stream; packrat does not
      --  use a parse stream.

      if Parser.User_Data /= null then
         Parser.User_Data.Reset;
      end if;
      Parser.Lex_All;

      for Nonterm in Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal loop
         Parser.Derivs (Nonterm).Clear (Free_Memory => True);
         Parser.Derivs (Nonterm).Set_First_Last
           (Parser.Tree.Get_Node_Index
              (Parser.Tree.Shared_Stream, Parser.Tree.Stream_First (Parser.Tree.Shared_Stream, Skip_SOI => True)),
            Parser.Tree.Get_Node_Index
              (Parser.Tree.Shared_Stream, Parser.Tree.Stream_Last (Parser.Tree.Shared_Stream, Skip_EOI => False)));
      end loop;

      Result := Apply_Rule
        (Parser, Parser.Start_ID,  Parser.Tree.Stream_First (Parser.Tree.Shared_Stream, Skip_SOI => False));

      --  Clear copies of Stream_Index so Clear_Parse_Streams can run.
      for Nonterm in Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal loop
         Parser.Derivs (Nonterm).Clear (Free_Memory => True);
      end loop;

      declare
         use WisiToken.Syntax_Trees;
         Success : constant Boolean := Result.State = Packrat.Success;

         --EMACSCMD:(progn (forward-line 3)(insert "(case Result.State is")(wisi-indent-newline-indent))
         --EMACSCMD:(progn (forward-line 3)(insert "Parser.Tree.SOI")(wisi-indent-newline-indent))
         Max_Examined_Node : constant Valid_Node_Access :=
           (if Result.Max_Examined_Pos = Invalid_Stream_Index
            then Parser.Tree.EOI
            else Parser.Tree.Get_Node (Parser.Tree.Shared_Stream, Result.Max_Examined_Pos));
         --EMACSCMD:(delete-region (line-beginning-position -4)(+ 5 (line-beginning-position -2)))

         Root_Node : constant Node_Access := (if Success then Result.Result else Invalid_Node_Access);
      begin
         Result := (No_Result, False);
         Parser.Tree.Clear_Parse_Streams; -- also frees excess tree nodes created by backtracking.

         if Success then
            Parser.Tree.Set_Root (Root_Node);
         else
            if Trace_Parse > Outline then
               Parser.Tree.Lexer.Trace.Put_Line ("parse failed");
            end if;

            raise Syntax_Error with Parser.Tree.Error_Message (Max_Examined_Node, "parse failed");
            --  FIXME packrat: add "expecting: ..." based on last nonterm?
         end if;
      end;
   end Parse;

end Ada_Mode.Incremental_Recover_05;
