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

package body WisiToken.Parse.Packrat.Generated is

   overriding procedure Parse
     (Parser     : in out Generated.Parser;
      Log_File   : in     Ada.Text_IO.File_Type;
      Edits      : in     KMN_Lists.List := KMN_Lists.Empty_List;
      Pre_Edited : in     Boolean        := False)
   is
      pragma Unreferenced (Log_File, Pre_Edited);
      use all type WisiToken.Syntax_Trees.User_Data_Access;
      use all type Ada.Containers.Count_Type;
      Descriptor : WisiToken.Descriptor renames Parser.Tree.Lexer.Descriptor.all;

      Result : Memo_Entry;
   begin
      if Edits.Length > 0 then
         raise WisiToken.Parse_Error;
      end if;

      for Deriv of Parser.Derivs loop
         for Memo of Deriv loop
            case Memo.State is
            when No_Result | Failure =>
               null;
            when Success =>
               Memo.Last_Pos := Syntax_Trees.Invalid_Stream_Index;
            end case;
         end loop;
      end loop;

      Parser.Tree.Clear;

      if Parser.User_Data /= null then
         Parser.User_Data.Reset;
      end if;
      Parser.Lex_All; -- Creates Tree.Shared_Stream

      --  FIXME: ref_count fails in this usage; works in procedural.
      Parser.Tree.Enable_Ref_Count_Check (Parser.Tree.Shared_Stream, Enable => False);

      for Nonterm in Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal loop
         Parser.Derivs (Nonterm).Clear (Free_Memory => True);
         Parser.Derivs (Nonterm).Set_First_Last
           (Parser.Tree.Get_Node_Index
              (Parser.Tree.Shared_Stream, Parser.Tree.Stream_First (Parser.Tree.Shared_Stream, Skip_SOI => True)),
            Parser.Tree.Get_Node_Index
              (Parser.Tree.Shared_Stream, Parser.Tree.Stream_Last (Parser.Tree.Shared_Stream, Skip_EOI => False)));
      end loop;

      Result := Parser.Parse_WisiToken_Accept
        (Parser, Parser.Tree.Stream_First (Parser.Tree.Shared_Stream, Skip_SOI => False));

      --  Clear copies of Stream_Index so Clear_Parse_Streams can run.
      for Nonterm in Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal loop
         Parser.Derivs (Nonterm).Clear (Free_Memory => True);
      end loop;

      declare
         use WisiToken.Syntax_Trees;

         Max_Examined_Node : constant Valid_Node_Access :=
           (case Result.State is
            when No_Result =>
               Parser.Tree.SOI,
            when Packrat.Success =>
               Parser.Tree.EOI,
            when Failure =>
              (if Result.Max_Examined_Pos = Invalid_Stream_Index
               then Parser.Tree.EOI
               else Parser.Tree.Get_Node (Parser.Tree.Shared_Stream, Result.Max_Examined_Pos)));

      begin
         if Result.State = Packrat.Success then
            --  Do this before Clear_Parse_Streams so the root node is not deleted.
            Parser.Tree.Set_Root (Result.Result);
            Result := (No_Result, False);
            Parser.Tree.Clear_Parse_Streams; -- also frees excess tree nodes created by backtracking.

         else
            if Trace_Parse > Outline then
               Parser.Tree.Lexer.Trace.Put_Line ("parse failed");
            end if;

            raise Syntax_Error with Parser.Tree.Error_Message (Max_Examined_Node, "parse failed");
            --  FIXME packrat: add "expecting: ..." based on last nonterm?
         end if;
      end;
   end Parse;

end WisiToken.Parse.Packrat.Generated;
