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

   procedure Packrat_Parse_No_Recover (Parser : in out Generated.Parser)
   is
      use all type WisiToken.Syntax_Trees.User_Data_Access;
      Descriptor : WisiToken.Descriptor renames Parser.Tree.Lexer.Descriptor.all;
      Trace      : WisiToken.Trace'Class renames Parser.Tree.Lexer.Trace.all;

      Result : Memo_Entry;
   begin
      if Trace_Time then
         Trace.Put_Clock ("start");
      end if;

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

      --  FIXME: debugging error message
      if Result.State /= Packrat.Success then
         if Trace_Parse > Detail then
            Trace.New_Line;
            Trace.Put_Line
              ("max_examined_pos: " & Parser.Tree.Image
                 (Parser.Tree.Get_Node (Parser.Tree.Shared_Stream, Result.Max_Examined_Pos), Node_Numbers => True));
            Trace.Put_Line ("derivs:");
            Parser.Print_Derivs;
         end if;
      end if;
      Finish_Parse (Parser, Result);

   end Packrat_Parse_No_Recover;

end WisiToken.Parse.Packrat.Generated;
