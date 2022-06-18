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

   overriding
   procedure Packrat_Parse_No_Recover
     (Parser : in out Generated.Parser;
      Resume : in     Boolean)
   is
      use all type WisiToken.Parse.Packrat.Parser.Parser_Label;
      use all type WisiToken.Syntax_Trees.User_Data_Access;
      Descriptor : WisiToken.Descriptor renames Parser.Tree.Lexer.Descriptor.all;
      Trace      : WisiToken.Trace'Class renames Parser.Tree.Lexer.Trace.all;
   begin
      if Resume then raise SAL.Not_Implemented; end if;

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
      Parser.Lex_All; -- Creates Tree.Shared_Stream

      --  FIXME: ref_count fails in this usage; works in procedural.
      Parser.Tree.Enable_Ref_Count_Check (Parser.Tree.Shared_Stream, Enable => False);

      Parser.Next_Packrat_Label := Packrat.Parser.Invalid_Parser_Label + 1;
      Parser.Packrat_Parsers.Append
        (Packrat.Parser.Parser_State'
           (First_Nonterminal => Descriptor.First_Nonterminal,
            Last_Nonterminal  => Descriptor.Last_Nonterminal,
            Packrat_Label     => Parser.Next_Packrat_Label,
            others            => <>));
      Parser.Next_Packrat_Label := @ + 1;

      for Parser_State of Parser.Packrat_Parsers loop
         if Trace_Packrat_McKenzie > Outline then
            Parser.Tree.Lexer.Trace.New_Line;
            Parser.Tree.Lexer.Trace.Put_Line ("packrat parser" & Parser_State.Packrat_Label'Image);
         end if;

         Parser_State.Result := Parser.Parse_WisiToken_Accept
           (Parser, Parser_State, Parser.Tree.Stream_First (Parser.Tree.Shared_Stream, Skip_SOI => False));
      end loop;

      Parser.Finish_Parse;

   end Packrat_Parse_No_Recover;

end WisiToken.Parse.Packrat.Generated;
