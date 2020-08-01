--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018 - 2020 Free Software Foundation, Inc.
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

package body WisiToken.Parse is

   function Next_Grammar_Token (Parser : in out Base_Parser'Class) return Token_ID
   is
      use all type Ada.Containers.Count_Type;
      use Syntax_Trees;

      Token : Base_Token;
      Error : Boolean;
      pragma Unreferenced (Error); --  FIXME: delete Error
   begin
      loop
         Error := Parser.Lexer.Find_Next (Token);

         --  We don't handle Error until later; we assume it was recovered. We
         --  also assume Token is a grammar token if Error is True.

         if Trace_Parse > Lexer_Debug then
            Parser.Trace.Put_Line (Image (Token, Parser.Trace.Descriptor.all));
         end if;

         if Token.ID >= Parser.Trace.Descriptor.First_Terminal then

            declare
               Index : constant Stream_Index := Parser.Tree.Add_Terminal (Token);
            begin
               Parser.Last_Grammar_Node := Parser.Tree.Get_Node (Index);
            end;

            if Parser.User_Data /= null then
               Parser.User_Data.Lexer_To_Augmented (Parser.Tree, Parser.Last_Grammar_Node);
            end if;

            if Token.Line /= Invalid_Line_Number then
               --  Some lexers don't support line numbers.
               if Parser.Lexer.First then
                  if Parser.Line_Begin_Token.Length = 0 then
                     Parser.Line_Begin_Token.Set_First_Last (Token.Line, Token.Line);
                  else
                     Parser.Line_Begin_Token.Set_First_Last (Parser.Line_Begin_Token.First_Index, Token.Line);
                  end if;
                  Parser.Line_Begin_Token (Token.Line) := Parser.Last_Grammar_Node;

               elsif Token.ID = Parser.Trace.Descriptor.EOI_ID then
                  Parser.Line_Begin_Token.Set_First_Last (Parser.Line_Begin_Token.First_Index, Token.Line + 1);
                  Parser.Line_Begin_Token (Token.Line + 1) := Parser.Last_Grammar_Node;
               end if;
            end if;

            exit;
         else
            --  non-grammar
            if Parser.Last_Grammar_Node = Invalid_Node_Access then
               Parser.Tree.Leading_Non_Grammar.Append (Token);
            else
               declare
                  Containing : Base_Token_Arrays_Var_Ref renames Parser.Tree.Non_Grammar (Parser.Last_Grammar_Node);
               begin
                  Containing.Append (Token);
               end;
               if Parser.User_Data /= null then
                  Parser.User_Data.Lexer_To_Augmented (Parser.Tree, Token, Parser.Last_Grammar_Node);
               end if;
            end if;
         end if;
      end loop;

      return Token.ID;
   end Next_Grammar_Token;

   procedure Lex_All (Parser : in out Base_Parser'Class)
   is
      EOF_ID : constant Token_ID := Parser.Trace.Descriptor.EOI_ID;
   begin
      Parser.Lexer.Errors.Clear;
      Parser.Line_Begin_Token.Clear;
      Parser.Last_Grammar_Node := WisiToken.Syntax_Trees.Invalid_Node_Access;

      loop
         exit when EOF_ID = Next_Grammar_Token (Parser);
      end loop;
      if Trace_Parse > Outline then
         Parser.Trace.Put_Line (Syntax_Trees.Get_Node_Index (Parser.Last_Grammar_Node)'Image & " tokens lexed");
      end if;

   end Lex_All;

end WisiToken.Parse;
