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

package body WisiToken.Parse.Packrat.Generated is

   overriding procedure Parse
     (Parser : in out Generated.Parser;
      Edits  : in     KMN_Lists.List := KMN_Lists.Empty_List)
   is
      use all type WisiToken.Syntax_Trees.User_Data_Access;
      use all type Ada.Containers.Count_Type;
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

      Parser.Derivs.Set_First_Last (Descriptor.First_Nonterminal, Descriptor.Last_Nonterminal);

      for Nonterm in Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal loop
         Parser.Derivs (Nonterm).Clear (Free_Memory => True);
         Parser.Derivs (Nonterm).Set_First_Last
           (Parser.Tree.Get_Node_Index
              (Parser.Tree.Shared_Stream, Parser.Tree.Stream_First (Parser.Tree.Shared_Stream)),
            Parser.Tree.Get_Node_Index
              (Parser.Tree.Shared_Stream, Parser.Tree.Stream_Last (Parser.Tree.Shared_Stream)));
      end loop;

      Result := Parser.Parse_WisiToken_Accept (Parser, Syntax_Trees.Invalid_Stream_Index);

      if Result.State /= Success then
         if Trace_Parse > Outline then
            Parser.Trace.Put_Line ("parse failed");
         end if;

         raise Syntax_Error with "parse failed"; --  FIXME: need better error message!
      else
         Parser.Tree.Set_Root (Result.Result);
      end if;

   end Parse;

   overriding function Any_Errors (Parser : in Generated.Parser) return Boolean
   is
      use all type Ada.Containers.Count_Type;
   begin
      return Parser.Lexer.Errors.Length > 0;
   end Any_Errors;

   overriding procedure Put_Errors (Parser : in Generated.Parser)
   is
      use Ada.Text_IO;
   begin
      for Item of Parser.Lexer.Errors loop
         Put_Line
           (Current_Error,
            Parser.Lexer.File_Name & ":0:0: lexer unrecognized character at" & Buffer_Pos'Image (Item.Char_Pos));
      end loop;

      --  FIXME: Packrat parser does not report errors yet.
   end Put_Errors;

end WisiToken.Parse.Packrat.Generated;
