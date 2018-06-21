--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018 Stephen Leake All Rights Reserved.
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

   function Next_Grammar_Token
     (Terminals        : in out          Base_Token_Arrays.Vector;
      Line_Begin_Token : in out          Line_Begin_Token_Vectors.Vector;
      Descriptor       : in              WisiToken.Descriptor'Class;
      Lexer            : not null access WisiToken.Lexer.Instance'Class;
      User_Data        : in              WisiToken.Syntax_Trees.User_Data_Access)
     return Token_ID
   is
      use all type Syntax_Trees.User_Data_Access;

      Token : Base_Token;
      Error : Boolean;
   begin
      loop
         Error := Lexer.Find_Next (Token);

         if User_Data /= null then
            User_Data.Lexer_To_Augmented (Token, Lexer);
         end if;

         if Token.Line /= Invalid_Line_Number then
            --  Some lexers don't support line numbers.
            if Lexer.First then
               Line_Begin_Token.Set_Length (Ada.Containers.Count_Type (Token.Line));
               Line_Begin_Token (Token.Line) := Terminals.Last_Index +
                 (if Token.ID >= Descriptor.First_Terminal then 1 else 0);

            elsif Token.ID = Descriptor.EOF_ID then
               Line_Begin_Token.Set_Length (Ada.Containers.Count_Type (Token.Line + 1));
               Line_Begin_Token (Token.Line + 1) := Terminals.Last_Index + 1;
            end if;
         end if;

         exit when Token.ID >= Descriptor.First_Terminal;
      end loop;
      Terminals.Append (Token);

      if Error then
         declare
            Error : WisiToken.Lexer.Error renames Lexer.Errors.Reference (Lexer.Errors.Last);
         begin
            if Error.Recover_Char (1) /= ASCII.NUL then
               Error.Recover_Token := Terminals.Last_Index;
            end if;
         end;
      end if;

      return Token.ID;
   end Next_Grammar_Token;

   procedure Lex_All
     (Lexer            : in     WisiToken.Lexer.Handle;
      Terminals        : in out Base_Token_Arrays.Vector;
      Line_Begin_Token : in out Line_Begin_Token_Vectors.Vector;
      User_Data        :        WisiToken.Syntax_Trees.User_Data_Access;
      Trace            : access WisiToken.Trace'Class)
   is begin
      Lexer.Errors.Clear;
      Terminals.Clear;
      Line_Begin_Token.Clear;
      loop
         exit when Trace.Descriptor.EOF_ID = Next_Grammar_Token
           (Terminals, Line_Begin_Token, Trace.Descriptor.all, Lexer, User_Data);
      end loop;
      if Trace_Parse > Outline then
         Trace.Put_Line (Token_Index'Image (Terminals.Last_Index) & " tokens lexed");
      end if;

   end Lex_All;

end WisiToken.Parse;
