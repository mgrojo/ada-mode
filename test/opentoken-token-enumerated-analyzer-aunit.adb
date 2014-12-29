--  Abstract :
--
--  See spec
--
--  Copyright (C) 2009, 2014 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.

with AUnit.Assertions;
package body OpenToken.Token.Enumerated.Analyzer.AUnit is

   procedure Check
     (Label        : in String;
      Analyzer     : in Handle;
      Last_Token   : in Token_ID;
      Tail_Null    : in Boolean          := False;
      Tail_Tokens  : in Token_Array      := Null_Tokens;
      Tail_Lexemes : in Lexeme_Array     := Null_Lexemes;
      Queue_Null   : in Boolean          := False;
      Queue_Token  : in Token_ID         := Token_ID'First;
      Head_Null    : in Boolean          := False;
      Head_Token   : in Token_ID         := Token_ID'First;
      Check_Token  : in Check_Token_Proc := null)
   is
      use Standard.AUnit.Assertions;
      Prev : Token_List_Node_Pointer := Analyzer.Lookahead_Tail;
   begin
      Check_Token_ID (Label & ".last", Analyzer.Last_Token_ID, Last_Token);
      Check_Token_ID (Label & ".syntax_list", Analyzer.Syntax_List (Last_Token).Token_Handle.ID, Last_Token);

      if Tail_Null then
         Assert (Analyzer.Lookahead_Tail = null, Label & ".tail not null");
      else
         for I in reverse Tail_Tokens'Range loop
            Assert (Prev /= null, Label & ".tail actual length =" & Integer'Image (Tail_Tokens'Last - I));

            Check_Token_ID (Label & ".tail" & Integer'Image (I), Prev.Token_Handle.ID, Tail_Tokens (I));

            if Check_Token /= null then
               Check_Token (Label & ".tail token" & Integer'Image (I), Prev.Token_Handle, Tail_Lexemes (I));
            end if;

            Prev := Prev.Prev;
         end loop;
      end if;
      Assert (Prev = null, Label & ".tail longer than expected");

      if Queue_Null then
         Assert (Analyzer.Lookahead_Queue = null, Label & ".queue not null");
      else
         Assert (Analyzer.Lookahead_Queue /= null, Label & ".queue is null");
         Check_Token_ID (Label & ".queue", Analyzer.Lookahead_Queue.Token_Handle.ID, Queue_Token);
      end if;

      if Head_Null then
         Assert (Analyzer.Lookahead_Head = null, Label & ".head not null");
      else
         Assert (Analyzer.Lookahead_Head /= null, Label & ".head is null");
         Check_Token_ID (Label & ".head", Analyzer.Lookahead_Head.Token_Handle.ID, Head_Token);
      end if;

   end Check;

end OpenToken.Token.Enumerated.Analyzer.AUnit;
