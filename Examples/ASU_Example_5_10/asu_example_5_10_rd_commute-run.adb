-------------------------------------------------------------------------------
--
--  Copyright (C) 2009 Stephe Leake
--
--  This file is part of the OpenToken package.
--
--  The OpenToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The OpenToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the OpenToken package;
--  see file GPL.txt. If not, write to the Free Software Foundation,
--  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.
-------------------------------------------------------------------------------

with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
procedure ASU_Example_5_10_RD_Commute.Run is

   procedure Put_Usage
   is begin
      Put_Line ("asu_example_5_10_rd_commute-run [-t] [filename]");
      Put_Line ("  -t : output trace of parser generation, execution");
      Put_Line ("  if no filename given, parse standard input");
   end Put_Usage;

   Input_File : File_Type;

   procedure Use_File (File_Name : in String)
   is begin
      Open (Input_File, In_File, File_Name);
      Set_Input (Input_File);
   end Use_File;

   Line        : String (1 .. 1024);
   Line_Length : Natural;

   use OpenToken.Token.Sequence;
   use OpenToken.Token.Selection;
begin

   declare
      use Ada.Command_Line;
   begin
      case Argument_Count is
      when 0 =>
         null;

      when 1 =>
         if Argument (1) = "-t" then
            OpenToken.Trace_Parse := True;

         else
            Use_File (Argument (1));
         end if;

      when 2 =>
         if Argument (1) = "-t" then
            OpenToken.Trace_Parse := True;

         else
            Set_Exit_Status (Failure);
            Put_Usage;
            return;
         end if;

         Use_File (Argument (2));

      when others =>
         Set_Exit_Status (Failure);
         Put_Usage;
         return;
      end case;
   end;

   --  Token E must parse the closing paren, in order for the parent
   --  sequence to see the following operator. That means the operator
   --  tokens must also parse all of their input.
   --
   --  Note that this results in always looking ahead to the end of
   --  the input, for every step along the way.
   OpenToken.Token.Default_Lookahead := 3;

   --  We'd like to arrange the selection order so that integers are
   --  accepted as quickly as possible. But then the operators are not
   --  considered, and the parse fails.

   L.all := Copy (E & EOF + Build_Print'Access).all;

   E.all := Copy ((T & Plus & E + Build_Plus'Access and "T + T") / T + Build_Selection'Access and "E").all;

   T.all := Copy ((F & Times & T + Build_Multiply'Access and "F * F") / F + Build_Selection'Access and "T").all;

   F.all :=
     Copy (Int / (Left_Paren & E & Right_Paren + Build_Parens'Access and "( E )") + Build_Selection'Access and "F").all;

   Master_Token.Set_Build (Int.all, Build_Integer'Access);

   if not Is_Open (Input_File) then
      Put_Line ("A simple calculator, as specified in example 5.10 in Aho, Sethi, and Ullman's");
      Put_Line ("""Compilers Principles, Techniques and Tools""");
      New_Line;
      Put_Line ("""+"", ""*"", and ""( num )"" are understood.");
      Put_Line ("(Enter a blank line to quit)");
   end if;

   --  Exit on null line or end of file
   loop
      Get_Line (Line, Line_Length);

      exit when Line_Length = 0;

      OpenToken.Text_Feeder.String.Set
        (Feeder => Feeder,
         Value  => Line (1 .. Line_Length));

      Put_Line ("Input_String => " & Line (1 .. Line_Length));

      Tokenizer.Reset (Analyzer);

      Tokenizer.Find_Next (Analyzer);

      Clear_Stack;

      OpenToken.Token.Sequence.Parse (L, Analyzer);
   end loop;

exception
when Ada.Text_IO.End_Error =>
   null;
end ASU_Example_5_10_RD_Commute.Run;
