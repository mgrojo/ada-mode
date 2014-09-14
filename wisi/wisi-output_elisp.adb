--  Abstract :
--
--  Output Elisp code implementing the grammar defined by the parameters.
--
--  Copyright (C) 2012, 2013, 2014 Stephen Leake.  All Rights Reserved.
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
--  the Free Software Foundation, 51 Franklin Street, Suite 500, Boston,
--  MA 02110-1335, USA.

pragma License (GPL);

with Ada.Text_IO;
with OpenToken.Production.Parser.LALR.Elisp;
with Wisi.Gen_Generate_Utils;
procedure Wisi.Output_Elisp
  (Input_File_Name   : in String;
   Elisp_Package     : in String;
   Prologue          : in String_Lists.List;
   Keywords          : in String_Pair_Lists.List;
   Tokens            : in Token_Lists.List;
   Start_Token       : in Standard.Ada.Strings.Unbounded.Unbounded_String;
   Conflicts         : in Conflict_Lists.List;
   Rules             : in Rule_Lists.List;
   First_State_Index : in Integer)
is
   EOI_Image              : constant String := "$EOI";
   OpenToken_Accept_Image : constant String := "opentoken_accept";

   function To_Token_Image (Item : in Ada.Strings.Unbounded.Unbounded_String) return String
   is begin
      return -Item;
   end To_Token_Image;

   package Generate_Utils is new Wisi.Gen_Generate_Utils
     (Keywords, Tokens, Conflicts, Rules, EOI_Image, OpenToken_Accept_Image,
      First_State_Index,
      To_Token_Image => To_Token_Image);

   package Parser_Elisp is new Generate_Utils.LALRs.Elisp (Generate_Utils.Token_Image);

   Shift_Reduce_Conflict_Count  : Integer;
   Reduce_Reduce_Conflict_Count : Integer;

   Grammar : constant Generate_Utils.Production_Lists.Instance := Generate_Utils.To_Grammar
     (Input_File_Name, -Start_Token);

   Parser : constant Generate_Utils.LALRs.Parse_Table_Ptr := Generate_Utils.LALR_Generators.Generate
     (Grammar,
      Generate_Utils.To_Conflicts (Shift_Reduce_Conflict_Count, Reduce_Reduce_Conflict_Count),
      Trace                    => Verbosity > 1,
      Put_Parse_Table          => Verbosity > 0,
      Ignore_Unused_Tokens     => Verbosity > 1,
      Ignore_Unknown_Conflicts => Verbosity > 1);

   File : Standard.Ada.Text_IO.File_Type;

   procedure Header (Elisp_Package : in String; Prologue : in String_Lists.List)
   is
      use Standard.Ada.Text_IO;
   begin
      Put_Line (";;; " & Elisp_Package & "-wy.el --- Generated parser support file");
      New_Line;
      for Line of Prologue loop
         Put_Line (Line);
      end loop;
   end Header;

   procedure Keyword_Table
     (Elisp_Package : in String;
      Keywords      : in Wisi.String_Pair_Lists.List)
   is
      use Standard.Ada.Text_IO;
   begin
      Put_Line ("(defconst " & Elisp_Package & "-wy--keyword-table");
      Put_Line ("  (semantic-lex-make-keyword-table");
      Put_Line ("   '(");
      for Pair of Keywords loop
         Put_Line ("    (" & (-Pair.Value) & " . " & (-Pair.Name) & ")");
      end loop;
      Put_Line ("    )");
      Put_Line ("   nil)");
      Put_Line ("  ""Table of language keywords."")");
   end Keyword_Table;

   procedure Token_Table
     (Elisp_Package : in String;
      Tokens        : in Wisi.Token_Lists.List)
   is
      use Standard.Ada.Strings.Unbounded; -- length
      use Standard.Ada.Text_IO;
   begin
      Put_Line ("(defconst " & Elisp_Package & "-wy--token-table");
      Put_Line ("  (semantic-lex-make-type-table");
      Put_Line ("   '(");
      for Kind of Tokens loop
         if not (-Kind.Kind = """line_comment""" or -Kind.Kind = """whitespace""") then
            Put_Line ("     (" & (-Kind.Kind));
            for Token of Kind.Tokens loop
               if 0 = Length (Token.Value) then
                  Put_Line ("      (" & (-Token.Name) & ")");
               else
                  Put_Line ("      (" & (-Token.Name) & " . " & (-Token.Value) & ")");
               end if;
            end loop;
            Put_Line ("     )");
         end if;
      end loop;
      Put_Line ("    )");
      Put_Line ("   nil)");
      Put_Line ("  ""Table of language tokens."")");
   end Token_Table;

   use Standard.Ada.Text_IO;
begin
   if Verbosity > 0 then
      Put_Line ("Grammar:");
      Generate_Utils.Print_Production_Lists.Print (Grammar);
      New_Line;
   end if;

   Create (File, Out_File, Elisp_Package & "-wy.el");
   Set_Output (File);
   Header (Elisp_Package, Prologue);
   New_Line;
   Keyword_Table (Elisp_Package, Keywords);
   New_Line;
   Token_Table (Elisp_Package, Tokens);
   New_Line;
   Parser_Elisp.Output (Elisp_Package, Tokens, Keywords, Rules, Parser);
   New_Line;
   Put_Line ("(provide '" & Elisp_Package & "-wy)");
   New_Line;
   Put_Line (";; end of file");
   Close (File);
end Wisi.Output_Elisp;
