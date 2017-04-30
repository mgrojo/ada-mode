--  Abstract :
--
--  Output Elisp code implementing the grammar defined by the parameters.
--
--  Copyright (C) 2012 - 2015, 2017 Stephen Leake.  All Rights Reserved.
--
--  The FastToken package is free software; you can redistribute it
--  and/or modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. This library is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with Ada.Text_IO;
with FastToken.Parser.LR.Elisp;
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
   Rule_Count        : in Integer;
   Action_Count      : in Integer;
   Parser_Algorithm  : in Parser_Algorithm_Type;
   First_State_Index : in Integer)
is
   EOI_Name : constant Ada.Strings.Unbounded.Unbounded_String := +"Wisi_EOI";
   --  See comments in wisi-output_ada_emacs.adb EOI_Name for what
   --  this must match.

   FastToken_Accept_Name : constant Ada.Strings.Unbounded.Unbounded_String := +"opentoken_accept";

   function To_Token_Image (Item : in Ada.Strings.Unbounded.Unbounded_String) return String
   is begin
      return -Item;
   end To_Token_Image;

   package Generate_Utils is new Wisi.Gen_Generate_Utils
     (Keywords, Tokens, Conflicts, Rules, EOI_Name, FastToken_Accept_Name,
      First_State_Index,
      To_Token_Image => To_Token_Image);

   package Parser_Elisp is new Generate_Utils.LR.Elisp (Generate_Utils.Token_Image);

   Accept_Reduce_Conflict_Count : Integer;
   Shift_Reduce_Conflict_Count  : Integer;
   Reduce_Reduce_Conflict_Count : Integer;

   Grammar : constant Generate_Utils.Production.List.Instance := Generate_Utils.To_Grammar
     (Input_File_Name, -Start_Token);

   Parser : constant Generate_Utils.LR.Parse_Table_Ptr := Generate_Utils.LALR_Generator.Generate
     (Grammar,
      Generate_Utils.To_Conflicts
        (Accept_Reduce_Conflict_Count, Shift_Reduce_Conflict_Count, Reduce_Reduce_Conflict_Count),
      Trace                    => Verbosity > 1,
      Put_Parse_Table          => Verbosity > 0,
      Ignore_Unused_Tokens     => Verbosity > 1,
      Ignore_Unknown_Conflicts => Verbosity > 1);

   File : Standard.Ada.Text_IO.File_Type;

   procedure Header (Elisp_Package : in String; Prologue : in String_Lists.List)
   is
      use Standard.Ada.Text_IO;
   begin
      Put_Line (";;; " & Elisp_Package & "-elisp.el --- Generated parser support file  -*- lexical-binding:t -*-");
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
      Put_Line ("(defconst " & Elisp_Package & "-elisp-keyword-table");
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
      Put_Line ("(defconst " & Elisp_Package & "-elisp-token-table");
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
   if Parser_Algorithm /= LALR then
      raise Programmer_Error with "parser algorithm LR1 not implemented";
   end if;

   if Verbosity > 0 then
      Put_Line ("Grammar:");
      Generate_Utils.Put_Trace_Production.Put_Trace (Grammar);
      New_Line;
   end if;

   Create (File, Out_File, Elisp_Package & "-elisp.el");
   Set_Output (File);
   Header (Elisp_Package, Prologue);
   New_Line;
   Put_Line ("(require 'wisi)");
   Put_Line ("(require 'semantic/lex)");
   Put_Line ("(require 'wisi-compile)");
   New_Line;
   Keyword_Table (Elisp_Package, Keywords);
   New_Line;
   Token_Table (Elisp_Package, Tokens);
   New_Line;
   Parser_Elisp.Output (Elisp_Package, Tokens, Keywords, Rules, Parser);
   New_Line;
   Put_Line ("(provide '" & Elisp_Package & "-elisp)");
   New_Line;
   Put_Line (";; end of file");
   Close (File);

   Set_Output (Standard_Output);

   if Verbosity > 0 then
      Put_Line
        (Integer'Image (Rule_Count) & " rules," &
           Integer'Image (Action_Count) & " actions," &
           Integer'Image (Accept_Reduce_Conflict_Count) & " accept/reduce conflicts," &
           Integer'Image (Shift_Reduce_Conflict_Count) & " shift/reduce conflicts," &
           Integer'Image (Reduce_Reduce_Conflict_Count) & " reduce/reduce conflicts," &
           Generate_Utils.LR.State_Index'Image (Parser'Last) & " states");
   end if;
end Wisi.Output_Elisp;
