--  Abstract :
--
--  Run LALR.Generate, for testing. Exceptions raised by Generate are
--  propagated.
--
--  Copyright (C) 2013, 2014 Stephen Leake.  All Rights Reserved.
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

with Ada.Exceptions;
with Ada.Text_IO;
with OpenToken.Production.List.Print;
with OpenToken.Production.Parser.LALR.Generator;
with OpenToken.Production.Print;
with OpenToken.Token.Enumerated.Analyzer;
with OpenToken.Token.Enumerated.List.Print;
with OpenToken.Token.Enumerated.Nonterminal;
with Wisi.Utils;
procedure Wisi.Test_Generate
  (Input_File_Name : in String;
   Keywords        : in String_Pair_Lists.List;
   Tokens          : in Token_Lists.List;
   Start_Token     : in Standard.Ada.Strings.Unbounded.Unbounded_String;
   Rules           : in Rule_Lists.List)
is
   subtype Token_IDs is Integer range
     1 .. Count (Tokens) + Integer (Keywords.Length) + 1 + Integer (Rules.Length) + 1;
   --  one extra terminal for $EOI
   --  one extra non-terminal for the OpenToken accept symbol followed by EOI.

   Token_Count : constant Token_IDs := Count (Tokens);
   EOI_ID      : constant Token_IDs := Token_Count + Token_IDs (Keywords.Length) + 1; -- last terminal
   Accept_ID   : constant Token_IDs := Token_IDs'Last;                                -- last nonterminal

   First_Rule_Line : constant Standard.Ada.Text_IO.Positive_Count := Rules.First_Element.Source_Line;

   function Find_Token_ID (Token : in String) return Token_IDs
   is
      use type Standard.Ada.Strings.Unbounded.Unbounded_String;
      Result : Token_IDs := Token_IDs'First;
   begin
      for Kind of Tokens loop
         for Pair of Kind.Tokens loop
            if Pair.Name = Token then
               return Result;
            end if;
            Result := Result + 1;
         end loop;
      end loop;
      for Pair of Keywords loop
         if Pair.Name = Token then
            return Result;
         end if;
         Result := Result + 1;
      end loop;
      Result := Result + 1; -- EOI

      for Rule of Rules loop
         if Rule.Left_Hand_Side = Token then
            return Result;
         end if;
         Result := Result + 1;
      end loop;
      raise Not_Found with "token '" & Token & "' not found";
   end Find_Token_ID;

   Token_Image_Width : Integer := 0;

   type ID_Array_Access_String_Type is array (Token_IDs) of access constant String;

   function Set_Token_Images return ID_Array_Access_String_Type
   is
      ID           : Token_IDs := Token_IDs'First;
      Token_Images : ID_Array_Access_String_Type;
   begin
      for Kind of Tokens loop
         for Pair of Kind.Tokens loop
            Token_Images (ID) := new String'(-Pair.Name);
            ID := ID + 1;
         end loop;
      end loop;

      if ID /= Token_Count + 1 then raise Programmer_Error; end if;

      for Pair of Keywords loop
         Token_Images (ID) := new String'(-Pair.Name);
         ID := ID + 1;
      end loop;

      if ID /= EOI_ID then raise Programmer_Error; end if;

      Token_Images (ID) := new String'("$EOI"); -- match wisent-eoi-term
      ID                := ID + 1;

      for Rule of Rules loop
         Token_Images (ID) := new String'(-Rule.Left_Hand_Side);
         ID := ID + 1;
      end loop;

      if ID /= Accept_ID then raise Programmer_Error; end if;

      Token_Images (ID) := new String'("opentoken_accept");

      for Token of Token_Images loop
         if Token.all'Length > Token_Image_Width then
            Token_Image_Width := Token.all'Length;
         end if;
      end loop;

      return Token_Images;
   end Set_Token_Images;

   Token_Images : constant ID_Array_Access_String_Type := Set_Token_Images;

   function Token_Image (ID : in Token_IDs) return String
   is begin
      return Token_Images (ID).all;
   end Token_Image;

   package Tokens_Pkg is new OpenToken.Token.Enumerated (Token_IDs, Token_Image, Token_Image_Width);
   --  we only need Analyzers to instantiate Parsers, but we might call it for debugging
   package Analyzers is new Tokens_Pkg.Analyzer (First_Terminal => Token_IDs'First, Last_Terminal => EOI_ID);
   package Token_Lists is new Tokens_Pkg.List;
   package Nonterminals is new Tokens_Pkg.Nonterminal (Token_Lists);
   package Productions is new OpenToken.Production (Tokens_Pkg, Token_Lists, Nonterminals);
   package Production_Lists is new Productions.List;
   package Parsers is new Productions.Parser (Production_Lists, Analyzers);
   package LALRs is new Parsers.LALR (First_State_Index => 0);
   package LALR_Generators is new LALRs.Generator;

   Grammar : Production_Lists.Instance;
   Parser  : LALRs.Parse_Table_Ptr;
   pragma Unreferenced (Parser);

   --  Allow infix operators for building productions
   use type Token_Lists.Instance;
   use type Productions.Right_Hand_Side;
   use type Productions.Instance;
   use type Production_Lists.Instance;

   function "&" (Tokens : in Token_Lists.Instance; Token : in String) return Token_Lists.Instance
   is begin
      return Tokens & Tokens_Pkg.Get (Find_Token_ID (Token));
   end "&";

begin
   begin
      Grammar := Production_Lists.Only
        (Nonterminals.Get (Accept_ID) <= Nonterminals.Get (Find_Token_ID (-Start_Token)) &
           Tokens_Pkg.Get (EOI_ID));
   exception
   when Not_Found =>
      Wisi.Utils.Put_Error
        (Input_File_Name, First_Rule_Line, "start token '" & (-Start_Token) & "' not found; need %start?");
      raise Syntax_Error;
   end;

   for Rule of Rules loop
      declare
         Index  : Integer := 0;
      begin
         for Right_Hand_Side of Rule.Right_Hand_Sides loop
            declare
               Tokens : Token_Lists.Instance;
            begin
               for Token of Right_Hand_Side.Production loop
                  Tokens := Tokens & Token;
               end loop;
               Grammar := Grammar and Nonterminals.Get (Find_Token_ID (-Rule.Left_Hand_Side)) <= Tokens + Index;
            exception
            when E : Not_Found =>
               Wisi.Utils.Put_Error
                 (Input_File_Name, Rule.Source_Line, Standard.Ada.Exceptions.Exception_Message (E));
               raise Syntax_Error;
            end;
            Index := Index + 1;
         end loop;
      end;
   end loop;

   --  List unused tokens first
   Parser := LALR_Generators.Generate
     (Grammar,
      Put_Parse_Table => Verbosity > 0,
      Trace           => Verbosity > 1);

   if Verbosity > 0 then
      declare
         use Standard.Ada.Text_IO;
         procedure Print_Action (Item : in Nonterminals.Synthesize) is null;
         package Token_List_Print is new Token_Lists.Print;
         package Print_Production is new Productions.Print (Token_List_Print, Print_Action);
         package Print_Production_Lists is new Production_Lists.Print (Print_Production.Print);
      begin
         Put_Line ("Tokens:");
         for I in Token_IDs'Range loop
            Put_Line (Token_IDs'Image (I) & " => " & Token_Image (I));
         end loop;
         New_Line;

         Put_Line ("Grammar:");
         Print_Production_Lists.Print (Grammar);
         New_Line;
      end;
   end if;

   --  FIXME: this should duplicate part of wisi-output_elisp.adb; add "No_Elisp" flag to that
end Wisi.Test_Generate;
