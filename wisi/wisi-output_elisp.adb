--  Abstract :
--
--  Output Elisp code implementing the grammar defined by the parameters.
--
--  Copyright (C) 2012, 2013 Stephen Leake.  All Rights Reserved.
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
with OpenToken.Production.List.Print;
with OpenToken.Production.Parser.LALR.Elisp;
with OpenToken.Production.Print;
with OpenToken.Token.Enumerated.Analyzer;
with OpenToken.Token.Enumerated.List.Print;
with OpenToken.Token.Enumerated.Nonterminal;
with OpenToken.Token.Enumerated;
procedure Wisi.Output_Elisp
  (Elisp_Package : in String;
   Copyright     : in String;
   Prologue      : in String_Lists.List;
   Keywords      : in String_Pair_Lists.List;
   Tokens        : in Token_Lists.List;
   Rules         : in Rule_Lists.List)
is
   subtype Token_IDs is Integer range
     1 .. Wisi.Count (Tokens) + Integer (Keywords.Length) + 1 + Integer (Rules.Length) + 1;
   --  one extra terminal for EOF
   --  one extra non-terminal for the OpenToken accept symbol followed by EOF.

   Token_Count       : constant Token_IDs := Count (Tokens);
   EOF_ID            : constant Token_IDs := Token_Count + Token_IDs (Keywords.Length) + 1; -- last terminal
   First_Nonterminal : constant Token_IDs := EOF_ID + 1;
   Accept_ID         : constant Token_IDs := Token_IDs'Last;                    -- last nonterminal

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
      Result := Result + 1; -- EOF

      for Rule of Rules loop
         if Rule.Left_Hand_Side = Token then
            return Result;
         end if;
         Result := Result + 1;
      end loop;
      raise Programmer_Error with "token '" & Token & "' not found";
   end Find_Token_ID;

   package Tokens_Pkg is new OpenToken.Token.Enumerated (Token_IDs);
   --  we only need Analyzers to instantiate Parsers, but we might call it for debugging
   package Analyzers is new Tokens_Pkg.Analyzer (Last_Terminal => EOF_ID);
   package Token_Lists is new Tokens_Pkg.List;
   package Nonterminals is new Tokens_Pkg.Nonterminal (Token_Lists);
   package Productions is new OpenToken.Production (Tokens_Pkg, Token_Lists, Nonterminals);
   package Production_Lists is new Productions.List;
   package Parsers is new Productions.Parser (Production_Lists, Analyzers);
   package LALR_Parsers is new Parsers.LALR;

   package Parser_Elisp is new LALR_Parsers.Elisp;

   Grammar : Production_Lists.Instance;
   Parser  : LALR_Parsers.Instance;

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
   Grammar := Production_Lists.Only
     (Nonterminals.Get (Accept_ID) <= Nonterminals.Get (First_Nonterminal) & Tokens_Pkg.Get (EOF_ID));

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
            end;
            Index := Index + 1;
         end loop;
      end;
   end loop;

   if Verbosity > 0 then
      declare
         use Standard.Ada.Text_IO;
         function Token_Image (ID : in Token_IDs) return String
         is begin
            return Parser_Elisp.Token_Image (ID, Tokens, Token_Count, Keywords, Rules);
         end Token_Image;

         procedure Print_Action (Item : in Nonterminals.Synthesize) is null;
         package Token_List_Print is new Token_Lists.Print;
         package Print_Production is new Productions.Print (Token_List_Print, Print_Action);
         package Print_Production_Lists is new Production_Lists.Print (Print_Production.Print);
      begin
         Put_Line ("Tokens:");
         for I in Token_IDs'Range loop
            Put_Line (Token_IDs'Image (I) & " => " & Token_Image (I));
         end loop;
         Put_Line ("Grammar:");
         Print_Production_Lists.Print (Grammar);
         New_Line;
      end;
   end if;

   Parser := LALR_Parsers.Generate
     (Grammar,
      Analyzers.Null_Analyzer,
      Trace             => Verbosity > 1,
      Put_Grammar       => Verbosity > 0,
      First_State_Index => 0); -- match Elisp array indexing

   Parser_Elisp.Output (Elisp_Package, Copyright, Prologue, Tokens, Token_Count, Keywords, Rules, Parser);

end Wisi.Output_Elisp;
