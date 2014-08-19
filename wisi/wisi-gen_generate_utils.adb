--  Abstract :
--
--  see spec
--
--  Copyright (C) 2014  All Rights Reserved.
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
with Ada.Strings.Fixed;
with Wisi.Utils;
package body Wisi.Gen_Generate_Utils is

   function Count_Non_Reporting return Integer
   is
      Result : Integer := 0;
   begin
      for Kind of Tokens loop
         if -Kind.Kind = """line_comment""" then
            Result := Result + Integer (Kind.Tokens.Length);
         end if;
      end loop;
      return Result;
   end Count_Non_Reporting;

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

      if Token = EOI_Image then
         return Result;
      end if;
      Result := Result + 1;

      for Rule of Rules loop
         if Rule.Left_Hand_Side = Token then
            return Result;
         end if;
         Result := Result + 1;
      end loop;

      if Token = "opentoken_accept" then
         return Result;
      end if;

      raise Not_Found with "token '" & Token & "' not found";
   end Find_Token_ID;

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

      Token_Images (ID) := new String'(EOI_Image);
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

   function Token_Image (ID : in Token_IDs) return String
   is begin
      return Token_Images (ID).all;
   end Token_Image;

   procedure Indent_Line (Text : in String)
   is
      use Ada.Text_IO;
   begin
      Set_Col (Indent);
      Put_Line (Text);
   end Indent_Line;

   procedure Put_Tokens
   is
      use Standard.Ada.Text_IO;
   begin
      Put_Line ("Tokens:");
      for I in Token_IDs'Range loop
         Put_Line (Token_IDs'Image (I) & " => " & Token_Image (I));
      end loop;
      New_Line;
   end Put_Tokens;

   function To_Conflicts return LALR_Parsers.Conflict_Lists.List
   is
      Result : LALR_Parsers.Conflict_Lists.List;
   begin
      for Conflict of Conflicts loop
         Result.Append
           ((LALR_Parsers.Conflict_Parse_Actions'Value (-Conflict.Action_A),
             Find_Token_ID (-Conflict.LHS_A),
             LALR_Parsers.Conflict_Parse_Actions'Value (-Conflict.Action_B),
             Find_Token_ID (-Conflict.LHS_B),
             -1,
             Find_Token_ID (-Conflict.On)));
      end loop;
      return Result;
   exception
   when E : Not_Found =>
      raise OpenToken.Grammar_Error with "known conflicts: " & Ada.Exceptions.Exception_Message (E);
   end To_Conflicts;

   function "&" (Tokens : in Token_Lists.Instance; Token : in String) return Token_Lists.Instance
   is
      use Token_Lists;
   begin
      return Tokens & Tokens_Pkg.Get (Find_Token_ID (Token));
   end "&";

   function To_Grammar (Source_File_Name : in String; Start_Token : in String) return Production_Lists.Instance
   is
      use Productions;
      use Token_Lists;

      Grammar : Production_Lists.Instance;
   begin
      begin
         Grammar := Production_Lists.Only
           (Nonterminals.Get (Accept_ID) <= Nonterminals.Get (Find_Token_ID (Start_Token)) &
              Tokens_Pkg.Get (EOI_ID));
      exception
      when Not_Found =>
         Wisi.Utils.Put_Error
           (Source_File_Name, First_Rule_Line, "start token '" & (Start_Token) & "' not found; need %start?");
         raise Syntax_Error;
      end;

      for Rule of Rules loop
         declare
            Index : Integer := 0;
         begin
            for Right_Hand_Side of Rule.Right_Hand_Sides loop
               declare
                  use Production_Lists;

                  Tokens : Token_Lists.Instance;
               begin
                  for Token of Right_Hand_Side.Production loop
                     Tokens := Tokens & Token;
                  end loop;
                  Grammar := Grammar and Nonterminals.Get (Find_Token_ID (-Rule.Left_Hand_Side)) <= Tokens + Index;
               exception
               when E : Not_Found =>
                  Wisi.Utils.Put_Error
                    (Source_File_Name, Rule.Source_Line, Standard.Ada.Exceptions.Exception_Message (E));
                  raise Syntax_Error;
               end;
               Index := Index + 1;
            end loop;
         end;
      end loop;

      return Grammar;
   end To_Grammar;

   function State_Image (Item : in LALR_Parsers.State_Index) return String
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;
   begin
      return Trim (LALR_Parsers.State_Index'Image (Item), Both);
   end State_Image;

   function Int_Image (Item : in Integer) return String
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;
   begin
      return Trim (Integer'Image (Item), Both);
   end Int_Image;

end Wisi.Gen_Generate_Utils;
