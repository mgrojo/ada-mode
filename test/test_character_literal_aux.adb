--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017 Stephen Leake All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with AUnit.Assertions;
with AUnit.Checks;
with Ada.Containers;
with Character_Literal;
with WisiToken.AUnit;
with WisiToken.Token_Line_Comment;
package body Test_Character_Literal_Aux is

   Statement_Count : Integer := 0;

   procedure Test_Statement_List_0 (Nonterm : in WisiToken.Semantic_State.Augmented_Token'Class)
   is
      use WisiToken.AUnit;

      Token : WisiToken.Token_Line_Comment.Token renames WisiToken.Token_Line_Comment.Token (Nonterm);
   begin
      if Enable then
         Statement_Count := Statement_Count + 1;

         case Statement_Count is
         when 1 =>
            Check ("statement_list_0 1 line", Token.Line, 2);
            Check ("statement_list_0 1 region", Token.Char_Region, (29, 32));

         when 2 =>
            Check ("statement_list_0 2 line", Token.Line, 3);
            Check ("statement_list_0 2 region", Token.Char_Region, (34, 37));
            Check ("statement_list_0 2 a", Token.First_Indent_Line, 3);
            Check ("statement_list_0 2 b", Token.Last_Indent_Line, 3);
            Check ("statement_list_0 2 c", Token.First_Trailing_Comment_Line, 5);
            Check ("statement_list_0 2 d", Token.Last_Trailing_Comment_Line, 5);

         when 10 =>
            --  object'attribute
            Check ("statement_list_0 10 line", Token.Line, 20);
            Check ("statement_list_0 10 region", Token.Char_Region, (295, 314));
            Check ("statement_list_0 10 a", Token.First_Indent_Line, 20);
            Check ("statement_list_0 10 b", Token.Last_Indent_Line, 21);
            Check ("statement_list_0 10 c", Token.First_Trailing_Comment_Line, WisiToken.Invalid_Line_Number);
            Check ("statement_list_0 10 d", Token.Last_Trailing_Comment_Line, WisiToken.Invalid_Line_Number);

         when others =>
            null;

         end case;
      end if;
   end Test_Statement_List_0;

   procedure Test_Statement_0 (Wisi_Tokens : in WisiToken.Semantic_State.Augmented_Token_Array)
   is
      use AUnit.Checks;
      use Character_Literal;
      use WisiToken.AUnit;

      Character_Token : WisiToken.Token_Line_Comment.Token renames WisiToken.Token_Line_Comment.Token
        (Wisi_Tokens (4).Element.all);

      Semicolon_Token : WisiToken.Token_Line_Comment.Token renames WisiToken.Token_Line_Comment.Token
        (Wisi_Tokens (6).Element.all);
   begin
      if Enable then
         case Character_Literal_Count is
         when 1 =>
            Check ("statement_0 1 char region", Character_Token.Char_Region, (95, 97));
            Check ("statement_0 1 byte region", Character_Token.Byte_Region, (95, 97));
            Check ("statement_0 1 text", Lexer.Buffer_Text (Character_Token.Byte_Region), "'a'");
            Check ("statement_0 1 line", Semicolon_Token.Line, 7);

         when 2 =>
            Check ("statement_0 2 char text", Lexer.Buffer_Text (Character_Token.Byte_Region), "'b'");
            Check ("statement_0 2 line", Semicolon_Token.Line, 8);

         when 3 =>
            Check ("statement_0 3 char text", Lexer.Buffer_Text (Character_Token.Byte_Region), "'c'");
            Check ("statement_0 3 line", Semicolon_Token.Line, 9);

         when 4 =>
            Check ("statement_0 4 char text", Lexer.Buffer_Text (Character_Token.Byte_Region), "'d'");
            Check ("statement_0 4 line", Semicolon_Token.Line, 10);

         when 5 =>
            Check ("statement_0 5 char text", Lexer.Buffer_Text (Character_Token.Byte_Region), "'π'");
            Check ("statement_0 5 line", Semicolon_Token.Line, 11);

         when 6 =>
            Check ("statement_0 6 char text", Lexer.Buffer_Text (Character_Token.Byte_Region), "'''");
            Check ("statement_0 6 line", Semicolon_Token.Line, 14);

         when others =>
            AUnit.Assertions.Assert (False, "unexpected character_literal statement");
         end case;
      end if;
   end Test_Statement_0;

   Statement_1_Count : Integer := 0;

   procedure Test_Statement_1 (Wisi_Tokens : in WisiToken.Semantic_State.Augmented_Token_Array)
   is
      use AUnit.Checks;
      use Ada.Containers;
      use Character_Literal;
      use WisiToken.AUnit;
      use WisiToken;
   begin
      Statement_1_Count := Statement_1_Count + 1;
      case Statement_1_Count is
      when 1 =>
         --  objectπ'attribute;
         for I in Wisi_Tokens.First_Index .. Wisi_Tokens.Last_Index loop
            declare
               Token : Token_Line_Comment.Token renames Token_Line_Comment.Token (Wisi_Tokens (I).Element.all);
            begin
               case I is
               when 1 =>
                  Check ("statement_1 1 1.First", Token.First, True);
               when 2 =>
                  Check ("statement_1 1 2.First", Token.First, False);
               when 3 =>
                  Check ("statement_1 1 3.First", Token.First, False);
               when 4 =>
                  Check ("statement_1 1 4.First", Token.First, True); -- trailing blank line
               when others =>
                  raise Programmer_Error;
               end case;
            end;
         end loop;
      when 2 =>
         --  object
         --    'attribute;
         for I in Wisi_Tokens.First_Index .. Wisi_Tokens.Last_Index loop
            declare
               Token : Token_Line_Comment.Token renames Token_Line_Comment.Token (Wisi_Tokens (I).Element.all);
            begin
               case I is
               when 1 =>
                  Check ("statement_1 2 1.First", Token.First, True);
               when 2 =>
                  Check ("statement_1 2 2.ID", Token.ID, +tick_ID);
                  Check ("statement_1 2 2.First", Token.First, True);
               when 3 =>
                  Check ("statement_1 2 3.First", Token.First, False);
               when 4 =>
                  Check ("statement_1 2 4.First", Token.First, True);
               when others =>
                  raise Programmer_Error with "unexpected token" & Count_Type'Image (I) & Token_ID'Image (Token.ID);
               end case;
            end;
         end loop;

      when others =>
         --  Not in Nominal
         null;
      end case;
   end Test_Statement_1;

   Statement_2_Count : Integer := 0;
   procedure Test_Statement_2 (Wisi_Tokens : in WisiToken.Semantic_State.Augmented_Token_Array)
   is
      use AUnit.Checks;
      use Ada.Containers;
      use Character_Literal;
      use WisiToken.AUnit;
      use WisiToken;
   begin
      Statement_2_Count := Statement_2_Count + 1;
      case Statement_2_Count is
      when 1 =>
         --  " a string with Greek ..."
         for I in Wisi_Tokens.First_Index .. Wisi_Tokens.Last_Index loop
            declare
               Token : Token_Line_Comment.Token renames Token_Line_Comment.Token (Wisi_Tokens (I).Element.all);
            begin
               case I is
               when 1 =>
                  Check ("statement_2 1 1.First", Token.First, True);
               when 2 =>
                  Check ("statement_2 1 2.ID", Token.ID, +SEMICOLON_ID);
                  Check ("statement_2 1 2.Line", Token.Line, 23);
                  Check ("statement_2 1 2.First", Token.First, True);

               when others =>
                  raise Programmer_Error with "unexpected token" & Count_Type'Image (I) & Token_ID'Image (Token.ID);
               end case;
            end;
         end loop;

      when others =>
         --  Not tested
         null;

      end case;
   end Test_Statement_2;

end Test_Character_Literal_Aux;
