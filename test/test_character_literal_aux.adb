--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017, 2018 Stephen Leake All Rights Reserved.
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
with Character_Literal;
with WisiToken.AUnit;
package body Test_Character_Literal_Aux is

   Statement_Count : Integer := 0;

   procedure Test_Statement_List_0 (Nonterm : in WisiToken.Semantic_State.Augmented_Token)
   is
      use WisiToken.AUnit;
   begin
      if Enable then
         Statement_Count := Statement_Count + 1;

         case Statement_Count is
         when 1 =>
            Check ("statement_list_0 1 line", Nonterm.Line, 2);
            Check ("statement_list_0 1 region", Nonterm.Char_Region, (29, 32));

         when 2 =>
            Check ("statement_list_0 2 line", Nonterm.Line, 3);
            Check ("statement_list_0 2 region", Nonterm.Char_Region, (34, 37));
            Check ("statement_list_0 2 a", Nonterm.First_Indent_Line, 3);
            Check ("statement_list_0 2 b", Nonterm.Last_Indent_Line, 3);
            Check ("statement_list_0 2 c", Nonterm.First_Trailing_Comment_Line, 5);
            Check ("statement_list_0 2 d", Nonterm.Last_Trailing_Comment_Line, 5);

         when 10 =>
            --  object'attribute
            Check ("statement_list_0 10 line", Nonterm.Line, 20);
            Check ("statement_list_0 10 region", Nonterm.Char_Region, (295, 314));
            Check ("statement_list_0 10 a", Nonterm.First_Indent_Line, 20);
            Check ("statement_list_0 10 b", Nonterm.Last_Indent_Line, 21);
            Check ("statement_list_0 10 c", Nonterm.First_Trailing_Comment_Line, WisiToken.Invalid_Line_Number);
            Check ("statement_list_0 10 d", Nonterm.Last_Trailing_Comment_Line, WisiToken.Invalid_Line_Number);

         when others =>
            null;

         end case;
      end if;
   end Test_Statement_List_0;

   procedure Test_Statement_0
     (User_Data   : in out Test_Character_Literal_Aux.User_Data;
      Wisi_Tokens : in     WisiToken.Semantic_State.Augmented_Token_Array)
   is
      use AUnit.Checks;
      use Character_Literal;
      use WisiToken.AUnit;

      Character_Token : WisiToken.Semantic_State.Augmented_Token renames Wisi_Tokens (4);
      Semicolon_Token : WisiToken.Semantic_State.Augmented_Token renames Wisi_Tokens (6);
   begin
      if Enable then
         case User_Data.Character_Literal_Count is
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

         when 7 =>
            Check ("statement_0 7 char text", Lexer.Buffer_Text (Character_Token.Byte_Region), "'[""03B8""]'");
            Check ("statement_0 7 line", Semicolon_Token.Line, 29);

         when others =>
            AUnit.Assertions.Assert (False, "unexpected character_literal statement");
         end case;
      end if;
   end Test_Statement_0;

   Statement_1_Count : Integer := 0;

   procedure Test_Statement_1 (Wisi_Tokens : in WisiToken.Semantic_State.Augmented_Token_Array)
   is
      use AUnit.Checks;
      use Character_Literal;
      use WisiToken.AUnit;
      use WisiToken;
   begin
      Statement_1_Count := Statement_1_Count + 1;
      case Statement_1_Count is
      when 1 =>
         --  objectπ'attribute;
         for I in Wisi_Tokens'Range loop
            declare
               Token : Semantic_State.Augmented_Token renames Wisi_Tokens (I);
            begin
               case I is
               when 1 =>
                  --  Objectπ
                  Check ("statement_1 1 1.First", Token.First, True);
                  Check ("statement_1 1 1.Line", Token.Line, 17);
                  Check ("statement_1 1 1.Byte_Region", Token.Byte_Region, (248, 255)); -- π occupies 2 bytes
                  Check ("statement_1 1 1.Char_Region", Token.Char_Region, (247, 253));
               when 2 =>
                  --  '
                  Check ("statement_1 1 2.First", Token.First, False);
               when 3 =>
                  --  attribute
                  Check ("statement_1 1 3.First", Token.First, False);
               when 4 =>
                  --  ;
                  Check ("statement_1 1 4.First", Token.First, True); -- includes non_grammar trailing blank line
               when others =>
                  raise Programmer_Error;
               end case;
            end;
         end loop;
      when 2 =>
         --  object
         --    'attribute;
         for I in Wisi_Tokens'Range loop
            declare
               Token : Semantic_State.Augmented_Token renames Wisi_Tokens (I);
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
                  raise Programmer_Error with "unexpected token" & Positive_Index_Type'Image (I) &
                    Token_ID'Image (Token.ID);
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
      use Character_Literal;
      use WisiToken.AUnit;
      use WisiToken;
   begin
      Statement_2_Count := Statement_2_Count + 1;
      case Statement_2_Count is
      when 1 =>
         --  " a string with Greek ..."
         for I in Wisi_Tokens'Range loop
            declare
               Token : Semantic_State.Augmented_Token renames Wisi_Tokens (I);
            begin
               case I is
               when 1 =>
                  Check ("statement_2 1 1.First", Token.First, True);
               when 2 =>
                  Check ("statement_2 1 2.ID", Token.ID, +SEMICOLON_ID);
                  Check ("statement_2 1 2.Line", Token.Line, 23);
                  Check ("statement_2 1 2.First", Token.First, True);

               when others =>
                  raise Programmer_Error with "unexpected token" & Positive_Index_Type'Image (I) &
                    Token_ID'Image (Token.ID);
               end case;
            end;
         end loop;

      when others =>
         --  Not tested
         null;

      end case;
   end Test_Statement_2;

end Test_Character_Literal_Aux;
