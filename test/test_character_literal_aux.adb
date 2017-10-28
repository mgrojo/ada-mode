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
with AUnit.Checks.Containers;
with AUnit.Checks.Text_IO;
with Ada.Containers;
with Character_Literal;
with WisiToken.AUnit;
with WisiToken.Token_Line_Comment;
package body Test_Character_Literal_Aux is

   Statement_Count : Integer := 0;

   procedure Test_Statement_List_0 (Nonterm : in WisiToken.Augmented_Token'Class)
   is
      use AUnit.Checks.Containers;
      use AUnit.Checks.Text_IO;
      use WisiToken.AUnit;

      Token : WisiToken.Token_Line_Comment.Token renames WisiToken.Token_Line_Comment.Token (Nonterm);
   begin
      if Enable then
         Statement_Count := Statement_Count + 1;

         case Statement_Count is
         when 1 =>
            Check ("statement_list_0 1 line", Token.Line, 2);
            Check ("statement_list_0 1 non_grammar.length", Token.Non_Grammar.Length, 0);
            Check ("statement_list_0 1 region", Token.Char_Region, (29, 33)); -- includes trailing non_grammar

         when 2 =>
            Check ("statement_list_0 2 line", Token.Line, 3);
            Check ("statement_list_0 2 non_grammar.length", Token.Non_Grammar.Length, 0);
            Check ("statement_list_0 2 region", Token.Char_Region, (34, 83));

         when 11 =>
            Check ("statement_list_0 11 line", Token.Line, 22);
            Check ("statement_list_0 11 non_grammar.length", Token.Non_Grammar.Length, 0);
            Check ("statement_list_0 11 region", Token.Char_Region, (390, 438)); -- EOF

         when others =>
            null;

         end case;
      end if;
   end Test_Statement_List_0;

   procedure Test_Statement_0 (Wisi_Tokens : in WisiToken.Augmented_Token_Array)
   is
      use AUnit.Checks;
      use AUnit.Checks.Containers;
      use AUnit.Checks.Text_IO;
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
            Check ("statement_0 1 non_grammar.length", Semicolon_Token.Non_Grammar.Length, 1);
            Check ("statement_0 1 non_grammar.id", Semicolon_Token.Non_Grammar (1).ID, +NEW_LINE_ID);

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
            Check ("statement_0 5 non_grammar.length", Semicolon_Token.Non_Grammar.Length, 4);
            Check ("statement_0 5 non_grammar (1).id", Semicolon_Token.Non_Grammar (1).ID, +NEW_LINE_ID);
            Check ("statement_0 5 non_grammar (2).id", Semicolon_Token.Non_Grammar (2).ID, +NEW_LINE_ID);
            Check ("statement_0 5 non_grammar (3).id", Semicolon_Token.Non_Grammar (3).ID, +COMMENT_ID);
            Check ("statement_0 5 non_grammar (4).id", Semicolon_Token.Non_Grammar (4).ID, +NEW_LINE_ID);

         when 6 =>
            Check ("statement_0 6 char text", Lexer.Buffer_Text (Character_Token.Byte_Region), "'''");
            Check ("statement_0 6 line", Semicolon_Token.Line, 14);

         when others =>
            AUnit.Assertions.Assert (False, "unexpected character_literal statement");
         end case;
      end if;
   end Test_Statement_0;

   Statement_1_Count : Integer := 0;

   procedure Test_Statement_1 (Wisi_Tokens : in WisiToken.Augmented_Token_Array)
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
                  Check ("statement_1 1 4.First", Token.First, False);
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
                  Check ("statement_1 2 4.First", Token.First, False);
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

   procedure Test_Statement_2 (Wisi_Tokens : in WisiToken.Augmented_Token_Array)
   is
      pragma Unreferenced (Wisi_Tokens);
   begin
      null;
      --  FIXME: test lines, comments; 'comment before EOF'
   end Test_Statement_2;

end Test_Character_Literal_Aux;
