--  Abstract :
--
--  Stuff needed by Character_Literal which cannot 'with'
--  Test_Character_Literal.
--
--  Copyright (C) 2017, 2018 Stephen Leake All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with WisiToken.Lexer;
with WisiToken.Semantic_State;
with WisiToken.Syntax_Trees;
package Test_Character_Literal_Aux is

   Enable : Boolean;
   --  If True, the following procedures execute checks. If False, they do nothing.

   Lexer : access WisiToken.Lexer.Instance'Class;

   type User_Data is new WisiToken.Syntax_Trees.User_Data_Type with record
      Character_Literal_Count : Integer                 := 0;
      String_Literal_Count    : Integer                 := 0;
      Char_Region             : WisiToken.Buffer_Region := WisiToken.Null_Buffer_Region;
   end record;

   procedure Test_Statement_List_0 (Nonterm : in WisiToken.Semantic_State.Augmented_Token);

   procedure Test_Statement_0
     (User_Data   : in out Test_Character_Literal_Aux.User_Data;
      Wisi_Tokens : in     WisiToken.Semantic_State.Augmented_Token_Array);

   procedure Test_Statement_1 (Wisi_Tokens : in WisiToken.Semantic_State.Augmented_Token_Array);

   procedure Test_Statement_2 (Wisi_Tokens : in WisiToken.Semantic_State.Augmented_Token_Array);

end Test_Character_Literal_Aux;
