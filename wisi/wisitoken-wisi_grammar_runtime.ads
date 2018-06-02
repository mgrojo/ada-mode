--  Abstract :
--
--  Runtime utils for wisi_grammar.wy actions.
--
--  Copyright (C) 2018 Stephen Leake All Rights Reserved.
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

with Wisi;
with WisiToken.Lexer;
with WisiToken.Syntax_Trees;
package WisiToken.Wisi_Grammar_Runtime is

   type User_Data_Type is new WisiToken.Syntax_Trees.User_Data_Type with
   record
      Lexer            : WisiToken.Lexer.Handle;
      Terminals        : Base_Token_Array_Access;
      Raw_Code         : Wisi.Raw_Code;
      Generate_Params  : Wisi.Generate_Param_Type;
      Tokens           : Wisi.Tokens;
      Elisp_Names      : Wisi.Elisp_Names;
      Conflicts        : Wisi.Conflict_Lists.List;
      McKenzie_Recover : Wisi.McKenzie_Recover_Param_Type;

      Rule_Count      : Integer := 0;
      Action_Count    : Integer := 0;
      Check_Count     : Integer := 0;

      Ignore_Lines : Boolean := False;
      --  An '%if' specified a different lexer
   end record;

   overriding
   procedure Set_Lexer_Terminals
     (User_Data : in out User_Data_Type;
      Lexer     : in     WisiToken.Lexer.Handle;
      Terminals : in     Base_Token_Array_Access);

   overriding procedure Reset (Data : in out User_Data_Type);

   procedure Start_If
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in     WisiToken.Syntax_Trees.Tree;
      Tokens    : in     WisiToken.Syntax_Trees.Valid_Node_Index_Array);

   procedure End_If (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class);

   procedure Add_Declaration
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in     WisiToken.Syntax_Trees.Tree;
      Tokens    : in     WisiToken.Syntax_Trees.Valid_Node_Index_Array);

   procedure Add_Nonterminal
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in     WisiToken.Syntax_Trees.Tree;
      Tokens    : in     WisiToken.Syntax_Trees.Valid_Node_Index_Array);

end WisiToken.Wisi_Grammar_Runtime;
