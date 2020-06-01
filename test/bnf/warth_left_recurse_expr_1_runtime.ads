--  Abstract :
--
--  Runtime utils for warth_left_recurse_expr_1.wy actions.
--
--  Copyright (C) 2018, 2020 Stephen Leake All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with SAL.Gen_Unbounded_Definite_Stacks;
with WisiToken.Lexer;
with WisiToken.Syntax_Trees;
package Warth_Left_Recurse_Expr_1_Runtime is

   package Integer_Stacks is new SAL.Gen_Unbounded_Definite_Stacks (Integer);

   type User_Data_Type is new WisiToken.Syntax_Trees.User_Data_Type with
   record
      Lexer     : WisiToken.Lexer.Handle;
      Terminals : WisiToken.Base_Token_Array_Access_Constant;
      Stack     : Integer_Stacks.Stack;
   end record;

   overriding
   procedure Set_Lexer_Terminals
     (User_Data : in out User_Data_Type;
      Lexer     : in     WisiToken.Lexer.Handle;
      Terminals : in     WisiToken.Base_Token_Array_Access_Constant);

   overriding procedure Reset (Data : in out User_Data_Type);

   procedure Push
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in     WisiToken.Syntax_Trees.Tree;
      Tokens    : in     WisiToken.Valid_Node_Index_Array;
      Arg_Index : in     WisiToken.Positive_Index_Type);
   --  Push value of Tree (Tokens (Arg_Index)) onto User_Data.Stack.

   procedure Subtract
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in     WisiToken.Syntax_Trees.Tree;
      Tokens    : in     WisiToken.Valid_Node_Index_Array);
   --  Pop two values, subtract them, push result.

end Warth_Left_Recurse_Expr_1_Runtime;
