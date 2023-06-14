--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018, 2020 - 2023 Stephen Leake All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

package body Warth_Left_Recurse_Expr_1_Runtime is

   ----------
   --  Public subprograms, declaration order

   procedure Reset (Data : in out User_Data_Type)
   is begin
      --  Preserve Lexer, Terminals
      Data.Stack.Clear;
   end Reset;

   procedure Push
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in     WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Arg_Index : in     WisiToken.Positive_Index_Type)
   is
      Data   : User_Data_Type renames User_Data_Type (User_Data);
      Region : constant WisiToken.Buffer_Region := Tree.Byte_Region
        (Tree.Child (Nonterm, Arg_Index), Trailing_Non_Grammar => False);
   begin
      Data.Stack.Push (Integer'Value (Tree.Lexer.Buffer_Text (Region)));
   end Push;

   procedure Subtract
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in     WisiToken.Syntax_Trees.Tree)
   is
      pragma Unreferenced (Tree);

      Data  : User_Data_Type renames User_Data_Type (User_Data);
      Right : constant Integer := Data.Stack.Pop;
      Left  : constant Integer := Data.Stack.Pop;
   begin
      Data.Stack.Push (Left - Right);
   end Subtract;

end Warth_Left_Recurse_Expr_1_Runtime;
