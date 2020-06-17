--  Abstract :
--
--  WisiToken utilities for using the tree-sitter parser.
--
--  Copyright (C) 2020 Free Software Foundation All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (Modified_GPL);

with WisiToken_Grammar_Runtime;
with WisiToken.Syntax_Trees;
package WisiToken.Tree_Sitter is

   procedure Print_Tree_Sitter
     (Data             : in     WisiToken_Grammar_Runtime.User_Data_Type;
      Tree             : in out WisiToken.Syntax_Trees.Tree;
      Input_File_Name  : in     String;
      Output_File_Name : in     String;
      Language_Name    : in     String);
   --  Tree is 'in out' because we use WisiToken.Syntax_Tree.LR_Utils lists.

end WisiToken.Tree_Sitter;
