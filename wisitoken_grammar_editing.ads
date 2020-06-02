--  Abstract :
--
--  Utilities for editing wisitoken grammars.
--
--  Copyright (C) 2018 - 2020 Free Software Foundation, Inc.
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

with WisiToken.Syntax_Trees;
with WisiToken_Grammar_Runtime;
with WisiToken.Syntax_Trees.LR_Utils;
package WisiToken_Grammar_Editing is

   function Find_Declaration
     (Data : in     WisiToken_Grammar_Runtime.User_Data_Type;
      Tree : in out WisiToken.Syntax_Trees.Tree;
      Name : in     String)
     return WisiToken.Node_Index;
   --  Return the node that declares Name, Invalid_Node_Index if none.
   --  The node is either a declaration or a nonterminal.

   procedure Translate_EBNF_To_BNF
     (Tree : in out WisiToken.Syntax_Trees.Tree;
      Data : in out WisiToken_Grammar_Runtime.User_Data_Type);
   --  Process EBNF nonterms, adding new nonterms as needed, resulting in
   --  a BNF tree.
   --
   --  Generator.LR.*_Generate requires a BNF grammar.

   procedure Print_Source
     (File_Name : in String;
      Tree      : in WisiToken.Syntax_Trees.Tree;
      Data      : in WisiToken_Grammar_Runtime.User_Data_Type);
   --  Print the wisitoken grammar source represented by Tree, Terminals
   --  to a new file File_Name.

end WisiToken_Grammar_Editing;
