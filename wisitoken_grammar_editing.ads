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
with Wisitoken_Grammar_Actions;
with WisiToken_Grammar_Runtime;
with WisiToken.Syntax_Trees.LR_Utils;
package WisiToken_Grammar_Editing is
   use all type WisiToken.Token_ID;
   use all type Wisitoken_Grammar_Actions.Token_Enum_ID;
   use all type WisiToken.Syntax_Trees.Node_Label;

   type Identifier_Token_Index (Label : WisiToken.Syntax_Trees.Terminal_Label) is record
      case Label is
      when Shared_Terminal =>
         Shared_Token : WisiToken.Token_Index;
         Shared_ID    : WisiToken.Token_ID; --  So function ID does not require Terminals.

      when Virtual_Terminal =>
         Virtual_ID : WisiToken.Token_ID;

      when Virtual_Identifier =>
         Identifier    : WisiToken.Identifier_Index;
         Identifier_ID : WisiToken.Token_ID;
         Byte_Region   : WisiToken.Buffer_Region;

      end case;
   end record;

   function ID (Item : in Identifier_Token_Index) return WisiToken.Token_ID
   is (case Item.Label is
       when Shared_Terminal    => Item.Shared_ID,
       when Virtual_Terminal   => Item.Virtual_ID,
       when Virtual_Identifier => Item.Identifier_ID);

   Invalid_Identifier_Token : constant Identifier_Token_Index :=
     (Label => WisiToken.Syntax_Trees.Virtual_Terminal, Virtual_ID => WisiToken.Invalid_Token_ID);

   function To_Identifier_Token
     (Item      : in WisiToken.Token_Index;
      Terminals : in WisiToken.Base_Token_Array_Access_Constant)
     return Identifier_Token_Index;

   function To_Identifier_Token
     (Item      : in WisiToken.Valid_Node_Index;
      Tree      : in WisiToken.Syntax_Trees.Tree;
      Terminals : in WisiToken.Base_Token_Array_Access_Constant)
     return Identifier_Token_Index
   with Pre => To_Token_Enum (Tree.ID (Item)) in rhs_element_ID | rhs_item_ID | IDENTIFIER_ID;

   function Add_Identifier_Token
     (Tree : in out WisiToken.Syntax_Trees.Tree;
      Item : in     Identifier_Token_Index)
     return WisiToken.Valid_Node_Index
   with Pre => Item.Label in WisiToken.Syntax_Trees.Virtual_Terminal_Label;

   function Add_RHS_Item
     (Tree : in out WisiToken.Syntax_Trees.Tree;
      Item : in     WisiToken.Valid_Node_Index)
     return WisiToken.Valid_Node_Index
   with Pre => Tree.ID (Item) = +IDENTIFIER_ID;

   function Add_RHS_Element
     (Tree : in out WisiToken.Syntax_Trees.Tree;
      Item : in     WisiToken.Valid_Node_Index)
     return WisiToken.Valid_Node_Index
   with Pre => Tree.ID (Item) = +rhs_item_ID;

   function Empty_RHS_Item_List
     (Tree : aliased in out WisiToken.Syntax_Trees.Tree)
     return WisiToken.Syntax_Trees.LR_Utils.List;

   function Empty_RHS_List
     (Tree : aliased in out WisiToken.Syntax_Trees.Tree)
     return WisiToken.Syntax_Trees.LR_Utils.List;

   function Add_RHS
     (Tree : in out WisiToken.Syntax_Trees.Tree;
      Item : in     WisiToken.Valid_Node_Index)
     return WisiToken.Valid_Node_Index
   with Pre => Tree.ID (Item) = +rhs_item_list_ID;

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
