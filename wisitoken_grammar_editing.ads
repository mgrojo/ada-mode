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
   use all type WisiToken.Production_ID;
   use all type WisiToken.Node_Index;
   use all type WisiToken.Token_ID;
   use all type WisiToken.Base_Token_Index;
   use all type WisiToken.Base_Identifier_Index;
   use all type Wisitoken_Grammar_Actions.Token_Enum_ID;
   use all type WisiToken.Syntax_Trees.Node_Label;

   type Identifier_Token_Index
     (Label : WisiToken.Syntax_Trees.Terminal_Label := WisiToken.Syntax_Trees.Terminal_Label'First)
   is record
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

   function Image (Item : in Identifier_Token_Index) return String
   is (case Item.Label is
       when Shared_Terminal => Trimmed_Image (Item.Shared_Token) & ":" &
            Image (Item.Shared_ID, Wisitoken_Grammar_Actions.Descriptor),
       when Virtual_Terminal => Image (Item.Virtual_ID, Wisitoken_Grammar_Actions.Descriptor),
       when Virtual_Identifier => Trimmed_Image (Item.Identifier) & ";" &
            Image (Item.Identifier_ID, Wisitoken_Grammar_Actions.Descriptor));

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
     (Item : in WisiToken.Identifier_Index;
      Byte_Region : in WisiToken.Buffer_Region := WisiToken.Null_Buffer_Region)
     return Identifier_Token_Index
   is ((Virtual_Identifier, Item, +IDENTIFIER_ID, Byte_Region));

   function To_Identifier_Token
     (Item      : in WisiToken.Valid_Node_Index;
      Tree      : in WisiToken.Syntax_Trees.Tree;
      Terminals : in WisiToken.Base_Token_Array_Access_Constant)
     return Identifier_Token_Index
   with Pre => To_Token_Enum (Tree.ID (Item)) in rhs_element_ID | rhs_item_ID | IDENTIFIER_ID;

   function Add_RHS_Group_Item
     (Tree      : in out WisiToken.Syntax_Trees.Tree;
      RHS_Index : in     Natural;
      Content   : in     WisiToken.Valid_Node_Index)
     return WisiToken.Valid_Node_Index
   with Pre => Tree.ID (Content) = +rhs_alternative_list_ID,
     Post => Tree.ID (Add_RHS_Group_Item'Result) = +rhs_group_item_ID;

   function Add_RHS_Optional_Item
     (Tree      : in out WisiToken.Syntax_Trees.Tree;
      RHS_Index : in     Natural;
      Content   : in     WisiToken.Valid_Node_Index)
     return WisiToken.Valid_Node_Index
   with Pre => To_Token_Enum (Tree.ID (Content)) in rhs_alternative_list_ID | IDENTIFIER_ID | STRING_LITERAL_2_ID and
               RHS_Index <= 3,
     Post => Tree.ID (Add_RHS_Optional_Item'Result) = +rhs_optional_item_ID;

   function Add_Identifier_Token
     (Tree      : in out WisiToken.Syntax_Trees.Tree;
      Item      : in     Identifier_Token_Index;
      Terminals : in     WisiToken.Base_Token_Array_Access_Constant)
     return WisiToken.Valid_Node_Index;

   function Add_RHS_Item
     (Tree : in out WisiToken.Syntax_Trees.Tree;
      Item : in     WisiToken.Valid_Node_Index)
     return WisiToken.Valid_Node_Index
   with Pre => Tree.ID (Item) = +IDENTIFIER_ID,
     Post => Tree.ID (Add_RHS_Item'Result) = +rhs_item_ID;

   function Add_RHS_Element
     (Tree  : in out WisiToken.Syntax_Trees.Tree;
      Data  : in     WisiToken_Grammar_Runtime.User_Data_Type;
      Item  : in     WisiToken.Valid_Node_Index;
      Label : in     Identifier_Token_Index := Invalid_Identifier_Token)
     return WisiToken.Valid_Node_Index
   with Pre => Tree.ID (Item) = +rhs_item_ID,
     Post => Tree.Production_ID (Add_RHS_Element'Result) =
             (+rhs_element_ID, (if Label = Invalid_Identifier_Token then 0 else 1));

   function Empty_RHS_Item_List
     (Tree : aliased in out WisiToken.Syntax_Trees.Tree)
     return WisiToken.Syntax_Trees.LR_Utils.List;

   function Empty_RHS_List
     (Tree : aliased in out WisiToken.Syntax_Trees.Tree)
     return WisiToken.Syntax_Trees.LR_Utils.List;

   function Add_RHS
     (Tree              : in out WisiToken.Syntax_Trees.Tree;
      Item              : in     WisiToken.Valid_Node_Index;
      Auto_Token_Labels : in     Boolean;
      Edited_Token_List : in     Boolean;
      Post_Parse_Action : in     WisiToken.Node_Index := WisiToken.Invalid_Node_Index;
      In_Parse_Action   : in     WisiToken.Node_Index := WisiToken.Invalid_Node_Index)
     return WisiToken.Valid_Node_Index
   with Pre => Tree.ID (Item) = +rhs_item_list_ID and
               (Post_Parse_Action = WisiToken.Invalid_Node_Index or else Tree.ID (Post_Parse_Action) = +ACTION_ID) and
               (In_Parse_Action = WisiToken.Invalid_Node_Index or else Tree.ID (In_Parse_Action) = +ACTION_ID),
     Post => Tree.ID (Add_RHS'Result) = +rhs_ID;

   function Find_Declaration
     (Data : in     WisiToken_Grammar_Runtime.User_Data_Type;
      Tree : in out WisiToken.Syntax_Trees.Tree;
      Name : in     String)
     return WisiToken.Node_Index
   with Post => Find_Declaration'Result = WisiToken.Invalid_Node_Index or else
                To_Token_Enum (Tree.ID (Find_Declaration'Result)) in declaration_ID | nonterminal_ID;
   --  Return the node that declares Name, Invalid_Node_Index if none.

   procedure Validate_Node
     (Tree                : in     WisiToken.Syntax_Trees.Tree;
      Node                : in     WisiToken.Valid_Node_Index;
      User_Data           : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Terminals           : in     WisiToken.Base_Token_Array_Access_Constant;
      Descriptor          : in     WisiToken.Descriptor;
      File_Name           : in     String;
      Error_Reported      : in out WisiToken.Node_Array_Booleans.Vector;
      Node_Image_Output   : in out Boolean;
      Node_Error_Reported : in out Boolean);
   --  Verify that all nodes match wisitoken_grammar.wy. Data must be of
   --  type WisiToken_Grammar_Runtime.User_Data_Type. Uses
   --  Data.EBNF_Allowed, Data.Error_Reported.
   --
   --  For use with Syntax_Trees.Validate_Tree.

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
