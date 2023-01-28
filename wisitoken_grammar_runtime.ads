--  Abstract :
--
--  Runtime utils for wisitoken_grammar.wy actions.
--
--  Copyright (C) 2018 - 2023 Free Software Foundation, Inc.
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

with Ada.Containers;
with SAL;
with WisiToken.BNF;
with WisiToken.Syntax_Trees;
with Wisitoken_Grammar_Actions;
package WisiToken_Grammar_Runtime is
   use all type WisiToken.Token_ID;
   use all type WisiToken.Syntax_Trees.Node_Access;
   use all type Wisitoken_Grammar_Actions.Token_Enum_ID;

   type Meta_Syntax is (Unknown, BNF_Syntax, EBNF_Syntax);
   --  Syntax used in grammar file.

   type Action_Phase is (Meta, Other);

   type User_Data_Type is new WisiToken.Syntax_Trees.User_Data_Type with
   record
      User_Lexer : WisiToken.BNF.Lexer_Type := WisiToken.BNF.None;
      --  Used to read the user language file, after user parser is generated;
      --  used now in '%if lexer' statements.

      User_Parser : WisiToken.BNF.Generate_Algorithm := WisiToken.BNF.None;
      --  Used to read the user language file; used now in '%if parser'
      --  statements.

      Generate_Set : WisiToken.BNF.Generate_Set_Access;
      --  As specified by %generate directives or command line.

      Phase : Action_Phase := Meta;
      --  Determines which actions Execute_Actions executes:
      --  Meta  - meta declarations, like %meta_syntax, %if, %generate
      --  Other - everything else

      EBNF_Ok : Boolean := False;
      --  Set True when don't need to translate EBNF to BNF.

      Meta_Syntax          : WisiToken_Grammar_Runtime.Meta_Syntax := Unknown;
      Raw_Code             : WisiToken.BNF.Raw_Code;
      Language_Params      : WisiToken.BNF.Language_Param_Type;
      Tokens               : aliased WisiToken.BNF.Tokens;

      Suppress : WisiToken.BNF.String_Pair_Lists.List;
      --  Declaration name, warning label; suppress warnings.

      Conflicts            : WisiToken.BNF.Conflict_Lists.List;
      McKenzie_Recover     : WisiToken.BNF.McKenzie_Recover_Param_Type;
      Max_Parallel         : SAL.Base_Peek_Type                    := 15;

      Rule_Count   : Integer                   := 0;
      Action_Count : Integer                   := 0;
      Check_Count  : Integer                   := 0;
      Label_Count  : Ada.Containers.Count_Type := 0;

      If_Lexer_Present  : Boolean := False;
      If_Parser_Present : Boolean := False;
      --  Set True by %if statements in Execute_Actions.

      Ignore_Lines : Boolean := False;
      --  An '%if' specified a different lexer, during Execute_Actions

      Error_Reported : WisiToken.Syntax_Trees.Node_Sets.Set;
      --  Used with Syntax_Trees.Validate_Tree.

   end record;
   type User_Data_Access is access all User_Data_Type;

   type Augmented is new WisiToken.Syntax_Trees.Base_Augmented with
   record
      EBNF : Boolean := False;

      Auto_Token_Labels : Boolean := False;
      --  Valid in an RHS node; True when token labels are generated by
      --  Translate_EBNF_To_BNF

      Edited_Token_List : Boolean := False;
      --  Valid in an RHS node; matches Wisitoken.BNF RHS.Edited_Token_List
   end record;
   type Augmented_Access is access all Augmented;
   type Augmented_Access_Constant is access constant Augmented;

   function Image (Item : in WisiToken.Syntax_Trees.Augmented_Class_Access_Constant) return String
   is (Augmented_Access_Constant (Item).EBNF'Image & " " &
         Augmented_Access_Constant (Item).Auto_Token_Labels'Image & " " &
         Augmented_Access_Constant (Item).Edited_Token_List'Image);

   overriding
   function Copy_Augmented
     (User_Data : in User_Data_Type;
      Augmented : in WisiToken.Syntax_Trees.Augmented_Class_Access)
     return WisiToken.Syntax_Trees.Augmented_Class_Access;

   overriding procedure Reset (Data : in out User_Data_Type);

   overriding
   procedure Initialize_Actions
     (Data : in out User_Data_Type;
      Tree : in WisiToken.Syntax_Trees.Tree'Class);

   function Get_Lexer_Set
     (User_Data     : in out User_Data_Type;
      Tree          : in out WisiToken.Syntax_Trees.Tree;
      Node          : in     WisiToken.Syntax_Trees.Valid_Node_Access)
     return WisiToken.BNF.Lexer_Set
   with Pre => To_Token_Enum (Tree.ID (Node)) in IDENTIFIER_ID | IDENTIFIER_BAR_list_ID;

   function Get_Generate_Algorithm_Set
     (User_Data : in out User_Data_Type;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Node      : in     WisiToken.Syntax_Trees.Valid_Node_Access)
     return WisiToken.BNF.Generate_Algorithm_Set
   with Pre => To_Token_Enum (Tree.ID (Node)) in IDENTIFIER_ID | IDENTIFIER_BAR_list_ID;

   procedure Start_If
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access);

   procedure End_If (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class);

   procedure Add_Declaration
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access);

   procedure Add_Nonterminal
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access);

   procedure Check_EBNF
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in     WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Token     : in     WisiToken.Positive_Index_Type);

   ----------
   --  Visible for WisiToken_Grammar_Editing

   function Get_Text
     (Virtual_Identifiers : in WisiToken.BNF.String_Arrays.Vector;
      Tree                : in WisiToken.Syntax_Trees.Tree;
      Tree_Index          : in WisiToken.Syntax_Trees.Node_Access;
      Strip_Quotes        : in Boolean := False)
     return String;
   --  If Tree_Index = Invalid_Node_Access, returns "<deleted child>".
   function Get_Text
     (Data         : in User_Data_Type;
      Tree         : in WisiToken.Syntax_Trees.Tree;
      Tree_Index   : in WisiToken.Syntax_Trees.Valid_Node_Access;
      Strip_Quotes : in Boolean := False)
     return String;
   --  Return source text for Tree_Index.
   --
   --  This fetches each token separately, without the non-grammar text.

   function Get_Item_Text
     (Data         : in User_Data_Type;
      Tree         : in WisiToken.Syntax_Trees.Tree;
      Node         : in WisiToken.Syntax_Trees.Valid_Node_Access;
      Strip_Quotes : in Boolean := False)
     return String
   with Pre => Tree.Is_Nonterm (Node);
   --  Find first descendant of Node that has rhs_item_ID, return source
   --  text for it.

   function Get_Code_Location_List
     (Tree    : in WisiToken.Syntax_Trees.Tree;
      Nonterm : in WisiToken.Syntax_Trees.Valid_Node_Access)
     return WisiToken.Syntax_Trees.Valid_Node_Access_Array
   with Pre => Tree.ID (Tree.Child (Nonterm, 3)) = +identifier_list_ID;
   --  Return location from a %code declaration.


end WisiToken_Grammar_Runtime;
