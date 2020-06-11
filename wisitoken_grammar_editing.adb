--  Abstract :
--
--  See spec.
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

with Ada.Characters.Handling;
with Ada.Containers;
with Ada.Exceptions;
with Ada.Text_IO;
with GNAT.Regexp;
with SAL.Generic_Decimal_Image;
with WisiToken.Generate;
with WisiToken.Lexer;
package body WisiToken_Grammar_Editing is

   use WisiToken;
   use Wisitoken_Grammar_Actions;
   use WisiToken_Grammar_Runtime;

   function To_Identifier_Token
     (Item      : in WisiToken.Token_Index;
      Terminals : in WisiToken.Base_Token_Array_Access_Constant)
     return Identifier_Token_Index
   is begin
      return
        (Label        => Shared_Terminal,
         Shared_Token => Item,
         Shared_ID    => Terminals.all (Item).ID);
   end To_Identifier_Token;

   function To_Identifier_Token
     (Item      : in WisiToken.Valid_Node_Index;
      Tree      : in WisiToken.Syntax_Trees.Tree;
      Terminals : in WisiToken.Base_Token_Array_Access_Constant)
     return Identifier_Token_Index
   is
      function From_Terminal (Item : in Valid_Node_Index) return Identifier_Token_Index
      is
      begin
         case WisiToken.Syntax_Trees.Terminal_Label'(Tree.Label (Item)) is
         when Shared_Terminal =>
            return To_Identifier_Token (Tree.First_Shared_Terminal (Item), Terminals);

         when Virtual_Terminal =>
            return (Virtual_Terminal, Tree.ID (Item));

         when Virtual_Identifier =>
            return (Virtual_Identifier, Tree.Identifier (Item), Tree.ID (Item), Null_Buffer_Region);
         end case;
      end From_Terminal;

   begin
      case To_Token_Enum (Tree.ID (Item)) is
      when rhs_element_ID =>
         return From_Terminal (Tree.First_Terminal (Tree.Find_Descendant (Item, +rhs_item_ID)));
      when rhs_item_ID =>
         return From_Terminal (Tree.First_Terminal (Item));
      when IDENTIFIER_ID =>
         return From_Terminal (Item);
      when others =>
         raise SAL.Programmer_Error;
      end case;
   end To_Identifier_Token;

   function Add_Identifier_Token
     (Tree      : in out WisiToken.Syntax_Trees.Tree;
      Item      : in     Identifier_Token_Index;
      Terminals : in     Base_Token_Array_Access_Constant)
     return Valid_Node_Index
   is begin
      case Item.Label is
      when Shared_Terminal =>
         return Tree.Add_Terminal (Item.Shared_Token, Terminals.all);

      when Virtual_Terminal =>
         return Tree.Add_Terminal (Item.Virtual_ID);

      when Virtual_Identifier =>
         return Tree.Add_Identifier (Item.Identifier_ID, Item.Identifier, Item.Byte_Region);
      end case;
   end Add_Identifier_Token;

   function Add_RHS_Item
     (Tree : in out WisiToken.Syntax_Trees.Tree;
      Item : in     WisiToken.Valid_Node_Index)
     return WisiToken.Valid_Node_Index
   is begin
      return Tree.Add_Nonterm ((+rhs_item_ID, 0), (1 => Item));
   end Add_RHS_Item;

   function Add_RHS_Element
     (Tree : in out WisiToken.Syntax_Trees.Tree;
      Item : in     WisiToken.Valid_Node_Index)
     return WisiToken.Valid_Node_Index
   is begin
      return Tree.Add_Nonterm ((+rhs_element_ID, 0), (1 => Item));
   end Add_RHS_Element;

   function Empty_RHS_Item_List
     (Tree : aliased in out WisiToken.Syntax_Trees.Tree)
     return WisiToken.Syntax_Trees.LR_Utils.List
   is begin
      return WisiToken.Syntax_Trees.LR_Utils.Creators.Empty_List
        (Tree,
         List_ID           => +rhs_item_list_ID,
         Multi_Element_RHS => 1,
         Element_ID        => +rhs_element_ID,
         Separator_ID      => Invalid_Token_ID);
   end Empty_RHS_Item_List;

   function To_RHS_Item_List
     (Tree         : aliased in out WisiToken.Syntax_Trees.Tree;
      List_Element :         in     Valid_Node_Index)
     return WisiToken.Syntax_Trees.LR_Utils.Constant_List
   with Pre => Tree.ID (List_Element) = +rhs_element_ID
   is
      use WisiToken.Syntax_Trees.LR_Utils;
      Result : List := Creators.Empty_List
        (Tree,
         List_ID           => +rhs_item_list_ID,
         Multi_Element_RHS => 1,
         Element_ID        => +rhs_element_ID,
         Separator_ID      => Invalid_Token_ID);
   begin
      Result.Append (List_Element);
      return Constant_List (Result);
   end To_RHS_Item_List;

   function Empty_RHS_List
     (Tree : aliased in out WisiToken.Syntax_Trees.Tree)
     return WisiToken.Syntax_Trees.LR_Utils.List
   is begin
      return WisiToken.Syntax_Trees.LR_Utils.Creators.Empty_List
        (Tree,
         List_ID           => +rhs_list_ID,
         Multi_Element_RHS => 1,
         Element_ID        => +rhs_ID,
         Separator_ID      => +BAR_ID);
   end Empty_RHS_List;

   function Add_RHS
     (Tree              : in out WisiToken.Syntax_Trees.Tree;
      Item              : in     WisiToken.Valid_Node_Index;
      Post_Parse_Action : in     WisiToken.Node_Index := WisiToken.Invalid_Node_Index;
      In_Parse_Action   : in     WisiToken.Node_Index := WisiToken.Invalid_Node_Index)
     return WisiToken.Valid_Node_Index
   is begin
      if In_Parse_Action = Invalid_Node_Index then
         if Post_Parse_Action = Invalid_Node_Index then
            return Tree.Add_Nonterm ((+rhs_ID, 1), (1 => Item));
         else
            return Tree.Add_Nonterm ((+rhs_ID, 2), (Item, Post_Parse_Action));
         end if;
      else
         if Post_Parse_Action = Invalid_Node_Index then
            return Tree.Add_Nonterm ((+rhs_ID, 3), (Item, Tree.Add_Terminal (+ACTION_ID), In_Parse_Action));
         else
            return Tree.Add_Nonterm ((+rhs_ID, 3), (Item, Post_Parse_Action, In_Parse_Action));
         end if;
      end if;
   end Add_RHS;

   function Empty_RHS
     (Tree : in out WisiToken.Syntax_Trees.Tree)
     return WisiToken.Valid_Node_Index
   is begin
      return Tree.Add_Nonterm ((+rhs_ID, 0), (1 .. 0 => Deleted_Child));
   end Empty_RHS;

   function Find_Declaration
     (Data : in     User_Data_Type;
      Tree : in out WisiToken.Syntax_Trees.Tree;
      Name : in     String)
     return WisiToken.Node_Index
   is
      use WisiToken.Syntax_Trees.LR_Utils;
      use WisiToken.Syntax_Trees.LR_Utils.Creators;

      function Decl_Name (Decl : in Valid_Node_Index) return String
      is begin
         case To_Token_Enum (Tree.ID (Decl)) is
         when declaration_ID =>
            case Tree.RHS_Index (Decl) is
            when 0 =>
               return Get_Text (Data, Tree, Tree.Child (Decl, 3));

            when 2 | 3 =>
               return Get_Text (Data, Tree, Tree.Child (Decl, 2));

            when others =>
               return "";
            end case;

         when nonterminal_ID =>
            return Get_Text (Data, Tree, Tree.Child (Decl, 1));

         when others =>
            return "";
         end case;
      end Decl_Name;

      --  Tree.Root is wisitoken_accept
      List : constant Constant_List := Create_List
        (Tree, Tree.Child (Tree.Root, 1), +compilation_unit_list_ID, +compilation_unit_ID);
   begin
      for N of List loop
         declare
            Decl : constant Valid_Node_Index := Tree.Child (N, 1);
         begin
            if Name = Decl_Name (Decl) then
               return Decl;
            end if;
         end;
      end loop;
      return Invalid_Node_Index;
   end Find_Declaration;

   procedure Translate_EBNF_To_BNF
     (Tree : in out WisiToken.Syntax_Trees.Tree;
      Data : in out User_Data_Type)
   is
      use all type SAL.Base_Peek_Type;
      use WisiToken.Syntax_Trees;

      Copied_EBNF_Nodes : WisiToken.Valid_Node_Index_Arrays.Vector;

      Symbol_Regexp : constant GNAT.Regexp.Regexp := GNAT.Regexp.Compile
        ((if Data.Language_Params.Case_Insensitive
          then "[A-Z0-9_]+"
          else "[a-zA-Z0-9_]+"),
         Case_Sensitive => not Data.Language_Params.Case_Insensitive);

      procedure Erase_Copied_EBNF_Node (Node : in Valid_Node_Index)
      is
         use Ada.Text_IO;
         Found : Boolean := False;
      begin
         if Trace_Generate_EBNF > Outline then
            Put_Line ("erase copied deleted EBNF node" & Node'Image);
         end if;
         --  Vector Delete replaces content with
         --  Valid_Node_Index_Arrays.Default_Element = Valid_Node_Index'Last =
         --  Deleted_Child; this is clearer.

         for I in Copied_EBNF_Nodes.First_Index .. Copied_EBNF_Nodes.Last_Index loop
            if Copied_EBNF_Nodes (I) = Node then
               Copied_EBNF_Nodes (I) := Deleted_Child;
               Found := True;
               exit;
            end if;
         end loop;
         if not Found then
            Put_Line (Current_Error, Tree.Image
                        (Node, Wisitoken_Grammar_Actions.Descriptor, Node_Numbers => True) &
                        " not found in Copied_EBNF_Nodes");
            raise SAL.Programmer_Error;
         end if;
      end Erase_Copied_EBNF_Node;

      procedure Clear_EBNF_Node (Node : in Valid_Node_Index)
      is begin
         if Node in Data.EBNF_Nodes.First_Index .. Data.EBNF_Nodes.Last_Index then
            if Trace_Generate_EBNF > Outline then
               Ada.Text_IO.Put_Line ("clear translated EBNF node" & Node'Image);
            end if;

            Data.EBNF_Nodes (Node) := False;
         else
            Erase_Copied_EBNF_Node (Node);
         end if;
      end Clear_EBNF_Node;

      function New_Identifier (Text : in String) return Identifier_Index
      is
         ID : constant Identifier_Index := Base_Identifier_Index (Data.Tokens.Virtual_Identifiers.Length) + 1;
      begin
         Data.Tokens.Virtual_Identifiers.Append (+Text);
         return ID;
      end New_Identifier;

      Keyword_Ident : constant Identifier_Index := New_Identifier ("keyword");
      Percent_Ident : constant Identifier_Index := New_Identifier ("percent");

      function Next_Nonterm_Name (Prefix : in String := "nonterminal") return Identifier_Index
      is
         function Image is new SAL.Generic_Decimal_Image (Identifier_Index);
         ID : constant Identifier_Index := Identifier_Index (Data.Tokens.Virtual_Identifiers.Length) + 1;
      begin

         if ID > 999 then
            --  We assume 3 digits below
            raise SAL.Programmer_Error with "more than 3 digits needed for virtual identifiers in EBNF translate";
         end if;

         Data.Tokens.Virtual_Identifiers.Append (+(Prefix & "_" & Image (ID, Width => 3)));

         return ID;
      end Next_Nonterm_Name;

      function Find_Nonterminal
        (Target : in String;
         Equal  : in WisiToken.Syntax_Trees.LR_Utils.Find_Equal)
        return Node_Index
      is
         use WisiToken.Syntax_Trees.LR_Utils;
      begin
         return Get_Node
           (Creators.Create_List
              (Tree, Tree.Child (Tree.Root, 1), +compilation_unit_list_ID, +compilation_unit_ID).Find
              (Target, Equal));
      end Find_Nonterminal;

      function Tree_Add_Nonterminal
        (Child_1 : in Valid_Node_Index;
         Child_2 : in Valid_Node_Index;
         Child_3 : in Valid_Node_Index;
         Child_4 : in Valid_Node_Index)
        return Valid_Node_Index
      is begin
         --  Work around GNAT error about arbitrary evaluation order in
         --  aggregates (no error about the arbitrary order in subprogram
         --  parameter_assocation_lists!).
         return Tree.Add_Nonterm
           (Production => (+nonterminal_ID, 0),
            Children   => (Child_1, Child_2, Child_3, Child_4),
            Action     => Wisitoken_Grammar_Actions.nonterminal_0'Access);
      end Tree_Add_Nonterminal;

      function Duplicate
        (List        : in Syntax_Trees.LR_Utils.List;
         New_Content : in Node_Index)
        return Boolean
      is
         --  We don't require New_Content.ID = List.Element_ID; since we are
         --  comparing result of Get_Text.
         New_Content_Str : constant String :=
           (if New_Content = Invalid_Node_Index
            then "" --  Empty RHS
            else Get_Text (Data, Tree, New_Content));
      begin
         for N of List loop
            if New_Content_Str = Get_Text (Data, Tree, N) then
               return True;
            end if;
         end loop;
         return False;
      end Duplicate;

      procedure Insert_Empty_RHS
        (RHS_List : in out WisiToken.Syntax_Trees.LR_Utils.List;
         After    : in     Valid_Node_Index)
      with Pre => RHS_List.List_ID = +rhs_list_ID and RHS_List.Element_ID = +rhs_ID and
                  Tree.ID (After) = +rhs_ID and RHS_List.Contains (After)
      is begin
         RHS_List.Insert
           (New_Element => Tree.Add_Nonterm
              ((+rhs_ID, 0),
               (1 .. 0 => Invalid_Node_Index)),
            After => RHS_List.To_Cursor (After));
      end Insert_Empty_RHS;

      procedure Insert_RHS
        (RHS_List          : in out WisiToken.Syntax_Trees.LR_Utils.List;
         New_RHS_Item_List : in     Valid_Node_Index;
         After             : in     Valid_Node_Index)
      with Pre => RHS_List.List_ID = +rhs_list_ID and RHS_List.Element_ID = +rhs_ID and
                  Tree.ID (New_RHS_Item_List) = +rhs_item_list_ID and
                  Tree.ID (After) = +rhs_ID and RHS_List.Contains (After)
      is begin
         RHS_List.Insert
           (New_Element => Tree.Add_Nonterm
              (Production => (+rhs_ID, Tree.RHS_Index (After)),
               Children =>
                 (case Tree.RHS_Index (After) is
                  when 1 => (1 => New_RHS_Item_List),
                  when 2 => (New_RHS_Item_List, Tree.Copy_Subtree (Tree.Child (After, 2))),
                  when 3 => (New_RHS_Item_List,
                             Tree.Copy_Subtree (Tree.Child (After, 2)),
                             Tree.Copy_Subtree (Tree.Child (After, 3))),
                  when others => raise SAL.Programmer_Error)),
            After => RHS_List.To_Cursor (After));
      end Insert_RHS;

      procedure Record_Copied_EBNF_Nodes (Node : in Valid_Node_Index)
      is
         procedure Record_Copied_Node
           (Tree : in out WisiToken.Syntax_Trees.Tree;
            Node : in WisiToken.Valid_Node_Index)
         is begin
            if To_Token_Enum (Tree.ID (Node)) in
              rhs_optional_item_ID |
              rhs_multiple_item_ID |
              rhs_group_item_ID |
              rhs_attribute_ID |
              STRING_LITERAL_2_ID
            then
               if Trace_Generate_EBNF > Outline then
                  Ada.Text_IO.Put_Line
                    ("new EBNF node " & Tree.Image
                       (Node, Wisitoken_Grammar_Actions.Descriptor,
                        Node_Numbers => True));
               end if;
               Copied_EBNF_Nodes.Append (Node);
            end if;
         end Record_Copied_Node;
      begin
         Tree.Process_Tree (Record_Copied_Node'Access, Node);
      end Record_Copied_EBNF_Nodes;

      procedure Erase_Deleted_EBNF_Nodes (Node : in Valid_Node_Index)
      is
         procedure Erase_Deleted_Node
           (Tree : in out WisiToken.Syntax_Trees.Tree;
            Node : in WisiToken.Valid_Node_Index)
         is begin
            if To_Token_Enum (Tree.ID (Node)) in
              rhs_optional_item_ID |
              rhs_multiple_item_ID |
              rhs_group_item_ID |
              rhs_attribute_ID |
              STRING_LITERAL_2_ID
            then
               if Node in Data.EBNF_Nodes.First_Index .. Data.EBNF_Nodes.Last_Index then
                  --  Node is original, not copied
                  if Trace_Generate_EBNF > Outline then
                     Ada.Text_IO.Put_Line ("erase original deleted EBNF node" & Node'Image);
                  end if;
                  Data.EBNF_Nodes (Node) := False;
               else
                  Erase_Copied_EBNF_Node (Node);
               end if;
            end if;
         end Erase_Deleted_Node;
      begin
         Tree.Process_Tree (Erase_Deleted_Node'Access, Node);
      end Erase_Deleted_EBNF_Nodes;

      function Insert_Optional_RHS (B : in Valid_Node_Index) return Valid_Node_Index
      with Pre => Tree.ID (B) in +rhs_multiple_item_ID | +rhs_optional_item_ID | +IDENTIFIER_ID
      is
         --  B is an optional item in an rhs_item_list:
         --  | A B? C
         --
         --  or B is a rhs_multiple_item that is allowed to be empty:
         --  | A B* C
         --
         --  or B is a virtual identifier naming the new nonterm replacing the
         --  original
         --
         --  A, C can be empty. The containing element may be rhs or
         --  rhs_alternative_list.
         --
         --  Insert either a second rhs, or a second rhs_item_list, after the
         --  one containing B, without B.
         --
         --  Return the List_Root of the edited list.

         use Syntax_Trees.LR_Utils;
         use Syntax_Trees.LR_Utils.Creators;
         use all type Ada.Containers.Count_Type;

         function Find_Skips return Skip_Info
         is
            Non_Empty_List : Node_Index := Invalid_Node_Index;
            --  First (nearest) rhs_item_list ancestor of B that will not be empty
            --  when B is skipped.

            Skip_Last        : Positive_Index_Type'Base := Positive_Index_Type'First;
            Last_Skip_Node   : Valid_Node_Index         := Tree.Find_Ancestor (B, +rhs_element_ID);
            Reset_Search_For : WisiToken.Token_ID       := +rhs_item_list_ID;

            procedure Search (Result : in out Skip_Info)
            is
               Skip_Node  : Valid_Node_Index   := Last_Skip_Node;
               Search_For : WisiToken.Token_ID := Reset_Search_For;
            begin
               loop
                  case To_Token_Enum (Search_For) is
                  when rhs_item_list_ID =>
                     Skip_Node := Tree.Find_Ancestor (Skip_Node, +rhs_item_list_ID);

                     Skip_Node := List_Root (Tree, Skip_Node, +rhs_item_list_ID);

                     Search_For := +rhs_element_ID;

                     if Result.Skips'Length = 0 then
                        declare
                           List_Count : constant Ada.Containers.Count_Type := Create_List
                             (Tree, Skip_Node, +rhs_item_list_ID, +rhs_element_ID).Count;
                        begin
                           if List_Count > 1 then
                              Non_Empty_List := List_Root (Tree, Skip_Node, +rhs_item_list_ID);
                              exit;

                           elsif Skip_Last = Positive_Index_Type'First and List_Count = 1 then
                              --  This list will be empty; no need to descend into it
                              Last_Skip_Node   := Skip_Node;
                              Reset_Search_For := Search_For;
                           else
                              Skip_Last := Skip_Last + 1;
                           end if;
                        end;
                     else
                        Result.Skips (Skip_Last) :=
                          (Label             => Nested,
                           Element           => Skip_Node,
                           List_Root         => Skip_Node,
                           List_ID           => +rhs_item_list_ID,
                           Element_ID        => +rhs_element_ID,
                           Separator_ID      => Invalid_Token_ID,
                           Multi_Element_RHS => 1);

                        Skip_Last := Skip_Last - 1;
                     end if;

                  when rhs_element_ID =>
                     declare
                        List_Node : Valid_Node_Index := Tree.Find_Ancestor
                          (Skip_Node, (+rhs_ID, +rhs_alternative_list_ID));
                     begin

                        if Result.Skips'Length = 0 and then
                          Tree.ID (List_Node) = +rhs_ID
                        then
                           Non_Empty_List := List_Root (Tree, Skip_Node, +rhs_item_list_ID);
                           Skip_Last      := Skip_Last - 1;
                           exit;
                        end if;

                        List_Node := List_Root (Tree, List_Node, +rhs_alternative_list_ID);
                        Skip_Node := Tree.Find_Ancestor (Skip_Node, +rhs_element_ID);

                        Search_For := +rhs_item_list_ID;

                        if Result.Skips'Length = 0 then
                           if Skip_Last = Positive_Index_Type'First then
                              --  This list will be empty; no need to descend into it
                              Last_Skip_Node   := Skip_Node;
                              Reset_Search_For := Search_For;
                           else
                              Skip_Last := Skip_Last + 1;
                           end if;
                        else
                           Result.Skips (Skip_Last) :=
                             (Label             => Nested,
                              Element           => Skip_Node,
                              List_Root         => List_Node,
                              List_ID           => +rhs_alternative_list_ID,
                              Element_ID        => +rhs_item_list_ID,
                              Separator_ID      => +BAR_ID,
                              Multi_Element_RHS => 1);

                           Skip_Last := Skip_Last - 1;
                        end if;
                     end;
                  when others =>
                     raise SAL.Programmer_Error;
                  end case;

               end loop;
            end Search;

            Result_1 : Skip_Info (Skip_Last => Positive_Index_Type'First - 1);
         begin
            --  First count the number of Skip_Items we need, and set
            --  Non_Empty_List.
            Search (Result_1);

            declare
               Result : Skip_Info (Skip_Last);
            begin
               if Result.Skips'Length = 0 then
                  return Result;
               end if;

               Result.Start_List_Root  := Non_Empty_List;
               Result.Start_List_ID    := +rhs_item_list_ID;
               Result.Start_Element_ID := +rhs_element_ID;

               Result.Start_Separator_ID      := Invalid_Token_ID;
               Result.Start_Multi_Element_RHS := 1;

               Result.Skips (Skip_Last) := (Skip, Last_Skip_Node);

               if Result.Skips'Length = 1 then
                  return Result;
               end if;

               Search (Result);
               return Result;
            end;
         end Find_Skips;

         Container : Valid_Node_Index := Tree.Find_Ancestor (B, (+rhs_ID, +rhs_alternative_list_ID));
         Container_ID : WisiToken.Token_ID := Tree.ID (Container);

         Container_List : Syntax_Trees.LR_Utils.List :=
           (if Container_ID = +rhs_ID
            then Create_From_Element
              (Tree,
               Element      => Container,
               List_ID      => +rhs_list_ID,
               Element_ID   => +rhs_ID,
               Separator_ID => +BAR_ID)
            else Create_List
              (Tree,
               Root         => List_Root (Tree, Container, +rhs_alternative_list_ID),
               List_ID      => +rhs_alternative_list_ID,
               Element_ID   => +rhs_item_list_ID,
               Separator_ID => +BAR_ID));

      begin
         if Trace_Generate_EBNF > Extra then
            Ada.Text_IO.New_Line;
            Ada.Text_IO.Put_Line ("Insert_Optional_RHS start: " & Get_Text (Data, Tree, Container));
            Tree.Print_Tree (Wisitoken_Grammar_Actions.Descriptor, Container);
         end if;

         declare
            Skip_List : constant Skip_Info := Find_Skips;

            New_RHS_AC   : Node_Index := Invalid_Node_Index;
            Is_Duplicate : Boolean    := False;
         begin
            if WisiToken.Trace_Generate_EBNF > Extra then
               Ada.Text_IO.New_Line;
               Ada.Text_IO.Put_Line ("skip: " & Image (Skip_List, Wisitoken_Grammar_Actions.Descriptor));
            end if;

            if Skip_List.Skips'Length = 0 or else
              +rhs_ID = Tree.ID (Tree.Parent (Skip_List.Start_List_Root))
            then
               --  Insert an edited rhs into the rhs_list.
               --
               --  We can't insert an empty rhs_item_list into an
               --  rhs_alterative_list, so we insert an empty rhs.

               if Container_ID = +rhs_alternative_list_ID then

                  Container := Tree.Find_Ancestor (B, +rhs_ID);

                  Container_ID := +rhs_ID;

                  Container_List := Create_From_Element
                    (Tree,
                     Element      => Container,
                     List_ID      => +rhs_list_ID,
                     Element_ID   => +rhs_ID,
                     Separator_ID => +BAR_ID);
               end if;

               if Skip_List.Skips'Length = 0 then
                  --  New rhs is empty; no rhs_item_list
                  null;
               else
                  New_RHS_AC := Copy_Skip_Nested (Skip_List, Tree);
               end if;

               if Duplicate (Container_List, New_RHS_AC) then
                  Is_Duplicate := True;
               else
                  if Skip_List.Skips'Length = 0 then
                     Insert_Empty_RHS (Container_List, Container);
                  else
                     Insert_RHS (Container_List, New_RHS_AC, After => Container);
                  end if;
               end if;

            else
               --  Insert an edited rhs_item_list into an rhs_alternative_list

               New_RHS_AC := Copy_Skip_Nested (Skip_List, Tree);

               if Duplicate (Container_List, New_RHS_AC) then
                  --  IMPROVEME: check for duplicate before do copy; requires version of
                  --  Get_Text that understands Skip_Info
                  Is_Duplicate := True;
               else
                  declare
                     After : Valid_Node_Index := B;
                  begin
                     loop
                        After := List_Root (Tree, Tree.Find_Ancestor (After, +rhs_item_list_ID), +rhs_item_list_ID);
                        exit when Container_List.Contains (After);
                     end loop;

                     Container_List.Insert
                       (New_Element => New_RHS_AC,
                        After       => Container_List.To_Cursor (After));
                  end;
               end if;
            end if;

            if Trace_Generate_EBNF > Detail then
               Ada.Text_IO.New_Line;
               if Is_Duplicate then
                  Ada.Text_IO.Put_Line ("Insert_Optional_RHS duplicate '" & Get_Text (Data, Tree, New_RHS_AC) & "'");
               else
                  if Container_ID = +rhs_ID then
                     Ada.Text_IO.Put_Line
                       ("Insert_Optional_RHS old rhs, new rhs: " & Get_Text (Data, Tree, Container_List.Root));
                     Tree.Print_Tree (Wisitoken_Grammar_Actions.Descriptor, Container_List.Root);
                  else
                     Ada.Text_IO.Put_Line
                       ("Insert_Optional_RHS edited rhs_alternative_list: " & Get_Text
                          (Data, Tree, Tree.Parent (Container_List.Root)));
                     Tree.Print_Tree (Wisitoken_Grammar_Actions.Descriptor, Tree.Parent (Container_List.Root));
                  end if;
               end if;
            end if;

            if not (Skip_List.Skips'Length = 0 or Is_Duplicate) then
               Record_Copied_EBNF_Nodes (New_RHS_AC);
            end if;
         end;
         return Container_List.Root;
      end Insert_Optional_RHS;

      procedure Add_Compilation_Unit (Label : in String; Unit : in Valid_Node_Index; Prepend : in Boolean := False)
      with Pre => Tree.ID (Unit) in +declaration_ID | +nonterminal_ID
      is
         use WisiToken.Syntax_Trees.LR_Utils;

         List : Syntax_Trees.LR_Utils.List := Creators.Create_List
           (Tree, Tree.Child (Tree.Root, 1), +compilation_unit_list_ID, +compilation_unit_ID, Invalid_Token_ID);

         Comp_Unit : constant Valid_Node_Index := Tree.Add_Nonterm
           ((+compilation_unit_ID, (if Tree.ID (Unit) = +declaration_ID then 0 else 1)),
            (1 => Unit));

         function Equal
           (Target    : in String;
            List      : in LR_Utils.Constant_List'Class;
            Comp_Unit : in Valid_Node_Index)
           return Boolean
         is
            pragma Unreferenced (List);
            Decl : constant Valid_Node_Index := Tree.Child (Comp_Unit, 1);
         begin
            return Tree.ID (Decl) = +declaration_ID and then Target =
              (case Tree.RHS_Index (Decl) is
               when 0      => Get_Text (Data, Tree, Tree.Child (Decl, 3)),
               when 2 | 3  => Get_Text (Data, Tree, Tree.Child (Decl, 2)),
               when others => "");
         end Equal;

      begin
         if Prepend then
            --  Prepend is true for keywords, which must be declared before they
            --  are used. We put them all after the %meta_syntax declaration, to
            --  closer match the likely original EBNF layout.
            declare
               Meta_Syntax : constant Cursor := List.Find ("meta_syntax", Equal'Unrestricted_Access);
            begin
               List.Insert (Comp_Unit, After => Meta_Syntax);
            end;
         else
            List.Append (Comp_Unit);
         end if;

         if Trace_Generate_EBNF > Outline then
            Ada.Text_IO.New_Line;
            Ada.Text_IO.Put_Line ("new " & Label & ":" & Comp_Unit'Image & ": '" & Get_Text (Data, Tree, Unit) & "'");
            if Trace_Generate_EBNF > Extra then
               Tree.Print_Tree (Wisitoken_Grammar_Actions.Descriptor, Comp_Unit);
            end if;
         end if;
      end Add_Compilation_Unit;

      function To_RHS_List (RHS_Element : in Valid_Node_Index) return Valid_Node_Index
      with Pre => Tree.ID (RHS_Element) = +rhs_element_ID
      is
         RHS_Item_List : constant Valid_Node_Index := Tree.Add_Nonterm ((+rhs_item_list_ID, 0), (1 => RHS_Element));
         RHS           : constant Valid_Node_Index := Tree.Add_Nonterm ((+rhs_ID, 1),           (1 => RHS_Item_List));
      begin
         return Tree.Add_Nonterm ((+rhs_list_ID, 0), (1 => RHS));
      end To_RHS_List;

      function Convert_RHS_Alternative (Content : in Valid_Node_Index) return Valid_Node_Index
      with Pre => Tree.ID (Content) = +rhs_alternative_list_ID
      is
         --  Convert rhs_alternative_list rooted at Content to an rhs_list
         Node : Valid_Node_Index := Content;
      begin
         loop
            exit when Tree.RHS_Index (Node) = 0;

            --  current tree:
            --  rhs_alternative_list : Node
            --  | rhs_alternative_list: Node.Child (1)
            --  | |  ...
            --  | BAR: Node.child (2)
            --  | rhs_item_list: Node.Child (3)

            --  new tree:
            --  rhs_list: Node
            --  | rhs_alternative_list: keep Node.Child (1)
            --  | |  ...
            --  | BAR: keep
            --  | rhs: new
            --  | | rhs_item_list: keep Node,Child (3)

            if not Tree.Has_Children (Tree.Child (Node, 3)) then
               --  Convert empty rhs_item_list to empty rhs
               Tree.Set_Children
                 (Tree.Child (Node, 3),
                  (+rhs_ID, 0),
                  (1 .. 0 => Invalid_Node_Index));

               Tree.Set_Children
                 (Node,
                  (+rhs_list_ID, 1),
                  (1 => Tree.Child (Node, 1),
                   2 => Tree.Child (Node, 2),
                   3 => Tree.Child (Node, 3)));
            else
               Tree.Set_Children
                 (Node,
                  (+rhs_list_ID, 1),
                  (1 => Tree.Child (Node, 1),
                   2 => Tree.Child (Node, 2),
                   3 => Tree.Add_Nonterm
                     ((+rhs_ID, 1),
                      (1 => Tree.Child (Node, 3)))));
            end if;

            Node := Tree.Child (Node, 1);
         end loop;

         --  current tree:
         --  rhs_alternative_list : Node
         --  | rhs_item_list: Node.Child (1)

         --  new tree:
         --  rhs_list: Node
         --  | rhs: new
         --  | | rhs_item_list: Node.Child (1)

         Tree.Set_Children
           (Node,
            (+rhs_list_ID, 0),
            (1 => Tree.Add_Nonterm ((+rhs_ID, 1), (1 => Tree.Child (Node, 1)))));

         return Content;
      end Convert_RHS_Alternative;

      procedure New_Nonterminal
        (Label          : in String;
         New_Identifier : in Identifier_Index;
         Content        : in Valid_Node_Index)
      with Pre => To_Token_Enum (Tree.ID (Content)) in rhs_alternative_list_ID | rhs_element_ID
      is
         --  Convert subtree rooted at Content to an rhs_list contained by a new nonterminal
         --  named New_Identifier.
      begin
         declare
            New_Nonterm : constant Valid_Node_Index := Tree_Add_Nonterminal
              (Child_1   => Tree.Add_Identifier (+IDENTIFIER_ID, New_Identifier, Tree.Byte_Region (Content)),
               Child_2   => Tree.Add_Terminal (+COLON_ID),
               Child_3   =>
                 (case To_Token_Enum (Tree.ID (Content)) is
                  when rhs_element_ID          => To_RHS_List (Content),
                  when rhs_alternative_list_ID => Convert_RHS_Alternative (Content),
                  when others => raise SAL.Programmer_Error),
               Child_4   => Tree.Add_Nonterm
                 ((+semicolon_opt_ID, 0),
                  (1     => Tree.Add_Terminal (+SEMICOLON_ID))));
         begin
            Add_Compilation_Unit (Label & New_Identifier'Image, New_Nonterm);
         end;
      end New_Nonterminal;

      procedure New_Nonterminal_List
        (List_Nonterm         : in Identifier_Token_Index;
         RHS_Item_List_1_Root : in Valid_Node_Index;
         Separator            : in Identifier_Token_Index)
      with Pre => Tree.ID (RHS_Item_List_1_Root) = +rhs_item_list_ID
      is
         use WisiToken.Syntax_Trees.LR_Utils;

         RHS_Item_List_1 : constant Constant_List := Creators.Create_List
           (Tree, RHS_Item_List_1_Root, +rhs_item_list_ID, +rhs_element_ID);

         RHS_Item_List_2 : List := Empty_RHS_Item_List (Tree);
         RHS_List        : List := Empty_RHS_List (Tree);
      begin
         RHS_Item_List_2.Append
           (Add_RHS_Element
              (Tree, Add_RHS_Item (Tree, Add_Identifier_Token (Tree, List_Nonterm, Data.Terminals))));

         if Separator /= Invalid_Identifier_Token then
            RHS_Item_List_2.Append
              (Add_RHS_Element
                 (Tree, Add_RHS_Item (Tree, Add_Identifier_Token (Tree, Separator, Data.Terminals))));
         end if;

         for Element of RHS_Item_List_1 loop
            RHS_Item_List_2.Append (Tree.Copy_Subtree (Element));
         end loop;

         RHS_List.Append (Add_RHS (Tree, RHS_Item_List_1.Root));
         RHS_List.Append (Add_RHS (Tree, RHS_Item_List_2.Root));

         Add_Compilation_Unit
           ("canonical list",
            Tree_Add_Nonterminal
              (Child_1 => Add_Identifier_Token (Tree, List_Nonterm, Data.Terminals),
               Child_2 => Tree.Add_Terminal (+COLON_ID),
               Child_3 => RHS_List.Root,
               Child_4 => Tree.Add_Nonterm
                 ((+semicolon_opt_ID, 0),
                  (1   => Tree.Add_Terminal (+SEMICOLON_ID)))));
      end New_Nonterminal_List;

      procedure New_Nonterminal_List
        (List_Nonterm : in Identifier_Token_Index;
         List_Element : in Identifier_Token_Index;
         Separator    : in Identifier_Token_Index)
      with Pre => ID (List_Element) = +IDENTIFIER_ID
      is
         --  Add a nonterminal declaration for a canonical list:
         --
         --  foo_list ;; List_Nonterm
         --  : foo ;; List_Element
         --  | foo_list separator foo ;; List_Nonterm Separator List_Element

         use WisiToken.Syntax_Trees.LR_Utils;

         RHS_Item_List_1 : List := Empty_RHS_Item_List (Tree);
      begin
         RHS_Item_List_1.Append
           (Add_RHS_Element (Tree, Add_RHS_Item (Tree, Add_Identifier_Token (Tree, List_Element, Data.Terminals))));
         New_Nonterminal_List (List_Nonterm, RHS_Item_List_1.Root, Separator);
      end New_Nonterminal_List;

      function List_Matches
        (N                 : in Valid_Node_Index;
         Separator_Content : in String;
         Element_Content   : in String)
        return Node_Index
      with Pre => Element_Content'Length > 0
      --  Return True if the declaration at N is a nonterminal for a
      --  canonical list matching Separator_Content, Element_Content.
      is
         use WisiToken.Syntax_Trees.LR_Utils;
         use all type Ada.Containers.Count_Type;
      begin
         if Tree.ID (N) = +nonterminal_ID then
            declare
               --  Target List_Nonterm is:
               --
               --  list_nonterm
               --     : element
               --     | list_nonterm separator? element
               --
               --  nonterminal: N
               --  | IDENTIFIER : Name_Node
               --  | COLON
               --  | rhs_list: RHS_List
               --  | | rhs_list:
               --  | | | rhs
               --  | | | | ... list_element
               --  | | BAR
               --  | | rhs: ... list_nonterm separator? list_element

               Name_Node : constant Node_Index    := Tree.Child (N, 1);
               RHS_List  : constant Constant_List := Creators.Create_List
                 (Tree, Tree.Child (N, 3), +rhs_list_ID, +rhs_ID);
            begin
               if RHS_List.Count = 2 then
                  declare
                     RHS_1 : constant String := Get_Text (Data, Tree, Get_Node (RHS_List.First));
                     RHS_2 : constant String := Get_Text (Data, Tree, Get_Node (RHS_List.Last));
                     Expected_RHS_2 : constant String := Get_Text (Data, Tree, Name_Node) & " " &
                       Separator_Content & (if Separator_Content = "" then "" else " ") & Element_Content;
                  begin
                     if Element_Content = RHS_1 and RHS_2 = Expected_RHS_2 then
                        --  Found a match.
                        return Name_Node;
                     end if;
                  end;
               end if;
            end;
         end if;
         return Invalid_Node_Index;
      end List_Matches;

      function Find_List_Nonterminal_1
        (Separator_Content : in String;
         Element_Content   : in String)
        return Identifier_Token_Index
      with Pre => Element_Content'Length > 0
      --  Search for a nonterminal declaration (virtual or not) implementing
      --  a list matching Separator_Content, Element_Content. If found,
      --  return an identifier_token for it. Otherwise, return
      --  Invalid_Identifier_Token.
      is
         use WisiToken.Syntax_Trees.LR_Utils;

         List : constant Constant_List := Creators.Create_List
           (Tree, Tree.Child (Tree.Root, 1), +compilation_unit_list_ID, +compilation_unit_ID);
      begin
         return List_Nonterm_Name : Identifier_Token_Index := Invalid_Identifier_Token do
            for N of List loop
               declare
                  Name_Node : constant Node_Index := List_Matches
                    (Tree.Child (N, 1), Separator_Content, Element_Content);
               begin
                  if Name_Node /= Invalid_Node_Index then
                     List_Nonterm_Name := To_Identifier_Token (Name_Node, Tree, Data.Terminals);
                     exit;
                  end if;
               end;
            end loop;
         end return;
      end Find_List_Nonterminal_1;

      function Maybe_New_Nonterminal_List
        (List_Nonterm_String   : in String;
         Element               : in Valid_Node_Index;
         Erase_EBNF_In_Element : in Boolean;
         Separator             : in Node_Index)
        return Identifier_Token_Index
      with Pre => To_Token_Enum (Tree.ID (Element)) in rhs_item_list_ID | rhs_element_ID | rhs_item_ID | IDENTIFIER_ID
      --  If there is an existing nonterminal matching List_Nonterm_String,
      --  Element_Content, Separator, return an identifier_token for it.
      --  Otherwise, create a new list nonterminal, return an
      --  identifier_token for that.
      --
      --  If Erase_EBNF_In_Element, and an existing list is returned,
      --  Ease_Deleted_EBNF_Nodes is called on Element.
      is
         Existing_Decl : constant Node_Index := Find_Declaration (Data, Tree, List_Nonterm_String);

         Element_Content : constant String := Get_Text (Data, Tree, Element); --  IMPROVEME: ignore token labels.

         Separator_Content : constant String :=
           (if Separator = Invalid_Node_Index
            then ""
            else Get_Item_Text (Data, Tree, Separator));

         Separator_Ident_Tok : constant Identifier_Token_Index :=
           (if Separator = Invalid_Node_Index
            then Invalid_Identifier_Token
            else To_Identifier_Token (Tree.Find_Descendant (Separator, +rhs_item_ID), Tree, Data.Terminals));

         Name_Node : constant Node_Index :=
           (if Existing_Decl = Invalid_Node_Index
            then Invalid_Node_Index
            else List_Matches (Existing_Decl, Separator_Content, Element_Content));
      begin
         return List_Nonterm_Name : Identifier_Token_Index do
            if Name_Node = Invalid_Node_Index then
               List_Nonterm_Name := Find_List_Nonterminal_1 (Separator_Content, Element_Content);

               if List_Nonterm_Name = Invalid_Identifier_Token then
                  List_Nonterm_Name := To_Identifier_Token
                    ((if Existing_Decl = Invalid_Node_Index
                      then New_Identifier (List_Nonterm_String)
                      else Next_Nonterm_Name (List_Nonterm_String)),
                     Byte_Region  => Tree.Byte_Region (Element));

                  case To_Token_Enum (Tree.ID (Element)) is
                  when rhs_item_list_ID =>
                     pragma Assert (Separator_Ident_Tok = Invalid_Identifier_Token);

                     New_Nonterminal_List (List_Nonterm_Name, Element, Separator_Ident_Tok);

                  when rhs_element_ID =>
                     New_Nonterminal_List
                       (List_Nonterm => List_Nonterm_Name,
                        List_Element => To_Identifier_Token
                          (Tree.Find_Descendant (Element, +rhs_item_ID), Tree, Data.Terminals),
                        Separator    => Separator_Ident_Tok);

                  when rhs_item_ID | IDENTIFIER_ID =>
                     New_Nonterminal_List
                       (List_Nonterm => List_Nonterm_Name,
                        List_Element => To_Identifier_Token (Element, Tree, Data.Terminals),
                        Separator    => Separator_Ident_Tok);

                  when others =>
                     raise SAL.Programmer_Error with "unexpected case in Maybe_New_List_Nonterminal: " &
                       Tree.Image (Element, Wisitoken_Grammar_Actions.Descriptor);
                  end case;
               end if;

            else
               List_Nonterm_Name := To_Identifier_Token (Name_Node, Tree, Data.Terminals);

               if Erase_EBNF_In_Element then
                  Erase_Deleted_EBNF_Nodes (Element);
               end if;

               if Trace_Generate_EBNF > Extra then
                  Ada.Text_IO.Put_Line ("use " & Get_Text (Data, Tree, Name_Node));
               end if;
            end if;

         end return;
      end Maybe_New_Nonterminal_List;

      procedure Copy_Non_Grammar
        (From : in Valid_Node_Index;
         To   : in Valid_Node_Index)
      is
         From_Aug : constant Base_Token_Class_Access := Tree.Augmented (From);
      begin
         if From_Aug /= null then
            declare
               New_Aug : constant Augmented_Token_Access := new Augmented_Token'
                 (ID          => Tree.ID (From),
                  Tree_Index  => To,
                  Non_Grammar => Augmented_Token_Access (From_Aug).Non_Grammar,
                  others => <>);
            begin
               Tree.Set_Augmented (To, Base_Token_Class_Access (New_Aug));
            end;
         end if;
      end Copy_Non_Grammar;

      procedure Translate_RHS_Group_Item (Node : in Valid_Node_Index)
      is
         --  Current tree:
         --
         --  rhs_element: Parent (Node, 2)
         --  | rhs_item: Parent (Node, 1)
         --  | | rhs_group_item: Node
         --  | | | LEFT_PAREN
         --  | | | rhs_alternative_list: Child (Node, 2)
         --  | | | RIGHT_PAREN

         use Syntax_Trees.LR_Utils;

         Element_Content  : constant String           := Get_Text (Data, Tree, Tree.Child (Node, 2));
         Right_Paren_Node : constant Valid_Node_Index := Tree.Child (Node, 3);
         List             : constant Constant_List    := Creators.Create_List
           (Tree, Tree.Child (Tree.Root, 1), +compilation_unit_list_ID, +compilation_unit_ID);
         Name_Node        : Node_Index;
         New_Ident        : Base_Identifier_Index     := Invalid_Identifier_Index;
      begin
         --  See if there's an existing nonterminal for this content.
         for N of List loop

            if Tree.Production_ID (Tree.Child (N, 1)) = (+nonterminal_ID, 0) then
               --  Target nonterm is:
               --
               --  (compilation_unit_1, (111 . 128))
               --  | (nonterminal_0, (111 . 128))
               --  | |  7;(IDENTIFIER, (111 . 128))
               --  | | (COLON)
               --  | | (rhs_list_1, (111 . 128))
               --  | | | ...
               declare
                  RHS_List_1 : constant Node_Index := Tree.Child (Tree.Child (N, 1), 3);
               begin
                  if RHS_List_1 /= Invalid_Node_Index and then
                    Element_Content = Get_Text (Data, Tree, RHS_List_1)
                  then
                     Name_Node := Tree.Child (Tree.Child (N, 1), 1);
                     case Tree.Label (Name_Node) is
                     when Shared_Terminal =>
                        New_Ident := New_Identifier (Get_Text (Data, Tree, Name_Node));
                     when Virtual_Identifier =>
                        New_Ident := Tree.Identifier (Name_Node);
                     when others =>
                        Raise_Programmer_Error ("process_node rhs_group_item", Data, Tree, Name_Node);
                     end case;

                     exit;
                  end if;
               end;
            end if;
         end loop;

         if New_Ident = Invalid_Identifier_Index then
            New_Ident := Next_Nonterm_Name;
            New_Nonterminal ("group item", New_Ident, Tree.Child (Node, 2));
         else
            Erase_Deleted_EBNF_Nodes (Tree.Child (Node, 2));
         end if;

         Tree.Set_Node_Identifier (Node, +IDENTIFIER_ID, New_Ident);
         Copy_Non_Grammar (Right_Paren_Node, Node);
         Tree.Set_Children (Tree.Parent (Node), (+rhs_item_ID, 0), (1 => Node));
         Clear_EBNF_Node (Node);
      end Translate_RHS_Group_Item;

      procedure Translate_RHS_Multiple_Item (B : in Valid_Node_Index)
      is
         --  We have one of:
         --
         --  | a { b }  c
         --  | a { b } - c
         --  | a ( b ) + c
         --  | a ( b ) * c
         --  | a b+ c
         --  | a b* c
         --
         --  where a and/or c can be empty. Replace it with a new canonical
         --  list nonterminal:
         --
         --  nonterminal_nnn_list
         --  : b
         --  | nonterminal_nnn_list b
         --
         --  and a second RHS if it can be empty:
         --  | a c

         --  Current tree:
         --
         --  rhs_element : Parent (B, 2)
         --  | rhs_item: Parent (B, 1)
         --  | | rhs_multiple_item: B
         --  | | | LEFT_BRACE | LEFT_PAREN
         --  | | | rhs_alternative_list
         --  | | | ...
         --  | | | RIGHT_BRACE | RIGHT_PAREN
         --  | | | [MINUS | PLUS | STAR]

         --  or:
         --
         --  rhs_element : Parent (B, 2)
         --  | rhs_item: Parent (B, 1)
         --  | | rhs_multiple_item: B
         --  | | | IDENTIFIER
         --  | | | PLUS | STAR

         use Syntax_Trees.LR_Utils;
         use Syntax_Trees.LR_Utils.Creators;
         use all type Ada.Containers.Count_Type;

         Canonical_List    : Boolean                   := False;
         Done              : Boolean                   := False;
         Parent_RHS_Item   : constant Valid_Node_Index := Tree.Parent (B);
         List_Nonterm_Name : Identifier_Token_Index    := Invalid_Identifier_Token;

         B_Alt_List_List : constant Constant_List :=
           (case Tree.RHS_Index (B) is
            when 0 .. 3 =>
               Create_List (Tree, Tree.Child (B, 2), +rhs_alternative_list_ID, +rhs_item_list_ID),
            when others => Invalid_List (Tree));
         --  The rhs_alternative_list of the rhs_multiple_item.

         B_Alt_List_Item_List : List :=
           (if B_Alt_List_List.Is_Invalid
            then Invalid_List (Tree)
            else Create_List
              (Tree, Get_Node (B_Alt_List_List.First), +rhs_item_list_ID, +rhs_element_ID,
               Separator_ID => Invalid_Token_ID));
         --  The first rhs_item_list of the rhs_multiple_item.

         Container_List_Root : Node_Index := Invalid_Node_Index;
         --  Updated by Insert_Optional_RHS.

         procedure Check_Canonical_List
         is
            --  In EBNF, a canonical list with a separator looks like:
            --
            --  A enumConstant (',' enumConstant)* C
            --
            --  or, with no separator:
            --
            --  SwitchLabels : SwitchLabel {SwitchLabel}
            --
            --  where B is the rhs_multiple_item containing "(','
            --  enumConstant)*" or "{SwitchLabel}".
            --
            --  The tokens may have labels.
            --
            --  Handling these cases specially eliminates a conflict between
            --  reducing to enumConstants and reducing to the introduced nonterm
            --  list.
            --
            --  Alternately, the no separator case can be:
            --
            --  enumConstants : enumConstant+ ;
            --
            --  Handling this no separator case specially does not eliminate any
            --  conflicts, but it does reduce the number of added nonterminals,
            --  and keeps the names simpler.

            List_Nonterm_Decl : constant Valid_Node_Index := Tree.Find_Ancestor (B, +nonterminal_ID);
            RHS_List_Root     : constant Valid_Node_Index := Tree.Child (List_Nonterm_Decl, 3);

            RHS_List : List := Create_List
              (Tree, RHS_List_Root, +rhs_list_ID, +rhs_ID, Separator_ID => +BAR_ID);

            RHS : constant Valid_Node_Index := Tree.Find_Ancestor
              (B, (+rhs_ID, +rhs_alternative_list_ID));
            --  If rhs_ID, the RHS containing the canonical list candidate.
            --  If rhs_alternative_list_ID, an unnamed canonical list candidate

            RHS_Item_List_Root : constant Valid_Node_Index := List_Root
              (Tree, Tree.Find_Ancestor (B, +rhs_item_list_ID), +rhs_item_list_ID);

            RHS_Item_List_List : List := Create_List
              (Tree, RHS_Item_List_Root, +rhs_item_list_ID, +rhs_element_ID, Separator_ID => Invalid_Token_ID);

            RHS_Item_List_Iter : constant Constant_Iterator := RHS_Item_List_List.Iterate_Constant;

            Element_2 : constant Cursor := RHS_Item_List_List.To_Cursor (Tree.Parent (B, 2));
            --  The rhs_element containing the rhs_multiple_item

            Element_1 : constant Node_Index := Get_Node (RHS_Item_List_Iter.Previous (Element_2));
            --  The rhs_element containing the list element

            Can_Be_Empty : constant Boolean := Element_1 = Invalid_Node_Index and Tree.RHS_Index (B) in 0 | 3;

            procedure Do_Simple_Named (List_Elements : in Valid_Node_Index)
            with Pre => To_Token_Enum (Tree.ID (List_Elements)) in rhs_element_ID | rhs_item_list_ID
            is
               pragma Assert (Tree.ID (RHS) = +rhs_ID);

               --  The existing nonterminal declaration is one of:
               --
               --  1a) list_name
               --       : list_element {separator list_element}
               --         %( action? )%
               --       ;
               --
               --  2a) nonterm_name
               --      : {list_element}
               --        %( action? )%
               --      ;
               --
               --  Rewrite 1a) to:
               --
               --  1b) list_name
               --        : list_element
               --          %( action? )%
               --        | list_name separator list_element
               --          %( action? )%
               --        ;
               --
               --  If 2a) can be empty, rewrite to:
               --
               --  2b) list_element_list
               --        : list_element
               --          %( action? )%
               --        | list_element_list list_element
               --          %( action? )%
               --        ;
               --
               --      nonterm_name
               --        : list_name
               --          %( action? )%
               --        | empty
               --        ;
               --
               --  If instead we do the shortcut:
               --      list_element_list
               --        : list_element
               --        | list_element_list list_element
               --        | empty
               --        ;
               --  and list_element starts with a nullable nonterm, then there is a
               --  conflict between reducing 0 tokens to an empty list_element_list
               --  or to the nullable nonterm; see ada_lite_ebnf.wy declarative_part.
               --  We have not computed Nullable yet, so we assume it is true.
               --
               --  otherwise rewrite to:
               --
               --  2c) nonterm_name
               --        : list_element
               --          %( action? )%
               --        | nonterm_name list_element
               --          %( action? )%
               --        ;

               RHS_Item_List_1 : constant Constant_List :=
                 (case To_Token_Enum (Tree.ID (List_Elements)) is
                  when rhs_element_ID   => To_RHS_Item_List (Tree, List_Elements),
                  when rhs_item_list_ID => Creators.Create_List
                    (Tree, List_Elements, +rhs_item_list_ID, +rhs_element_ID),
                  when others           => raise SAL.Programmer_Error);

               New_RHS_List : List := Empty_RHS_List (Tree);

               Post_Parse_Action : constant Node_Index := Tree.Child (RHS, 2); --  deleted by first Add_RHS
               In_Parse_Action   : constant Node_Index := Tree.Child (RHS, 3);

               List_Name : constant Identifier_Token_Index :=
                 (if Can_Be_Empty
                  then To_Identifier_Token
                    (New_Identifier
                       (Get_Text (Data, Tree, Tree.Find_Descendant (List_Elements, +rhs_item_ID)) & "_list"))
                  else To_Identifier_Token (Tree.Child (List_Nonterm_Decl, 1), Tree, Data.Terminals));
            begin
               New_RHS_List.Append
                 (Add_RHS
                    (Tree, RHS_Item_List_1.Root,
                     Post_Parse_Action => Post_Parse_Action,
                     In_Parse_Action   => In_Parse_Action));

               B_Alt_List_Item_List.Prepend
                 (Add_RHS_Element
                    (Tree, Add_RHS_Item
                       (Tree,
                        Add_Identifier_Token
                          (Tree,
                           List_Name,
                           Data.Terminals))));

               New_RHS_List.Append
                 (Add_RHS
                    (Tree,
                     B_Alt_List_Item_List.Root,
                     Post_Parse_Action => Tree.Copy_Subtree (Post_Parse_Action),
                     In_Parse_Action   => Tree.Copy_Subtree (In_Parse_Action)));

               if Can_Be_Empty then
                  Add_Compilation_Unit
                    ("canonical list",
                     Tree_Add_Nonterminal
                       (Child_1 => Add_Identifier_Token (Tree, List_Name, Data.Terminals),
                        Child_2 => Tree.Add_Terminal (+COLON_ID),
                        Child_3 => New_RHS_List.Root,
                        Child_4 => Tree.Add_Nonterm
                          ((+semicolon_opt_ID, 0),
                           (1   => Tree.Add_Terminal (+SEMICOLON_ID)))));

                  Tree.Replace_Child
                    (Parent               => Tree.Find_Descendant (Get_Node (RHS_List.First), +rhs_item_list_ID),
                     Child_Index          => 1,
                     New_Child            => Add_RHS_Element
                       (Tree,
                        Add_RHS_Item
                          (Tree, Add_Identifier_Token (Tree, List_Name, Data.Terminals))),
                     Old_Child            => Get_Node (Element_2),
                     Old_Child_New_Parent => Invalid_Node_Index);

                  RHS_List.Append (Empty_RHS (Tree));

               else
                  Tree.Replace_Child
                    (Parent               => List_Nonterm_Decl,
                     Child_Index          => 3,
                     Old_Child            => RHS_List_Root,
                     New_Child            => New_RHS_List.Root,
                     Old_Child_New_Parent => Invalid_Node_Index);
               end if;

               Clear_EBNF_Node (B);

               if Trace_Generate_EBNF > Extra then
                  Ada.Text_IO.New_Line;
                  Ada.Text_IO.Put_Line ("Simple_Named Canonical_List edited nonterm:");
                  Tree.Print_Tree (Wisitoken_Grammar_Actions.Descriptor, List_Nonterm_Decl);
               end if;
            end Do_Simple_Named;

            Simple_Named  : Boolean := False;
            Has_Separator : Boolean := False;
         begin
            if Trace_Generate_EBNF > Detail then
               Ada.Text_IO.Put_Line ("Check_Canonical_List start: RHS " & Get_Text (Data, Tree, RHS_List.Root));
               Ada.Text_IO.Put_Line ("Check_Canonical_List start: B " & Get_Text (Data, Tree, B));
            end if;

            if Is_Invalid (B_Alt_List_Item_List) then
               return;
            end if;

            if Element_1 = Invalid_Node_Index then
               Has_Separator := False;
            else
               if (Tree.RHS_Index (B) in 4 .. 5 or else B_Alt_List_Item_List.Count in 1 .. 2) and then
                  Get_Item_Text (Data, Tree, Element_1) =
                  Get_Item_Text (Data, Tree, Get_Node (B_Alt_List_Item_List.Last))
               then
                  Has_Separator := B_Alt_List_Item_List.Count = 2;
               else
                  return;
               end if;
            end if;

            if Duplicate
              (RHS_List,
               Tree.Find_Descendant
                 ((if Element_1 = Invalid_Node_Index
                   then Get_Node (B_Alt_List_Item_List.Last)
                   else Element_1),
                  +rhs_item_ID))
            then
               --  See ada_lite_ebnf.wy expression; recognizing this would cause
               --  conflicts between reducing a relation to expression or one of the
               --  lists.
               return;
            end if;

            Canonical_List := True;

            if ((RHS_Item_List_List.Count = 1 and B_Alt_List_Item_List.Count = 1) or
                  (RHS_Item_List_List.Count = 2 and Element_2 = RHS_Item_List_List.Last)) and
              Tree.ID (RHS) = +rhs_ID and
              RHS_List.Count = 1
            then
               Simple_Named := True;
            end if;

            if Trace_Generate_EBNF > Detail then
               Ada.Text_IO.Put_Line
                 ((if Simple_Named then "simple named " else "embedded ") & "canonical list" &
                    (if Has_Separator then " with separator" else ""));
            end if;

            if Simple_Named then
               declare
                  List_Elements : constant Node_Index :=
                    (if Element_1 = Invalid_Node_Index
                     then Tree.Copy_Subtree (B_Alt_List_Item_List.Root)
                     else Element_1);
               begin
                  Do_Simple_Named (List_Elements);

                  if Element_1 = Invalid_Node_Index then
                     Record_Copied_EBNF_Nodes (List_Elements);
                  end if;
                  Done := True;
                  return;
               end;
            elsif Can_Be_Empty then
               --  use cases for this Insert_Optional_RHS:
               --  yes: java_types_ch19.wy Dims
               --  no: ada_lite_ebnf.wy enumeration_type_definition simple_expression
               Container_List_Root := Insert_Optional_RHS (B);
            end if;

            declare
               Separator : constant Node_Index :=
                 (if Has_Separator
                  then Get_Node (B_Alt_List_Item_List.First)
                  else Invalid_Node_Index);

               List_Nonterm_String : constant String :=
                 (if Has_Separator
                  then Get_Item_Text (Data, Tree, Element_1) & "_" & Get_Item_Text (Data, Tree, Separator)
                  elsif Element_1 /= Invalid_Node_Index
                  then Get_Item_Text (Data, Tree, Element_1) & "_" &
                     Get_Item_Text (Data, Tree, Get_Node (B_Alt_List_Item_List.First))
                  else Get_Item_Text (Data, Tree, Get_Node (B_Alt_List_Item_List.First)) &
                    (if B_Alt_List_Item_List.Count = 1
                     then ""
                     else "_" & Get_Item_Text
                       (Data, Tree, Get_Node (B_Alt_List_Item_List.Iterate.Next (B_Alt_List_Item_List.First))))) &
                 "_list";
            begin
               List_Nonterm_Name := Maybe_New_Nonterminal_List
                 (List_Nonterm_String   => List_Nonterm_String,
                  Element               =>
                    (if Element_1 = Invalid_Node_Index
                     then B_Alt_List_Item_List.Root
                     else Tree.Find_Descendant (Element_1, +rhs_item_ID)),
                  Erase_EBNF_In_Element => True,
                  Separator             => Separator);

               if Element_1 /= Invalid_Node_Index then
                  declare
                     Cur : Cursor := RHS_Item_List_List.To_Cursor (Element_1);
                  begin
                     --  Delete element_1; code below will replace element_2 with List_Nonterm_Name
                     RHS_Item_List_List.Delete (Cur);
                  end;
               end if;
            end;
         end Check_Canonical_List;

         procedure Find_List_Nonterminal_2 (Separator_Content : in String; Element_Content : in String)
         is
            --  Look for a pair of nonterms implementing a list of [Separator_Content] Element_Content.
            --
            --  list_element : element ;
            --
            --  list
            --    : list_element
            --    | list separator list_element
            --    ;
            --
            --  If found, set List_Nonterm_*_Name
            List : constant Constant_List := Creators.Create_List
              (Tree, Tree.Child (Tree.Root, 1), +compilation_unit_list_ID, +compilation_unit_ID);
         begin
            for Comp_Unit of List loop
               declare
                  Nonterm : constant Valid_Node_Index := Tree.Child (Comp_Unit, 1);
               begin
                  if Tree.ID (Nonterm) = +nonterminal_ID then
                     if Element_Content = Get_Text (Data, Tree, Tree.Child (Nonterm, 3)) then
                        --  Found list_element_nonterm : element ;
                        List_Nonterm_Name := Find_List_Nonterminal_1
                          (Separator_Content, Get_Text (Data, Tree, Tree.Child (Nonterm, 1)));
                        exit;
                     end if;
                  end if;
               end;
            end loop;
         end Find_List_Nonterminal_2;

      begin
         --  Check if this is a recognized pattern
         Check_Canonical_List;
         if Done then
            return;
         end if;

         --  Check to see if there is an already declared nonterminal
         --  list with the same content; if not, create one.
         case Tree.RHS_Index (B) is
         when 0 .. 3 =>
            --  0: { rhs_alternative_list }
            --  1: { rhs_alternative_list } -
            --  2: ( rhs_alternative_list ) +
            --  3: ( rhs_alternative_list ) *

            if Tree.RHS_Index (B) in 0 | 3 and not Canonical_List then
               Container_List_Root := Insert_Optional_RHS (B);
            end if;

            if Canonical_List then
               --  List_Nonterm_Name set by Check_Canonical_List
               null;

            elsif 1 = B_Alt_List_List.Count then
               --  IMPROVEME: this is redundant with check_canonical_list when Element_1 = invalid; simplify.
               List_Nonterm_Name := Maybe_New_Nonterminal_List
                 (List_Nonterm_String =>
                    Get_Item_Text (Data, Tree, Get_Node (B_Alt_List_Item_List.First)) &
                      (if B_Alt_List_Item_List.Count = 1
                       then ""
                       else "_" & Get_Item_Text
                         (Data, Tree, Get_Node (B_Alt_List_Item_List.Iterate.Next (B_Alt_List_Item_List.First)))) &
                      "_list",
                  Element               => B_Alt_List_Item_List.Root,
                  Erase_EBNF_In_Element => True,
                  Separator             => Invalid_Node_Index);

            else
               --  IMPROVEME: handle separator here? need test case
               --  IMPROVEME: ignore token labels
               --  IMPROVEME: extend maybe_new_nonterminal_list to handle this case.
               Find_List_Nonterminal_2
                 (Separator_Content => "",
                  Element_Content   => Get_Text (Data, Tree, Tree.Child (B, 2)));

               if List_Nonterm_Name = Invalid_Identifier_Token then
                  declare
                     List_Element_Name_String : constant String :=
                       Get_Item_Text (Data, Tree, Get_Node (B_Alt_List_Item_List.First)) &
                       (if B_Alt_List_Item_List.Count > 1
                        then "_" & Get_Item_Text
                          (Data, Tree, Get_Node (B_Alt_List_Item_List.Iterate.Next (B_Alt_List_Item_List.First)))
                        else "_" & Get_Item_Text
                          (Data, Tree, Get_Node (B_Alt_List_List.Iterate_Constant.Next (B_Alt_List_List.First))));

                     List_Nonterm_Name_String : constant String := List_Element_Name_String & "_list";

                     Existing_Decl : constant Node_Index := Find_Declaration (Data, Tree, List_Nonterm_Name_String);

                     List_Element_Name : constant Identifier_Index :=
                       (if Existing_Decl = Invalid_Node_Index
                        then New_Identifier (List_Element_Name_String)
                        else Next_Nonterm_Name (List_Element_Name_String));
                  begin
                     List_Nonterm_Name := To_Identifier_Token
                       ((if Existing_Decl = Invalid_Node_Index
                         then New_Identifier (List_Nonterm_Name_String)
                         else Next_Nonterm_Name ("list")));

                     New_Nonterminal ("canonical list element", List_Element_Name, Tree.Child (B, 2));
                     New_Nonterminal_List
                       (List_Nonterm_Name,
                        To_Identifier_Token (List_Element_Name, Tree.Byte_Region (B)),
                        Separator => Invalid_Identifier_Token);
                  end;
               else
                  Erase_Deleted_EBNF_Nodes (Tree.Child (B, 2));
               end if;
            end if;

         when 4 | 5 =>
            --  IDENTIFIER + | *

            List_Nonterm_Name := Maybe_New_Nonterminal_List
              (List_Nonterm_String   => Get_Text (Data, Tree, Tree.Child (B, 1)) & "_list",
               Element               => Tree.Child (B, 1),
               Erase_EBNF_In_Element => True, -- irrelevant
               Separator             => Invalid_Node_Index);

            if Tree.RHS_Index (B) = 5 then
               Container_List_Root := Insert_Optional_RHS (B);
            end if;

         when others =>
            Raise_Programmer_Error ("Translate_RHS_Multiple_Item unimplemented", Data, Tree, B);
         end case;

         --  Edit rhs_item to use list name
         declare
            Child : constant Valid_Node_Index := Add_Identifier_Token (Tree, List_Nonterm_Name, Data.Terminals);
         begin
            Tree.Set_Children (Parent_RHS_Item, (+rhs_item_ID, 0), (1 => Child));
         end;

         Clear_EBNF_Node (B);

         if Trace_Generate_EBNF > Detail then
            declare
               Item : constant Valid_Node_Index :=
                 (if Container_List_Root = Invalid_Node_Index
                  then Tree.Parent (Parent_RHS_Item)
                  else Container_List_Root);
            begin
               Ada.Text_IO.New_Line;
               Ada.Text_IO.Put_Line ("Translate_RHS_Multiple_Item edited: " & Get_Text (Data, Tree, Item));
               if Trace_Generate_EBNF > Extra then
                  Tree.Print_Tree (Wisitoken_Grammar_Actions.Descriptor, Item);
               end if;
            end;
         end if;
      end Translate_RHS_Multiple_Item;

      procedure Translate_RHS_Optional_Item (B : in Valid_Node_Index)
      is
         --  Source looks like:
         --
         --  | A [B] C
         --
         --  where A, B, C are token sequences. All are contained in one
         --  rhs_item_list, which may be contained in an rhs or an
         --  rhs_alternative_list. B contains an rhs_alternative_list.
         --
         --  First add a second rhs_item_list without B:
         --  | A C
         --
         --  then for each alternative in B, splice together rhs_item_lists A,
         --  B_i, C, copying A, C on all after the first:
         --  | A B_i C
         --
         --  See nested_ebnf_optional.wy for an example of nested optional
         --  items.
         --
         --  We don't create a separate nonterminal for B, so token labels stay
         --  in the same RHS for actions.
         --
         --  current tree:
         --
         --  rhs_list:
         --  | rhs | rhs_alternative_list:
         --  | | rhs_item_list
         --  | | | rhs_item_list
         --  | | ...
         --  | | | | | rhs_element: a.last
         --  | | | | | | rhs_item:
         --  | | | | rhs_element:
         --  | | | | | rhs_item: contains b
         --  | | | | | | rhs_optional_item: B
         --  | | | | | | | LEFT_BRACKET: B.Children (1)
         --  | | | | | | | rhs_alternative_list: B.Children (2) b
         --  | | | | | | | RIGHT_BRACKET: B.Children (3)
         --  | | | rhs_element: c.first
         --  | | | | rhs_item:

         use Syntax_Trees.LR_Utils;
         use Syntax_Trees.LR_Utils.Creators;

         Container_List_Root : constant Valid_Node_Index := Insert_Optional_RHS (B);
      begin
         case Tree.RHS_Index (B) is
         when 0 | 1 =>
            --  : LEFT_BRACKET rhs_alternative_list RIGHT_BRACKET
            --  | LEFT_PAREN rhs_alternative_list RIGHT_PAREN QUESTION

            declare
               Container_List : Syntax_Trees.LR_Utils.List :=
                 (if Tree.ID (Container_List_Root) = +rhs_list_ID
                  then Create_List
                    (Tree,
                     Root         => Container_List_Root,
                     List_ID      => +rhs_list_ID,
                     Element_ID   => +rhs_ID,
                     Separator_ID => +BAR_ID)
                  else Create_List
                    (Tree,
                     Root         => Container_List_Root,
                     List_ID      => +rhs_alternative_list_ID,
                     Element_ID   => +rhs_item_list_ID,
                     Separator_ID => +BAR_ID));

               Container_Cur : Cursor := Container_List.Find
                 (if Container_List.Element_ID = +rhs_ID
                  then Tree.Find_Ancestor (B, +rhs_ID)
                  else List_Root (Tree, Tree.Find_Ancestor (B, +rhs_item_list_ID), +rhs_item_list_ID));

               ABC_List : List := Create_From_Element
                 (Tree, Tree.Parent (B, 2),
                  List_ID      => +rhs_item_list_ID,
                  Element_ID   => +rhs_element_ID,
                  Separator_ID => Invalid_Token_ID);

               ABC_Iter : constant Iterator := ABC_List.Iterate;

               ABC_B_Cur   : constant Cursor := ABC_List.To_Cursor (Tree.Parent (B, 2));
               ABC_A_Last  : constant Cursor := ABC_Iter.Previous (ABC_B_Cur);
               ABC_C_First : constant Cursor := ABC_Iter.Next (ABC_B_Cur);

               B_Alternative_List : constant Constant_List := Create_List
                 (Tree, Tree.Child (B, 2), +rhs_alternative_list_ID, +rhs_item_list_ID);

            begin
               --  An alternate design would be to splice together the existing A,
               --  B_i, C; but it's too hard to get all the parent updates right.
               for Alt of reverse B_Alternative_List loop

                  declare
                     B_Item_List : constant Constant_List := Create_List
                       (Tree, Alt, +rhs_item_list_ID, +rhs_element_ID);

                     New_ABC : List := Empty_List (ABC_List);
                  begin
                     if Has_Element (ABC_A_Last) then
                        Copy (Source_List => ABC_List,
                              Source_Last => ABC_A_Last,
                              Dest_List   => New_ABC);
                     end if;

                     Copy (B_Item_List, Dest_List => New_ABC);

                     if Has_Element (ABC_C_First) then
                        Copy (ABC_List, Source_First => ABC_C_First, Dest_List => New_ABC);
                     end if;

                     if Container_List.Element_ID = +rhs_ID then
                        Insert_RHS (Container_List, New_ABC.Root, After => Get_Node (Container_Cur));
                     else
                        Container_List.Insert (New_ABC.Root, After => Container_Cur);
                     end if;

                     Record_Copied_EBNF_Nodes (New_ABC.Root);
                  end;
               end loop;

               Erase_Deleted_EBNF_Nodes (Get_Node (Container_Cur));
               --  This includes B, so we don't do 'Clear_EBNF_Node (B)'.

               Container_List.Delete (Container_Cur);
            end;

         when 2 =>
            --  | IDENTIFIER QUESTION
            --
            --  Current tree:
            --   rhs_item_3
            --   | rhs_optional_item_2: B
            --   | | IDENTIFIER
            --   | | QUESTION
            --
            --  Change to:
            --   rhs_item_0
            --   | IDENTIFIER

            Tree.Set_Children (Tree.Parent (B), (+rhs_item_ID, 0), (1 => Tree.Child (B, 1)));
            Clear_EBNF_Node (B);

         when 3 =>
            --  | STRING_LITERAL_2 QUESTION
            Tree.Set_Children (Tree.Parent (B), (+rhs_item_ID, 1), (1 => Tree.Child (B, 1)));
            Clear_EBNF_Node (B);

         when others =>
            Raise_Programmer_Error ("translate_ebnf_to_bnf rhs_optional_item unimplemented", Data, Tree, B);
         end case;

         if WisiToken.Trace_Generate_EBNF > Detail then
            Ada.Text_IO.New_Line;
            Ada.Text_IO.Put_Line ("Translate_RHS_Optional_Item edited:");
            Tree.Print_Tree (Wisitoken_Grammar_Actions.Descriptor, Container_List_Root);
         end if;
      end Translate_RHS_Optional_Item;

      procedure Translate_Token_Literal (Node : in Valid_Node_Index)
      is
         use Syntax_Trees.LR_Utils;

         Name_Ident : Identifier_Index;

         function Equal
           (Target : in String;
            List   : in Constant_List'Class;
            N      : in Valid_Node_Index)
           return Boolean
         is
            pragma Unreferenced (List);
         begin
            if Tree.Production_ID (Tree.Child (N, 1)) = (+declaration_ID, 0) then
               declare
                  Decl       : constant Node_Index       := Tree.Child (N, 1);
                  Value_Node : constant Valid_Node_Index := Tree.Child (Tree.Child (Decl, 4), 1);
               begin
                  if Tree.ID (Value_Node) = +declaration_item_ID and then
                    Tree.ID (Tree.Child (Value_Node, 1)) in
                    +IDENTIFIER_ID | +STRING_LITERAL_1_ID | +STRING_LITERAL_2_ID and then
                    Target = Get_Text (Data, Tree, Tree.Child (Value_Node, 1), Strip_Quotes => True)
                  then
                     case Tree.Label (Tree.Child (Decl, 3)) is
                     when Shared_Terminal =>
                        Name_Ident := New_Identifier (Get_Text (Data, Tree, Tree.Child (Decl, 3)));
                     when Virtual_Identifier =>
                        Name_Ident := Tree.Identifier (Tree.Child (Decl, 3));
                     when others =>
                        raise SAL.Programmer_Error;
                     end case;
                     return True;
                  else
                     return False;
                  end if;
               end;
            else
               return False;
            end if;
         end Equal;

         Value : constant String     := Get_Text (Data, Tree, Node, Strip_Quotes => True);
         Found : constant Node_Index := Find_Nonterminal (Value, Equal'Unrestricted_Access);
      begin
         if Found = Invalid_Node_Index then
            if GNAT.Regexp.Match (Value, Symbol_Regexp) then
               Name_Ident := New_Identifier (Ada.Characters.Handling.To_Upper (Value));
            else
               WisiToken.Generate.Put_Error
                 (WisiToken.Generate.Error_Message
                    (Data.Grammar_Lexer.File_Name, Get_Line (Data, Tree, Node),
                     "punctuation token '" & Value & "' not declared"));
               return;
            end if;
         end if;

         --  Replace string literal in rhs_item
         declare
            Parent : constant Valid_Node_Index := Tree.Parent (Node);
         begin
            case To_Token_Enum (Tree.ID (Parent)) is
            when rhs_item_ID =>
               Tree.Set_Children
                 (Tree.Parent (Node),
                  (+rhs_item_ID, 0),
                  (1 => Tree.Add_Identifier (+IDENTIFIER_ID, Name_Ident, Tree.Byte_Region (Node))));

            when rhs_optional_item_ID =>
               Tree.Set_Children
                 (Tree.Parent (Node),
                  (+rhs_optional_item_ID, 2),
                  (Tree.Add_Identifier (+IDENTIFIER_ID, Name_Ident, Tree.Byte_Region (Node)),
                   Tree.Child (Tree.Parent (Node), 2)));

            when others =>
               Raise_Programmer_Error ("translate_ebnf_to_bnf string_literal_2 unimplemented", Data, Tree, Node);
            end case;
         end;

         Clear_EBNF_Node (Node);
         if Found /= Invalid_Node_Index then
            return;
         end if;

         --  Declare token for keyword string literal
         declare
            Keyword        : constant Valid_Node_Index := Tree.Add_Identifier
              (+KEYWORD_ID, Keyword_Ident, Tree.Byte_Region (Node));
            Kind           : constant Valid_Node_Index := Tree.Add_Nonterm
              ((+token_keyword_non_grammar_ID, 0),
               (1 => Keyword));
            Value_Literal  : constant Valid_Node_Index := Tree.Add_Identifier
              (+STRING_LITERAL_1_ID, New_Identifier ('"' & Value & '"'), Tree.Byte_Region (Node));
            Decl_Item      : constant Valid_Node_Index := Tree.Add_Nonterm
              ((+declaration_item_ID, 1),
               (1 => Value_Literal));
            Decl_Item_List : constant Valid_Node_Index := Tree.Add_Nonterm
              ((+declaration_item_list_ID, 0),
               (1 => Decl_Item));

            Percent : constant Valid_Node_Index := Tree.Add_Identifier
              (+PERCENT_ID, Percent_Ident, Tree.Byte_Region (Node));
            Name    : constant Valid_Node_Index := Tree.Add_Identifier
              (+IDENTIFIER_ID, Name_Ident, Tree.Byte_Region (Node));
            Decl    : constant Valid_Node_Index := Tree.Add_Nonterm
              ((+declaration_ID, 0), (Percent, Kind, Name, Decl_Item_List), Action => declaration_0'Access);
         begin
            Add_Compilation_Unit ("literal token", Decl, Prepend => True);
         end;

      end Translate_Token_Literal;

      procedure Process_Node (Node : in Valid_Node_Index)
      is begin
         case To_Token_Enum (Tree.ID (Node)) is
         --  Token_Enum_ID alphabetical order
         when declaration_ID =>
            --  Must be "%meta_syntax EBNF"; change to BNF
            declare
               Decl_Item    : constant Valid_Node_Index       := Tree.Find_Descendant
                 (Tree.Child (Node, 3), +declaration_item_ID);
               Old_Children : constant Valid_Node_Index_Array := Tree.Children (Decl_Item);
               New_Children : constant Valid_Node_Index_Array :=
                 (1 => Tree.Add_Identifier
                    (+IDENTIFIER_ID, New_Identifier ("BNF"), Tree.Byte_Region (Decl_Item)));
            begin
               Copy_Non_Grammar (Old_Children (1), New_Children (1));
               Tree.Set_Children (Decl_Item, (+declaration_item_ID, 1), New_Children);
            end;
            Clear_EBNF_Node (Node);

         when rhs_alternative_list_ID =>
            --  All handled by New_Nonterminal*
            raise SAL.Programmer_Error;

         when rhs_attribute_ID =>
            --  Just delete it
            declare
               use WisiToken.Syntax_Trees.LR_Utils;
               RHS_Item_List : List := Creators.Create_From_Element
                 (Tree, Tree.Parent (Node, 2), +rhs_item_list_ID, +rhs_element_ID, Invalid_Token_ID);
               Element : Cursor := RHS_Item_List.To_Cursor (Tree.Parent (Node, 2));
            begin
               RHS_Item_List.Delete (Element);
            end;
            Clear_EBNF_Node (Node);

         when rhs_group_item_ID =>
            Translate_RHS_Group_Item (Node);

         when rhs_multiple_item_ID =>
            Translate_RHS_Multiple_Item (Node);

         when rhs_optional_item_ID =>
            Translate_RHS_Optional_Item (Node);

         when STRING_LITERAL_2_ID =>
            Translate_Token_Literal (Node);

         when others =>
            Raise_Programmer_Error ("unimplemented EBNF node", Data, Tree, Node);
         end case;
      exception
      when SAL.Programmer_Error =>
         raise;
      when E : others =>
         Raise_Programmer_Error
           ("unhandled exception " & Ada.Exceptions.Exception_Name (E) & ": " &
              Ada.Exceptions.Exception_Message (E),
            Data, Tree, Node);
      end Process_Node;

      EBNF_Allowed : Boolean := True;

      procedure Validate_Node
        (Tree              : in     Syntax_Trees.Tree;
         Node              : in     Valid_Node_Index;
         Node_Image_Output : in out Boolean)
      is
         use Ada.Text_IO;

         procedure Put_Error (Msg : in String)
         is begin
            if not Node_Image_Output then
               Node_Image_Output := True;
               Put_Line
                 (Current_Error,
                  Error_Message
                    (Tree, Data.Terminals, Node, Data.Grammar_Lexer.File_Name,
                     Tree.Image
                       (Node, Wisitoken_Grammar_Actions.Descriptor,
                        Include_RHS_Index => True,
                        Include_Children  => Trace_Generate_EBNF > Detail,
                        Node_Numbers      => True)));
            end if;
            Put_Line (Current_Error, "... " & Msg);
            WisiToken.Generate.Error := True;
         end Put_Error;

         procedure Check_EBNF_Allowed
         is begin
            if not EBNF_Allowed then
               Put_Error ("no EBNF allowed");
            end if;
         end Check_EBNF_Allowed;

      begin
         if Tree.Label (Node) /= Nonterm then
            return;
         end if;

         declare
            use all type Ada.Containers.Count_Type;
            Children  : constant Valid_Node_Index_Array := Tree.Children (Node);
            RHS_Index : constant Natural                := Tree.RHS_Index (Node);
         begin
            if (for some Child of Children => Child = Deleted_Child) then
               Put_Error ("deleted child");
               return;
            end if;

            case To_Token_Enum (Tree.ID (Node)) is
            when nonterminal_ID =>
               null;

            when rhs_list_ID =>
               case RHS_Index is
               when 0 =>
                  if Children'Length /= 1 then
                     Put_Error ("expected child_count 1");
                  elsif Tree.ID (Children (1)) /= +rhs_ID then
                     Put_Error ("child 1 not rhs");
                  end if;

               when 1 =>
                  if Tree.Child_Count (Node) /= 3 then
                     Put_Error ("expected child_count 3");
                  elsif Tree.ID (Children (1)) /= +rhs_list_ID or
                    Tree.ID (Children (2)) /= +BAR_ID or
                    Tree.ID (Children (3)) /= +rhs_ID
                  then
                     Put_Error ("expecting rhs_list BAR rhs");
                  end if;

               when others =>
                  Put_Error ("unexpected RHS_Index");
               end case;

            when rhs_ID =>
               case RHS_Index is
               when 0 =>
                  if Children'Length /= 0 then
                     Put_Error ("expected child_count 0");
                  end if;

               when 1 =>
                  if Tree.Child_Count (Node) /= 1 then
                     Put_Error ("expected child_count 1");
                  elsif Tree.ID (Children (1)) /= +rhs_item_list_ID then
                     Put_Error ("expecting rhs_item_list");
                  end if;

               when 2 =>
                  if Tree.Child_Count (Node) /= 2 then
                     Put_Error ("expected child_count 2");
                  elsif Tree.ID (Children (1)) /= +rhs_item_list_ID or
                    Tree.ID (Children (2)) /= +ACTION_ID
                  then
                     Put_Error ("expecting rhs_item_list ACTION");
                  end if;

               when 3 =>
                  if Tree.Child_Count (Node) /= 3 then
                     Put_Error ("expected child_count 3");
                  elsif Tree.ID (Children (1)) /= +rhs_item_list_ID or
                    Tree.ID (Children (2)) /= +ACTION_ID or
                    Tree.ID (Children (3)) /= +ACTION_ID
                  then
                     Put_Error ("expecting rhs_item_list ACTION ACTION");
                  end if;

               when others =>
                  Put_Error ("unexpected RHS_Index");
               end case;

            when rhs_attribute_ID =>
               Check_EBNF_Allowed;

            when rhs_element_ID =>
               case RHS_Index is
               when 0 =>
                  if Tree.Child_Count (Node) /= 1 then
                     Put_Error ("expected child_count 1");
                  elsif Tree.ID (Children (1)) /= +rhs_item_ID then
                     Put_Error ("expecting rhs_item");
                  end if;

               when 1 =>
                  if Tree.Child_Count (Node) /= 3 then
                     Put_Error ("expected child_count 3");
                  elsif Tree.ID (Children (1)) /= +IDENTIFIER_ID or
                    Tree.ID (Children (2)) /= +EQUAL_ID or
                    Tree.ID (Children (3)) /= +rhs_item_ID
                  then
                     Put_Error ("expecting IDENTIFIER EQUAL rhs_item");
                  end if;

               when others =>
                  Put_Error ("unexpected RHS_Index");
               end case;

            when rhs_item_list_ID =>
               case RHS_Index is
               when 0 =>
                  if Tree.Child_Count (Node) /= 1 then
                     Put_Error ("expected child_count 1");
                  elsif Tree.ID (Children (1)) /= +rhs_element_ID then
                     Put_Error ("expecting rhs_element");
                  end if;

               when 1 =>
                  if Tree.Child_Count (Node) /= 2 then
                     Put_Error ("expected child_count 2");
                  elsif Tree.ID (Children (1)) /= +rhs_item_list_ID or
                    Tree.ID (Children (2)) /= +rhs_element_ID
                  then
                     Put_Error ("expecting rhs_item_list ELEMENT");
                  end if;

               when others =>
                  Put_Error ("unexpected RHS_Index");
               end case;

            when rhs_item_ID =>
               if Tree.Child_Count (Node) /= 1 then
                  Put_Error ("expected child_count 1");
               end if;

               case RHS_Index is
               when 0 =>
                  if Tree.ID (Children (1)) /= +IDENTIFIER_ID then
                     Put_Error ("expecting IDENTIFIER");
                  end if;

               when 1 =>
                  if Tree.ID (Children (1)) /= +STRING_LITERAL_2_ID then
                     Put_Error ("expecting STRING_LITERAL_2");
                  end if;

               when 2 =>
                  if Tree.ID (Children (1)) /= +rhs_attribute_ID then
                     Put_Error ("expecting rhs_attribute");
                  end if;

               when 3 =>
                  if Tree.ID (Children (1)) /= +rhs_optional_item_ID then
                     Put_Error ("expecting rhs_optional_item");
                  end if;

               when 4 =>
                  if Tree.ID (Children (1)) /= +rhs_multiple_item_ID then
                     Put_Error ("expecting rhs_multiple_item");
                  end if;

               when 5 =>
                  if Tree.ID (Children (1)) /= +rhs_group_item_ID then
                     Put_Error ("expecting rhs_group_item");
                  end if;

               when others =>
                  Put_Error ("unexpected RHS_Index");
               end case;

            when rhs_group_item_ID =>
               Check_EBNF_Allowed;
               if RHS_Index /= 0 or
                 (Children'Length /= 3 or else
                    (Tree.ID (Children (1)) /= +LEFT_PAREN_ID or
                       Tree.ID (Children (2)) /= +rhs_alternative_list_ID or
                       Tree.ID (Children (3)) /= +RIGHT_PAREN_ID))
               then
                  Put_Error ("expecting RHS_Index 0, LEFT_PAREN rhs_alternative_list RIGHT_PAREN");
               end if;

            when rhs_optional_item_ID =>
               Check_EBNF_Allowed;
               case RHS_Index is
               when 0 =>
                  if Children'Length /= 3 or else
                    (Tree.ID (Children (1)) /= +LEFT_BRACKET_ID or
                       Tree.ID (Children (2)) /= +rhs_alternative_list_ID or
                       Tree.ID (Children (3)) /= +RIGHT_BRACKET_ID)
                  then
                     Put_Error ("expecting LEFT_BRACKET rhs_alternative_list RIGHT_BRACKET");
                  end if;

               when 1 =>
                  if Children'Length /= 4 or else
                    (Tree.ID (Children (1)) /= +LEFT_PAREN_ID or
                       Tree.ID (Children (2)) /= +rhs_alternative_list_ID or
                       Tree.ID (Children (3)) /= +RIGHT_PAREN_ID or
                       Tree.ID (Children (4)) /= +QUESTION_ID)
                  then
                     Put_Error ("expecting LEFT_PAREN rhs_alternative_list RIGHT_PAREN QUESTION");
                  end if;

               when 2 =>
                  if Children'Length /= 2 or else
                    (Tree.ID (Children (1)) /= +IDENTIFIER_ID or
                       Tree.ID (Children (2)) /= +QUESTION_ID)
                  then
                     Put_Error ("expecting IDENTIFIER QUESTION");
                  end if;

               when 3 =>
                  if Children'Length /= 2 or else
                    (Tree.ID (Children (1)) /= +STRING_LITERAL_2_ID or
                       Tree.ID (Children (2)) /= +QUESTION_ID)
                  then
                     Put_Error ("expecting STRING_LITERAL_2 QUESTION");
                  end if;

               when others =>
                  Put_Error ("unexpected RHS_Index");
               end case;

            when rhs_multiple_item_ID =>
               Check_EBNF_Allowed;
               case RHS_Index is
               when 0 =>
                  if Children'Length /= 3 or else
                    (Tree.ID (Children (1)) /= +LEFT_BRACE_ID or
                       Tree.ID (Children (2)) /= +rhs_alternative_list_ID or
                       Tree.ID (Children (3)) /= +RIGHT_BRACE_ID)
                  then
                     Put_Error ("expecting LEFT_BRACE rhs_alternative_list RIGHT_BRACE");
                  end if;

               when 1 =>
                  if Children'Length /= 4 or else
                    (Tree.ID (Children (1)) /= +LEFT_BRACE_ID or
                       Tree.ID (Children (2)) /= +rhs_alternative_list_ID or
                       Tree.ID (Children (3)) /= +RIGHT_BRACE_ID or
                       Tree.ID (Children (4)) /= +MINUS_ID)
                  then
                     Put_Error ("expecting LEFT_BRACE rhs_alternative_list RIGHT_BRACE MINUS");
                  end if;

               when 2 =>
                  if Children'Length /= 4 or else
                    (Tree.ID (Children (1)) /= +LEFT_PAREN_ID or
                       Tree.ID (Children (2)) /= +rhs_alternative_list_ID or
                       Tree.ID (Children (3)) /= +RIGHT_PAREN_ID or
                       Tree.ID (Children (4)) /= +PLUS_ID)
                  then
                     Put_Error ("expecting LEFT_PAREN rhs_alternative_list RIGHT_PAREN PLUS");
                  end if;

               when 3 =>
                  if Children'Length /= 4 or else
                    (Tree.ID (Children (1)) /= +LEFT_PAREN_ID or
                       Tree.ID (Children (2)) /= +rhs_alternative_list_ID or
                       Tree.ID (Children (3)) /= +RIGHT_PAREN_ID or
                       Tree.ID (Children (4)) /= +STAR_ID)
                  then
                     Put_Error ("expecting LEFT_PAREN rhs_alternative_list RIGHT_PAREN STAR");
                  end if;

               when 4 =>
                  if Children'Length /= 2 or else
                    (Tree.ID (Children (1)) /= +IDENTIFIER_ID or
                       Tree.ID (Children (2)) /= +PLUS_ID)
                  then
                     Put_Error ("expecting IDENTIFIER PLUS");
                  end if;

               when 5 =>
                  if Children'Length /= 2 or else
                    (Tree.ID (Children (1)) /= +IDENTIFIER_ID or
                       Tree.ID (Children (2)) /= +STAR_ID)
                  then
                     Put_Error ("expecting IDENTIFIER STAR");
                  end if;

               when others =>
                  Put_Error ("unexpected RHS_Index");
               end case;

            when rhs_alternative_list_ID =>
               Check_EBNF_Allowed;
               case RHS_Index is
               when 0 =>
                  if Children'Length /= 1 or else
                    (Tree.ID (Children (1)) /= +rhs_item_list_ID)
                  then
                     Put_Error ("expecting rhs_item_list");
                  end if;

               when 1 =>
                  if Children'Length /= 3 or else
                    (Tree.ID (Children (1)) /= +rhs_alternative_list_ID or
                       Tree.ID (Children (2)) /= +BAR_ID or
                       Tree.ID (Children (3)) /= +rhs_item_list_ID)
                  then
                     Put_Error ("expecting rhs_alternative_list BAR rhs_item_list");
                  end if;
               when others =>
                  Put_Error ("unexpected RHS_Index");
               end case;

            when compilation_unit_ID =>
               null;

            when compilation_unit_list_ID =>
               null;

            when others =>
               null;
            end case;
         end;
      end Validate_Node;

      procedure Check_Original_EBNF
      is
         use Ada.Text_IO;
         Sub_Tree_Root : Node_Index;
      begin
         for N in Data.EBNF_Nodes.First_Index .. Data.EBNF_Nodes.Last_Index loop
            if Data.EBNF_Nodes (N) then
               Sub_Tree_Root := Tree.Sub_Tree_Root (N);
               if Sub_Tree_Root /= Tree.Root then
                  Put_Line
                    (Current_Error,
                     Error_Message
                       (Tree, Data.Terminals, N, Data.Grammar_Lexer.File_Name,
                        Tree.Image
                          (N, Wisitoken_Grammar_Actions.Descriptor,
                           Node_Numbers      => True)));
                  Put_Line (Current_Error, "... not in tree; in root" & Sub_Tree_Root'Image);
                  WisiToken.Generate.Error := True;
               end if;
            end if;
         end loop;
      end Check_Original_EBNF;

      procedure Check_Copied_EBNF
      is
         use Ada.Text_IO;
         Sub_Tree_Root : Node_Index;
      begin
         for N of Copied_EBNF_Nodes loop
            if N /= Deleted_Child then
               Sub_Tree_Root := Tree.Sub_Tree_Root (N);
               if Sub_Tree_Root /= Tree.Root then
                  Put_Line
                    (Current_Error,
                     Error_Message
                       (Tree, Data.Terminals, N, Data.Grammar_Lexer.File_Name,
                        Tree.Image
                          (N, Wisitoken_Grammar_Actions.Descriptor,
                           Node_Numbers      => True)));
                  Put_Line (Current_Error, "... not in tree; in root" & Sub_Tree_Root'Image);
                  WisiToken.Generate.Error := True;
               end if;
            end if;
         end loop;
      end Check_Copied_EBNF;

   begin
      --  Process nodes in node increasing order, so contained items are
      --  translated first, so duplicates of the containing item can be found
      for I in Data.EBNF_Nodes.First_Index .. Data.EBNF_Nodes.Last_Index loop
         if Data.EBNF_Nodes (I) then
            if Trace_Generate_EBNF > Outline then
               Ada.Text_IO.New_Line;
               Ada.Text_IO.Put_Line
                 ("translate original node " & Tree.Image
                    (I, Wisitoken_Grammar_Actions.Descriptor,
                     Include_RHS_Index => True,
                     Node_Numbers      => True));
            end if;

            Process_Node (I);

            Tree.Validate_Tree
              (Data.Terminals, Wisitoken_Grammar_Actions.Descriptor, Data.Grammar_Lexer.File_Name, Tree.Root,
               Validate_Node'Unrestricted_Access);
            Check_Original_EBNF;
            Check_Copied_EBNF;
         end if;
      end loop;

      declare
         use Ada.Text_IO;
      begin
         for Node in Data.EBNF_Nodes.First_Index .. Data.EBNF_Nodes.Last_Index loop
            if Data.EBNF_Nodes (Node) then
               Put_Line
                 (Current_Error,
                  Error_Message
                    (Tree, Data.Terminals, Node, Data.Grammar_Lexer.File_Name,
                     Tree.Image
                       (Node, Wisitoken_Grammar_Actions.Descriptor,
                        Include_RHS_Index => True,
                        Include_Children  => Trace_Generate_EBNF > Detail,
                        Node_Numbers      => True)));
               Put_Line (Current_Error, "... original EBNF node not translated");
            end if;
         end loop;
      end;

      declare
         I : SAL.Base_Peek_Type := Copied_EBNF_Nodes.First_Index;
      begin
         --  Processing copied nodes may produce more copied nodes, so we can't
         --  use a 'for' loop.
         loop
            exit when I > Copied_EBNF_Nodes.Last_Index;
            if Copied_EBNF_Nodes (I) = Deleted_Child then
               --  Deleted
               if Trace_Generate_EBNF > Outline then
                  Ada.Text_IO.New_Line;
                  Ada.Text_IO.Put_Line
                    ("skipping deleted copied node " & Tree.Image
                       (Copied_EBNF_Nodes (I), Wisitoken_Grammar_Actions.Descriptor,
                        Include_RHS_Index => True,
                        Node_Numbers      => True));
               end if;
            else
               if Trace_Generate_EBNF > Outline then
                  Ada.Text_IO.New_Line;
                  Ada.Text_IO.Put_Line
                    ("translate copied node " & Tree.Image
                       (Copied_EBNF_Nodes (I), Wisitoken_Grammar_Actions.Descriptor,
                        Include_RHS_Index => True,
                        Node_Numbers      => True));
               end if;

               Process_Node (Copied_EBNF_Nodes (I));

               Tree.Validate_Tree
                 (Data.Terminals, Wisitoken_Grammar_Actions.Descriptor, Data.Grammar_Lexer.File_Name, Tree.Root,
                  Validate_Node'Unrestricted_Access);
               Check_Copied_EBNF;
            end if;
            I := I + 1;
         end loop;
      end;

      declare
         use Ada.Text_IO;
      begin
         for Node of Copied_EBNF_Nodes loop
            if Node /= Deleted_Child then
               Put_Line
                 (Current_Error,
                  Error_Message
                    (Tree, Data.Terminals, Node, Data.Grammar_Lexer.File_Name,
                     Tree.Image
                       (Node, Wisitoken_Grammar_Actions.Descriptor,
                        Include_RHS_Index => True,
                        Include_Children  => Trace_Generate_EBNF > Detail,
                        Node_Numbers      => True)));
               Put_Line (Current_Error, "... copied EBNF node not translated");
            end if;
         end loop;
      end;

      EBNF_Allowed := False;
      Tree.Validate_Tree
        (Data.Terminals, Wisitoken_Grammar_Actions.Descriptor, Data.Grammar_Lexer.File_Name, Tree.Root,
         Validate_Node'Unrestricted_Access);

      Data.Meta_Syntax := BNF_Syntax;

      if Trace_Generate_EBNF > Detail then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("Identifiers:");
         for I in Data.Tokens.Virtual_Identifiers.First_Index .. Data.Tokens.Virtual_Identifiers.Last_Index loop
            Ada.Text_IO.Put_Line (Base_Identifier_Index'Image (I) & " " & (-Data.Tokens.Virtual_Identifiers (I)));
         end loop;
      end if;
   end Translate_EBNF_To_BNF;

   procedure Print_Source
     (File_Name : in String;
      Tree      : in WisiToken.Syntax_Trees.Tree;
      Data      : in User_Data_Type)
   is
      use Ada.Text_IO;
      use WisiToken.Syntax_Trees;

      File : File_Type;

      procedure Put_Comments
        (Node           : in Valid_Node_Index;
         Force_New_Line : in Boolean := False;
         Force_Comment  : in String  := "")
      is
         Last_Term : constant Node_Index              := Tree.Last_Terminal (Node);
         Aug       : constant Base_Token_Class_Access :=
           (if Last_Term = Invalid_Node_Index
            then null
            else Tree.Augmented (Last_Term));

         Comments_Include_Newline : Boolean := False;
      begin
         if Aug = null then
            if Force_Comment /= "" then
               Put_Line (File, Force_Comment);

            elsif Force_New_Line then
               New_Line (File);
            end if;
         else
            for Token of Augmented_Token_Access (Aug).Non_Grammar loop
               if Token.ID = +NEW_LINE_ID then
                  Comments_Include_Newline := True;
               end if;
               Put (File, Data.Grammar_Lexer.Buffer_Text (Token.Byte_Region));
            end loop;
            if Force_New_Line and not Comments_Include_Newline then
               New_Line (File);
            end if;
         end if;
      end Put_Comments;

      procedure Put_Declaration_Item (Node : in Valid_Node_Index)
      is
         Children : constant Valid_Node_Index_Array := Tree.Children (Node);
      begin
         case To_Token_Enum (Tree.ID (Children (1))) is
         when IDENTIFIER_ID | NUMERIC_LITERAL_ID | STRING_LITERAL_1_ID | STRING_LITERAL_2_ID =>
            Put (File, ' ' & Get_Text (Data, Tree, Children (1)));
         when REGEXP_ID =>
            Put (File, " %[" & Get_Text (Data, Tree, Children (1)) & "]%");
         when others =>
            Put (File, Image (Tree.ID (Children (1)), Wisitoken_Grammar_Actions.Descriptor));
         end case;
      end Put_Declaration_Item;

      procedure Put_Declaration_Item_List (Node : in Valid_Node_Index)
      is
         Children : constant Valid_Node_Index_Array := Tree.Children (Node);
      begin
         if Children'Length = 1 then
            Put_Declaration_Item (Children (1));
         else
            Put_Declaration_Item_List (Children (1));
            Put_Declaration_Item (Children (2));
         end if;
      end Put_Declaration_Item_List;

      procedure Put_Identifier_List (Node : in Valid_Node_Index)
      is
         Children : constant Valid_Node_Index_Array := Tree.Children (Node);
      begin
         if Children'Length = 1 then
            Put (File, Get_Text (Data, Tree, Children (1)));
         else
            Put_Identifier_List (Children (1));
            Put (File, ' ');
            Put (File, Get_Text (Data, Tree, Children (2)));
         end if;
      end Put_Identifier_List;

      procedure Put_RHS_Element (Node : in Valid_Node_Index)
      with Pre => Tree.ID (Node) = +rhs_element_ID
      is begin
         --  We don't raise an exception for errors here; it's easier to debug from the
         --  mangled source listing.

         case Tree.RHS_Index (Node) is
         when 0 =>
            Put (File, Get_Text (Data, Tree, Node));

         when 1 =>
            --  Output no spaces around "="
            declare
               Children : constant Valid_Node_Index_Array := Tree.Children (Node);
            begin
               Put (File, Get_Text (Data, Tree, Children (1)) & "=" & Get_Text (Data, Tree, Children (3)));
            end;

         when others =>
            New_Line (File);
            Put (File, " ;; not translated: " & Node_Index'Image (Node) & ":" &
                   Tree.Image (Node, Wisitoken_Grammar_Actions.Descriptor,
                               Include_Children  => True,
                               Include_RHS_Index => True,
                               Node_Numbers      => True));
         end case;
      exception
      when SAL.Programmer_Error =>
         raise;

      when E : others =>
         declare
            use Ada.Exceptions;
         begin
            Raise_Programmer_Error
              ("Put_RHS_Element: " & Exception_Name (E) & ": " & Exception_Message (E), Data, Tree, Node);
         end;
      end Put_RHS_Element;

      procedure Put_RHS_Item_List (Node : in Valid_Node_Index)
      with Pre => Tree.ID (Node) = +rhs_item_list_ID
      is
         Children : constant Valid_Node_Index_Array := Tree.Children (Node);
      begin
         if Children'Length = 1 then
            Put_RHS_Element (Children (1));
         else
            Put_RHS_Item_List (Children (1));
            Put (File, ' ');
            Put_RHS_Element (Children (2));
         end if;
      exception
      when SAL.Programmer_Error =>
         raise;

      when E : others =>
         declare
            use Ada.Exceptions;
         begin
            Raise_Programmer_Error
              ("Put_RHS_Item_List: " & Exception_Name (E) & ": " & Exception_Message (E), Data, Tree, Node);
         end;
      end Put_RHS_Item_List;

      procedure Put_RHS
        (Node  : in Valid_Node_Index;
         First : in Boolean)
      with Pre => Tree.ID (Node) = +rhs_ID
      is
         Children : constant Valid_Node_Index_Array := Tree.Children (Node);
      begin
         Put (File, (if First then "  : " else "  | "));
         case Tree.RHS_Index (Node) is
         when 0 =>
            Put_Comments (Tree.Parent (Node), Force_Comment => ";; empty");

         when 1 .. 3 =>
            Put_RHS_Item_List (Children (1));
            Put_Comments (Children (1), Force_New_Line => True);

            if Tree.RHS_Index (Node) > 1 then
               Put (File, "    %(" & Get_Text (Data, Tree, Children (2)) & ")%"); -- action
               Put_Comments (Children (2), Force_New_Line => True);

               if Tree.RHS_Index (Node) > 2 then
                  Put (File, "    %(" & Get_Text (Data, Tree, Children (3)) & ")%"); -- check
                  Put_Comments (Children (3), Force_New_Line => True);
               end if;
            end if;

         when others =>
            Raise_Programmer_Error ("Put_RHS", Data, Tree, Node);
         end case;
      exception
      when SAL.Programmer_Error =>
         raise;

      when E : others =>
         declare
            use Ada.Exceptions;
         begin
            Raise_Programmer_Error ("Put_RHS: " & Exception_Name (E) & ": " & Exception_Message (E), Data, Tree, Node);
         end;
      end Put_RHS;

      procedure Put_RHS_List
        (Node    : in     Valid_Node_Index;
         First   : in out Boolean;
         Virtual : in     Boolean)
      with Pre => Tree.ID (Node) = +rhs_list_ID
      is
         Children : constant Valid_Node_Index_Array := Tree.Children (Node);
      begin
         case Tree.RHS_Index (Node) is
         when 0 =>
            Put_RHS (Children (1), First);
            First := False;
         when 1 =>
            Put_RHS_List (Children (1), First, Virtual);
            Put_RHS (Children (3), First => False);
         when 2 =>
            Put
              (File, "%if " & Get_Text (Data, Tree, Children (3)) & " = " & Get_Text (Data, Tree, Children (4)));
            Put_Comments (Node);

         when 3 =>
            Put (File, "%end if");
            Put_Comments (Node);

         when others =>
            Raise_Programmer_Error ("Put_RHS_List", Data, Tree, Node);
         end case;
      exception
      when SAL.Programmer_Error =>
         raise;

      when E : others =>
         declare
            use Ada.Exceptions;
         begin
            Raise_Programmer_Error
              ("Put_RHS_List: " & Exception_Name (E) & ": " & Exception_Message (E), Data, Tree, Node);
         end;
      end Put_RHS_List;

      procedure Process_Node (Node : in Valid_Node_Index)
      is begin
         case To_Token_Enum (Tree.ID (Node)) is
         --  Enum_Token_ID alphabetical order
         when compilation_unit_ID =>
            Process_Node (Tree.Child (Node, 1));

         when compilation_unit_list_ID =>
            declare
               Children : constant Valid_Node_Index_Array := Tree.Children (Node);
            begin
               case To_Token_Enum (Tree.ID (Children (1))) is
               when compilation_unit_list_ID =>
                  Process_Node (Children (1));
                  Process_Node (Children (2));
               when compilation_unit_ID =>
                  Process_Node (Children (1));
               when others =>
                  raise SAL.Programmer_Error;
               end case;
            end;

         when declaration_ID =>
            declare
               Children : constant Valid_Node_Index_Array := Tree.Children (Node);
            begin
               case Tree.RHS_Index (Node) is
               when 0 =>
                  case Tree.RHS_Index (Children (2)) is
                  when 0 =>
                     Put (File, "%keyword");
                  when 1 =>
                     Put (File, "%non_grammar <" & Get_Text (Data, Tree, Tree.Child (Children (2), 3)) & ">");
                  when 2 =>
                     Put (File, "%token <" & Get_Text (Data, Tree, Tree.Child (Children (2), 3)) & ">");
                  when others =>
                     raise SAL.Programmer_Error;
                  end case;

                  Put (File, " " & Get_Text (Data, Tree, Children (3)));
                  Put_Declaration_Item_List (Children (4));
                  Put_Comments (Children (4), Force_New_Line => True);

               when 1 =>
                  Put (File, "%code ");
                  Put_Identifier_List (Children (3));
                  Put (File, " %{" & Get_Text (Data, Tree, Children (4)) & "}%"); -- RAW_CODE
                  Put_Comments (Node);

               when 2 =>
                  declare
                     Key : constant String := Get_Text (Data, Tree, Children (2));
                  begin
                     if Key = "conflict" then
                        Put (File, Data.Grammar_Lexer.Buffer_Text (Tree.Byte_Region (Node)));
                     else
                        Put (File, "%" & Key);
                        Put_Declaration_Item_List (Children (3));
                     end if;
                  end;
                  Put_Comments (Children (3));

               when 3 =>
                  Put (File, "%" & Get_Text (Data, Tree, Children (2)));
                  Put_Comments (Children (2));

               when 4 =>
                  Put
                    (File, "%if" & Get_Text (Data, Tree, Children (2)) & " = " & Get_Text (Data, Tree, Children (4)));
                  Put_Comments (Node);

               when 5 =>
                  Put (File, "%end if");
                  Put_Comments (Node);

               when others =>
                  raise SAL.Programmer_Error;
               end case;
            end;

         when nonterminal_ID =>
            declare
               Children : constant Valid_Node_Index_Array := Tree.Children (Node);
               Virtual  : constant Boolean                := Tree.Label (Children (1)) = Virtual_Identifier;
               First    : Boolean                         := True;
            begin
               Put (File, Get_Text (Data, Tree, Children (1)));
               Put_Comments (Children (1), Force_New_Line => True);

               Put_RHS_List (Children (3), First, Virtual);

               if Tree.Children (Children (4))'Length > 0 then
                  if Virtual then
                     Put_Line (File, "  ;");
                  else
                     Put (File, "  ;");
                     Put_Comments (Children (4));
                  end if;
               end if;
            end;

         when wisitoken_accept_ID =>
            Process_Node (Tree.Child (Node, 1));

         when others =>
            raise SAL.Not_Implemented with Image (Tree.ID (Node), Wisitoken_Grammar_Actions.Descriptor);
         end case;
      end Process_Node;
   begin
      Create (File, Out_File, File_Name);
      Put_Line (File, ";;; generated from " & Data.Grammar_Lexer.File_Name & " -*- buffer-read-only:t -*-");
      Put_Line (File, ";;;");

      for Token of Data.Leading_Non_Grammar loop
         Put (File, Data.Grammar_Lexer.Buffer_Text (Token.Byte_Region));
      end loop;

      Process_Node (Tree.Root);

      Close (File);
   exception
   when E : SAL.Not_Implemented =>
      Close (File);
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error, "Print_Source not implemented: " & Ada.Exceptions.Exception_Message (E));
   end Print_Source;

end WisiToken_Grammar_Editing;
--  Local Variables:
--  ada-which-func-parse-size: 50000
--  End:
