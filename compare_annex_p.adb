--  Abstract :
--
--  Compare ARM Annex P to ada_annex_p.wy
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

with Ada.Command_Line;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with SAL;
with WisiToken.BNF.Generate_Utils;
with WisiToken.Parse.LR.Parser_No_Recover;
with WisiToken.Syntax_Trees.LR_Utils;
with WisiToken_Grammar_Runtime;
with Wisitoken_Grammar_Actions;
procedure Compare_Annex_P
is
   --  Usage: compare_annex_p <upstream.wy> <downstream.wy> [verbosity]
   --
   --  upstream.wy is from ARM Annex P via annex_p_to_wy.adb
   --  downstream.wy is the working verion, with grammar actions.

   Upstream_WY_Source   : constant String := Ada.Command_Line.Argument (1);
   Downstream_WY_Source : constant String := Ada.Command_Line.Argument (2);

   Verbosity : constant Integer :=
     (if Ada.Command_Line.Argument_Count >= 3
      then Integer'Value (Ada.Command_Line.Argument (3))
      else 0);

   type String_Array is array (Positive range <>) of Ada.Strings.Unbounded.String_Access;

   Upstream_Parser   : WisiToken.Parse.LR.Parser_No_Recover.Parser;
   Downstream_Parser : WisiToken.Parse.LR.Parser_No_Recover.Parser;

   Virtual_Identifiers : WisiToken.BNF.String_Arrays.Vector;
   --  Empty because we are not translating to BNF; required by
   --  wisitoken_grammar_runtime Get_Text.

   Up_Pad : constant String := Ada.Strings.Fixed."*"
     (Integer'Max (0, Ada.Directories.Simple_Name (Downstream_WY_Source)'Length -
                     Ada.Directories.Simple_Name (Upstream_WY_Source)'Length),
      ' ');

   Down_Pad : constant String := Ada.Strings.Fixed."*"
     (Integer'Max (0, Ada.Directories.Simple_Name (Upstream_WY_Source)'Length -
                     Ada.Directories.Simple_Name (Downstream_WY_Source)'Length),
      ' ');

   Error_Count : Integer := 0;
begin
   WisiToken.BNF.Generate_Utils.Parse_Grammar_File (Upstream_Parser, Upstream_WY_Source);
   WisiToken.BNF.Generate_Utils.Parse_Grammar_File (Downstream_Parser, Downstream_WY_Source);

   declare
      use WisiToken;
      use WisiToken.Syntax_Trees.LR_Utils;
      use Wisitoken_Grammar_Actions;

      Upstream_Comp_Units : constant Constant_List     := Creators.Create_List
        (Upstream_Parser.Tree_Var_Ref, Upstream_Parser.Tree.Child (Upstream_Parser.Tree.Root, 1),
         +compilation_unit_list_ID, +compilation_unit_ID);
      Upstream_Iter       : constant Constant_Iterator := Upstream_Comp_Units.Iterate_Constant;
      Upstream_Cur        : Cursor                     := Upstream_Iter.First;

      Downstream_Comp_Units : constant Constant_List     := Creators.Create_List
        (Downstream_Parser.Tree_Var_Ref, Downstream_Parser.Tree.Child (Downstream_Parser.Tree.Root, 1),
         +compilation_unit_list_ID, +compilation_unit_ID);
      Downstream_Iter       : constant Constant_Iterator := Downstream_Comp_Units.Iterate_Constant;
      Downstream_Cur        : Cursor                     := Downstream_Iter.First;

      function Upstream_ID return Token_Enum_ID
      is begin
         return -Upstream_Parser.Tree.ID (Upstream_Parser.Tree.Child (Element (Upstream_Cur), 1));
      end Upstream_ID;

      function Upstream_Nonterm return Valid_Node_Index
      is (Upstream_Parser.Tree.Child (Element (Upstream_Cur), 1));

      function Up_Text (Node : in Valid_Node_Index) return String
      is (WisiToken_Grammar_Runtime.Get_Text
            (Upstream_Parser.Terminals, Upstream_Parser.Lexer, Virtual_Identifiers, Upstream_Parser.Tree, Node));

      function Upstream_Error_Message (Msg : in String) return String
      is (Syntax_Trees.Error_Message
            (Upstream_Parser.Tree, Upstream_Parser.Terminals, Element (Upstream_Cur),
             Ada.Directories.Simple_Name (Upstream_WY_Source), Msg));

      function Up_Child (Node : in Valid_Node_Index; Child : in Positive_Index_Type) return Valid_Node_Index
      is (Upstream_Parser.Tree.Child (Node, Child));

      function Downstream_ID return Token_Enum_ID
      is begin
         return -Downstream_Parser.Tree.ID (Downstream_Parser.Tree.Child (Element (Downstream_Cur), 1));
      end Downstream_ID;

      function Downstream_Nonterm return Valid_Node_Index
      is (Downstream_Parser.Tree.Child (Element (Downstream_Cur), 1));

      function Down_Text (Node : in Valid_Node_Index) return String
      is (WisiToken_Grammar_Runtime.Get_Text
            (Downstream_Parser.Terminals, Downstream_Parser.Lexer, Virtual_Identifiers, Downstream_Parser.Tree, Node));

      function Downstream_Error_Message (Msg : in String) return String
      is (Syntax_Trees.Error_Message
            (Downstream_Parser.Tree, Downstream_Parser.Terminals, Element (Downstream_Cur),
             Ada.Directories.Simple_Name (Downstream_WY_Source), Msg));

      function Down_Child (Node : in Valid_Node_Index; Child : in Positive_Index_Type) return Valid_Node_Index
      is (Downstream_Parser.Tree.Child (Node, Child));

      function Build_Inlined_Up_Nonterms return WisiToken.BNF.String_Pair_Lists.List
      is begin
         return Result : WisiToken.BNF.String_Pair_Lists.List do
            Result.Append ((+"defining_identifier", +"IDENTIFIER"));
            Result.Append ((+"subtype_mark", +"name"));
            Result.Append ((+"defining_character_literal", +"CHARACTER_LITERAL"));
            Result.Append ((+"operator_symbol", +"STRING_LITERAL"));
            Result.Append ((+"defining_operator_symbol", +"STRING_LITERAL"));
            Result.Append ((+"prefix", +"name"));
            --  Result.Append ((+""));
            --  Result.Append ((+""));
            --  Result.Append ((+""));
            --  Result.Append ((+""));
         end return;
      end Build_Inlined_Up_Nonterms;

      Inlined_Up_Nonterms : constant WisiToken.BNF.String_Pair_Lists.List := Build_Inlined_Up_Nonterms;

      function Is_Inlined (Nonterm : in Valid_Node_Index) return Boolean
      is
         Tree : Syntax_Trees.Tree renames Upstream_Parser.Tree_Var_Ref.Element.all;

         Nonterm_Name : constant String := Up_Text (Tree.Child (Nonterm, 1));
      begin
         return (for some Pair of Inlined_Up_Nonterms => -Pair.Name = Nonterm_Name);
      end Is_Inlined;

      function Up_Text_Inlined (Nonterm : in Valid_Node_Index) return String
      is
         use Ada.Strings.Unbounded;

         Tree : Syntax_Trees.Tree renames Upstream_Parser.Tree_Var_Ref.Element.all;
         Children : constant Valid_Node_Index_Array := Tree.Children (Nonterm);
         RHS_List : constant Constant_List := Creators.Create_List
           (Upstream_Parser.Tree_Var_Ref, Children (3), +rhs_list_ID, +rhs_ID);

         Result : Unbounded_String := +Up_Text (Children (1)) & " " & Up_Text (Children (2)) & " ";

         Need_Bar : Boolean := False;

         function Do_Inlined (Orig : in String) return String
         is begin
            for Pair of Inlined_Up_Nonterms loop
               if -Pair.Name = Orig then
                  return -Pair.Value;
               end if;
            end loop;
            return Orig;
         end Do_Inlined;

         function Get_Inlined (Node : in Valid_Node_Index) return String
         with Pre => To_Token_Enum (Tree.ID (Node)) in rhs_ID | rhs_alternative_list_ID | rhs_item_list_ID
         is
            Result : Unbounded_String;
         begin
            case To_Token_Enum (Tree.ID (Node)) is
            when rhs_ID =>
               if Tree.RHS_Index (Node) = 0 then
                  return "";
               else
                  return Get_Inlined (Tree.Child (Node, 1)); --  rhs_item_list
               end if;

            when rhs_item_list_ID =>
               declare
                  Item_List : constant Constant_List := Creators.Create_List
                    (Tree, Node, +rhs_item_list_ID, +rhs_element_ID);
                  Need_Space : Boolean := False;
               begin
                  for Node of Item_List loop
                     declare
                        Item : constant Valid_Node_Index := Tree.Find_Descendant (Node, +rhs_item_ID);
                     begin
                        Result := Result & (if Need_Space then " " else "");
                        Need_Space := True;

                        case Tree.RHS_Index (Item) is
                        when 0 =>
                           Result := Result & Do_Inlined (Up_Text (Item));

                        when 4 =>
                           pragma Assert (Tree.ID (Tree.Child (Item, 1)) = +rhs_multiple_item_ID);
                           declare
                              Children : constant Valid_Node_Index_Array := Tree.Children (Tree.Child (Item, 1));
                           begin
                              case Children'Length is
                              when 3 | 4 =>
                                 Result := Result & Up_Text (Children (1)) & " " & Get_Inlined (Children (2)) & " " &
                                   Up_Text (Children (3)) &
                                   (if Children'Length = 4 then Up_Text (Children (4)) else "");
                              when 2 =>
                                 Result := Result & Do_Inlined (Up_Text (Children (1))) & " " & Up_Text (Children (2));
                              when others =>
                                 raise SAL.Programmer_Error;
                              end case;
                           end;

                        when 1 | 2 | 3 | 5 =>
                           --  So far no nested optional or group
                           Result := Result & Up_Text (Item);
                        when others =>
                           raise SAL.Programmer_Error;
                        end case;
                     end;
                  end loop;
               end;

            when rhs_alternative_list_ID =>
               declare
                  Alt_List : constant Constant_List := Creators.Create_List
                    (Upstream_Parser.Tree_Var_Ref, Node, +rhs_alternative_list_ID, +rhs_item_list_ID);
                  Need_Bar : Boolean := False;
               begin
                  for Item_List of Alt_List loop
                     Result := Result & (if Need_Bar then " | " else "") & Get_Inlined (Item_List);
                     Need_Bar := True;
                  end loop;
               end;

            when others =>
               raise SAL.Programmer_Error;
            end case;

            return -Result;
         end Get_Inlined;
      begin
         for RHS of RHS_List loop
            Result := Result & (if Need_Bar then " | " else "") & Get_Inlined (RHS);
            Need_Bar := True;
         end loop;
         declare
            Semicolon : constant String := Up_Text (Children (4));
         begin
            if Semicolon'Length > 0 then
               Result := Result & " " & Semicolon;
            end if;
         end;
         return To_String (Result);
      end Up_Text_Inlined;

      function Down_Text_Redundant (Nonterm : in Valid_Node_Index) return String
      --  Get text of Nonterm tokens from Downstream tree, but include
      --  ;;redundant comments as code, and substitute Inlined_Up_Nonterms.
      is
         use Ada.Strings.Unbounded;

         Tree : Syntax_Trees.Tree renames Downstream_Parser.Tree_Var_Ref.Element.all;
         Children : constant Valid_Node_Index_Array := Tree.Children (Nonterm);
         RHS_List : constant Constant_List := Creators.Create_List
           (Downstream_Parser.Tree_Var_Ref, Children (3), +rhs_list_ID, +rhs_ID);

         Result : Unbounded_String := +Down_Text (Children (1)) & " " & Down_Text (Children (2)) & " ";

         Need_Bar : Boolean := False;

         function Is_New (RHS : in Valid_Node_Index) return Boolean
         is
            use Ada.Strings.Fixed;
            --  A new RHS looks like:
            --      pragma_g ;; new
            New_Pattern : constant String := ";; new";

            Aug : constant Base_Token_Class_Access := Tree.Augmented (Tree.Last_Terminal (RHS));
         begin
            if Aug = null then
               return False;
            end if;
            declare
               Non_Grammar : WisiToken.Base_Token_Arrays.Vector renames WisiToken_Grammar_Runtime.Augmented_Token_Access
                 (Aug).Non_Grammar;
            begin
               for Tok of Non_Grammar loop
                  if Tok.ID = +COMMENT_ID then
                     declare
                        Line : constant String := Downstream_Parser.Lexer.Buffer_Text (Tok.Byte_Region);
                        Red_Index : constant Integer := Index (Line, New_Pattern);
                     begin
                        return Red_Index /= 0;
                     end;
                  end if;
               end loop;
               return False;
            end;
         end Is_New;

         procedure Get_Redundant (RHS : in Valid_Node_Index)
         is
            use Ada.Strings.Fixed;
            --  A redundant RHS looks like:
            --    ;; [ IDENTIFIER '=>' ] name ;; redundant with expression
            --
            Red_Pattern : constant String := ";; redundant";

            Aug : constant Base_Token_Class_Access := Tree.Augmented (Tree.Last_Terminal (RHS));
         begin
            if Aug = null then
               return;
            end if;
            declare
               Non_Grammar : WisiToken.Base_Token_Arrays.Vector renames WisiToken_Grammar_Runtime.Augmented_Token_Access
                 (Aug).Non_Grammar;
            begin
               for Tok of Non_Grammar loop
                  if Tok.ID = +COMMENT_ID then
                     declare
                        Line : constant String := Downstream_Parser.Lexer.Buffer_Text (Tok.Byte_Region);
                        Red_Index : constant Integer := Index (Line, Red_Pattern);
                        Comment_Index : constant Integer := Index (Line, ";;");
                     begin
                        if Red_Index = 0 then
                           null;
                        else
                           Result := Result &
                             (if Need_Bar then " | " else "") &
                             Line (Comment_Index + 3 .. Red_Index - 2);
                           Need_Bar := True;
                        end if;
                     end;
                  end if;
               end loop;
            end;
         end Get_Redundant;

      begin
         Get_Redundant (Children (2)); -- original first RHS, after '::='

         for RHS of RHS_List loop
            if Is_New (RHS) then
               null;
            else
               Result := Result & (if Need_Bar then " | " else "") & Down_Text (RHS);
               Need_Bar := True;
               Get_Redundant (RHS);
            end if;
         end loop;
         declare
            Semicolon : constant String := Down_Text (Children (4));
         begin
            if Semicolon'Length > 0 then
               Result := Result & " " & Semicolon;
            end if;
         end;
         return To_String (Result);
      end Down_Text_Redundant;

      Exclude_Up_Nonterms : constant String_Array :=
        --  These are replaced by regexp.
        (new String'("IDENTIFIER"),
         new String'("identifier_start"),
         new String'("identifier_extend"),
         new String'("numeric_literal"),
         new String'("decimal_literal"),
         new String'("numeral"),
         new String'("exponent"),
         new String'("digit"),
         new String'("based_literal"),
         new String'("base"),
         new String'("based_numeral"),
         new String'("extended_digit"),
         new String'("character_literal"),
         new String'("string_literal"),
         new String'("string_element"),
         new String'("comment"),

         --  These are redundant with something
         new String'("indexed_component"));

      function Up_Exclude (Nonterm : in Valid_Node_Index) return Boolean
      is (for some Ptr of Exclude_Up_Nonterms => Ptr.all = Up_Text (Up_Child (Nonterm, 1)));

   begin
      loop
         exit when not Has_Element (Upstream_Cur) or not Has_Element (Downstream_Cur);
         if Upstream_ID = nonterminal_ID and then not Up_Exclude (Upstream_Nonterm) then
            if Downstream_ID = nonterminal_ID then
               declare
                  Up_Nonterm   : constant Valid_Node_Index := Upstream_Nonterm;
                  Down_Nonterm : constant Valid_Node_Index := Downstream_Nonterm;
               begin
                  if Is_Inlined (Up_Nonterm) then
                     if Verbosity > 0 then
                        Put_Line (Up_Text (Up_Child (Up_Nonterm, 1)) & " inlined");
                     end if;
                     Upstream_Cur := Upstream_Iter.Next (Upstream_Cur);

                  else
                     declare
                        Up   : constant String := Up_Text_Inlined (Up_Nonterm);
                        Down : constant String := Down_Text_Redundant (Down_Nonterm);
                     begin
                        if Up = Down then
                           if Verbosity > 0 then
                              Put_Line (Up_Text (Up_Child (Up_Nonterm, 1)) & " = " &
                                          Down_Text (Down_Child (Down_Nonterm, 1)));
                           end if;
                        else
                           Put_Line (Standard_Error, Downstream_Error_Message (Down_Pad & Down));
                           Put_Line (Standard_Error, Upstream_Error_Message (Up_Pad & Up));
                           Error_Count := @ + 1;

                           if Up_Text (Up_Child (Up_Nonterm, 1)) /= Down_Text (Down_Child (Down_Nonterm, 1)) then
                              --  Missing a nonterm; all subsequent error messages would be bogus
                              exit;
                           end if;
                        end if;

                        Upstream_Cur   := Upstream_Iter.Next (Upstream_Cur);
                        Downstream_Cur := Downstream_Iter.Next (Downstream_Cur);
                     end;
                  end if;
               end;
            else
               Downstream_Cur := Downstream_Iter.Next (Downstream_Cur);
            end if;
         else
            Upstream_Cur   := Upstream_Iter.Next (Upstream_Cur);
         end if;
      end loop;
   end;

   if Error_Count > 0 then
      declare
         use Ada.Command_Line;
      begin
         Set_Exit_Status (Failure);
      end;
   end if;
end Compare_Annex_P;
