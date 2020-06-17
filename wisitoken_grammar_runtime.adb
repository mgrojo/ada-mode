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

with Ada.Exceptions;
with Ada.Strings.Unbounded;
with SAL;
with WisiToken.Generate;   use WisiToken.Generate;
package body WisiToken_Grammar_Runtime is

   use WisiToken;
   use Wisitoken_Grammar_Actions;

   ----------
   --  Body subprograms, misc order

   function Get_Line
     (Data : in User_Data_Type;
      Tree : in Syntax_Trees.Tree;
      Node : in WisiToken.Valid_Node_Index)
     return WisiToken.Line_Number_Type
   is
      --  Find a source line for Node.

      use WisiToken.Syntax_Trees;

      Temp : Node_Index := Node;
   begin
      loop
         if Tree.First_Shared_Terminal (Temp) = Invalid_Token_Index then
            --  Node is empty or all virtual_identifiers; try parents.
            Temp := Tree.Parent (Temp);
            exit when Temp = Invalid_Node_Index;
         else
            return Data.Terminals.all (Tree.First_Shared_Terminal (Temp)).Line;
         end if;
      end loop;
      return Invalid_Line_Number;
   end Get_Line;

   function Get_Text
     (Data         : in User_Data_Type;
      Tree         : in Syntax_Trees.Tree;
      Tree_Index   : in Valid_Node_Index;
      Strip_Quotes : in Boolean := False)
     return String
   is
      use all type Syntax_Trees.Node_Label;

      function Strip_Delimiters (Tree_Index : in Valid_Node_Index) return String
      is
         Region : Buffer_Region renames Data.Terminals.all (Tree.Terminal (Tree_Index)).Byte_Region;
      begin
         if -Tree.ID (Tree_Index) in RAW_CODE_ID | REGEXP_ID | ACTION_ID then
            --  Strip delimiters. We don't strip leading/trailing spaces to preserve indent.
            return Data.Grammar_Lexer.Buffer_Text ((Region.First + 2, Region.Last - 2));

         elsif -Tree.ID (Tree_Index) in STRING_LITERAL_1_ID | STRING_LITERAL_2_ID and Strip_Quotes then
            return Data.Grammar_Lexer.Buffer_Text ((Region.First + 1, Region.Last - 1));
         else
            return Data.Grammar_Lexer.Buffer_Text (Region);
         end if;
      end Strip_Delimiters;

   begin
      case Tree.Label (Tree_Index) is
      when Shared_Terminal =>
         return Strip_Delimiters (Tree_Index);

      when Virtual_Terminal =>
         --  Terminal keyword inserted during tree edit. We could check for
         --  Identifier, but that will be caught later.
         return Image (Tree.ID (Tree_Index), Wisitoken_Grammar_Actions.Descriptor);

      when Virtual_Identifier =>
         if Strip_Quotes then
            declare
               Quoted : constant String := -Data.Tokens.Virtual_Identifiers (Tree.Identifier (Tree_Index));
            begin
               return Quoted (Quoted'First + 1 .. Quoted'Last - 1);
            end;
         else
            return -Data.Tokens.Virtual_Identifiers (Tree.Identifier (Tree_Index));
         end if;

      when Nonterm =>
         declare
            use all type Ada.Strings.Unbounded.Unbounded_String;
            Result       : Ada.Strings.Unbounded.Unbounded_String;
            Tree_Indices : constant Valid_Node_Index_Array := Tree.Get_Terminals (Tree_Index);
            Need_Space   : Boolean                                      := False;
         begin
            for Tree_Index of Tree_Indices loop
               Result := Result & (if Need_Space then " " else "") &
                 Get_Text (Data, Tree, Tree_Index, Strip_Quotes);
               Need_Space := True;
            end loop;
            return -Result;
         end;
      end case;
   end Get_Text;

   function Get_Item_Text
     (Data         : in User_Data_Type;
      Tree         : in WisiToken.Syntax_Trees.Tree;
      Node         : in WisiToken.Valid_Node_Index;
      Strip_Quotes : in Boolean := False)
     return String
   is begin
      return Get_Text (Data, Tree, Tree.Find_Descendant (Node, +rhs_item_ID), Strip_Quotes);
   end Get_Item_Text;

   function Get_Child_Text
     (Data         : in User_Data_Type;
      Tree         : in Syntax_Trees.Tree;
      Parent       : in Valid_Node_Index;
      Child        : in SAL.Peek_Type;
      Strip_Quotes : in Boolean := False)
     return String
   is
      Tree_Indices : constant Valid_Node_Index_Array := Tree.Get_Terminals (Parent);
   begin
      return Get_Text (Data, Tree, Tree_Indices (Child), Strip_Quotes);
   end Get_Child_Text;

   procedure Start_If_1
     (Data    : in out User_Data_Type;
      Tree    : in     Syntax_Trees.Tree;
      A_Index : in     Valid_Node_Index;
      B_Index : in     Valid_Node_Index)
   is
      use all type WisiToken.BNF.Generate_Algorithm;
      use all type WisiToken.BNF.Lexer_Type;
   begin
      if "lexer" = Get_Text (Data, Tree, A_Index) then
         Data.If_Lexer_Present := True;
         Data.Ignore_Lines     := Data.User_Lexer /= WisiToken.BNF.To_Lexer (Get_Text (Data, Tree, B_Index));

      elsif "parser" = Get_Text (Data, Tree, A_Index) then
         Data.If_Parser_Present := True;
         Data.Ignore_Lines := Data.User_Parser /= WisiToken.BNF.Generate_Algorithm'Value
           (Get_Text (Data, Tree, B_Index));

      else
         raise Grammar_Error with
           Error_Message
             (Data.Grammar_Lexer.File_Name, Data.Terminals.all (Tree.First_Shared_Terminal (A_Index)).Line,
              "invalid '%if'; must be one of {lexer | parser}");
      end if;
   end Start_If_1;

   function Get_RHS
     (Data   : in out User_Data_Type;
      Tree   : in     Syntax_Trees.Tree;
      Labels : in out WisiToken.BNF.String_Arrays.Vector;
      Token  : in     Valid_Node_Index)
     return WisiToken.BNF.RHS_Type
   with Pre => Tree.ID (Token) = +rhs_ID
   is
      use all type SAL.Base_Peek_Type;
      Children : constant Valid_Node_Index_Array := Tree.Children (Token);
   begin
      return RHS : WisiToken.BNF.RHS_Type do
         RHS.Source_Line := Get_Line (Data, Tree, Token);

         if Children'Length > 0 then
            for I of Tree.Get_IDs (Children (1), +rhs_element_ID) loop
               case Tree.RHS_Index (I) is
               when 0 =>
                  --  rhs_item
                  RHS.Tokens.Append
                    ((Label      => +"",
                      Identifier => +Get_Text (Data, Tree, Tree.Child (I, 1))));

               when 1 =>
                  --  IDENTIFIER = rhs_item
                  declare
                     Label : constant String := Get_Text (Data, Tree, Tree.Child (I, 1));
                  begin
                     RHS.Tokens.Append
                       ((Label      => +Label,
                         Identifier => +Get_Text (Data, Tree, Tree.Child (I, 3))));

                     if (for all L of Labels => -L /= Label) then
                        Labels.Append (+Label);
                     end if;
                  end;

               when others =>
                  Raise_Programmer_Error ("Get_RHS; unimplimented token", Data, Tree, I);
               end case;
            end loop;

            if Children'Last >= 2 then
               declare
                  Text : constant String := Get_Text (Data, Tree, Children (2));
               begin
                  if Text'Length > 0 and (for some C of Text => C /= ' ') then
                     RHS.Action := +Text;
                     Data.Action_Count := Data.Action_Count + 1;
                  end if;
               end;
            end if;

            if Children'Last >= 3 then
               RHS.Check := +Get_Text (Data, Tree, Children (3));
               Data.Check_Count := Data.Check_Count + 1;
            end if;
         end if;
      end return;
   exception
   when SAL.Programmer_Error =>
      raise;
   when E : others =>
      declare
         use Ada.Exceptions;
      begin
         Raise_Programmer_Error ("Get_RHS: " & Exception_Name (E) & ": " & Exception_Message (E), Data, Tree, Token);
      end;
   end Get_RHS;

   procedure Get_Right_Hand_Sides
     (Data             : in out User_Data_Type;
      Tree             : in     WisiToken.Syntax_Trees.Tree;
      Right_Hand_Sides : in out WisiToken.BNF.RHS_Lists.List;
      Labels           : in out WisiToken.BNF.String_Arrays.Vector;
      Token            : in     WisiToken.Valid_Node_Index)
   with Pre => Tree.ID (Token) = +rhs_list_ID
   is
      Tokens : constant Valid_Node_Index_Array := Tree.Children (Token);
   begin
      case Tree.RHS_Index (Token) is
      when 0 =>
         --  | rhs
         if not Data.Ignore_Lines then
            Right_Hand_Sides.Append (Get_RHS (Data, Tree, Labels, Tokens (1)));
         end if;

      when 1 =>
         --  | rhs_list BAR rhs
         Get_Right_Hand_Sides (Data, Tree, Right_Hand_Sides, Labels, Tokens (1));

         if not Data.Ignore_Lines then
            Right_Hand_Sides.Append (Get_RHS (Data, Tree, Labels, Tokens (3)));
         end if;

      when 2 | 3 =>
         --  | rhs_list PERCENT (IF | ELSIF) IDENTIFIER EQUAL IDENTIFIER
         Get_Right_Hand_Sides (Data, Tree, Right_Hand_Sides, Labels, Tokens (1));
         Start_If_1 (Data, Tree, Tokens (4), Tokens (6));

      when 4 =>
         --  | rhs_list PERCENT END IF
         Get_Right_Hand_Sides (Data, Tree, Right_Hand_Sides, Labels, Tokens (1));
         Data.Ignore_Lines := False;

      when others =>
         Raise_Programmer_Error ("Get_Right_Hand_Sides", Data, Tree, Token);
      end case;
   end Get_Right_Hand_Sides;

   ----------
   --  Public subprograms, declaration order

   overriding
   procedure Set_Lexer_Terminals
     (User_Data : in out User_Data_Type;
      Lexer     : in     WisiToken.Lexer.Handle;
      Terminals : in     Base_Token_Array_Access_Constant)
   is begin
      User_Data.Grammar_Lexer := Lexer;
      User_Data.Terminals     := Terminals;
   end Set_Lexer_Terminals;

   overriding procedure Reset (Data : in out User_Data_Type)
   is begin
      --  Preserve data set in Phase Meta, or by Set_Lexer_Terminals, or by
      --  wisitoken-bnf-generate.

      --  Preserve Grammar_Lexer
      --  Preserve User_Lexer
      --  Preserve User_Parser
      --  Perserve Generate_Set
      --  Preserve Meta_Syntax
      --  Preserve Phase
      --  Preserve Terminals
      --  Preserve Non_Grammar
      --  EBNF_Nodes handled in Initialize_Actions
      Data.Raw_Code          := (others => <>);
      Data.Language_Params   :=
        (Case_Insensitive => Data.Language_Params.Case_Insensitive,
         others => <>);
      Data.Tokens            :=
        (Virtual_Identifiers => Data.Tokens.Virtual_Identifiers,
         others => <>);
      Data.Conflicts.Clear;
      Data.McKenzie_Recover  := (others => <>);
      Data.Rule_Count        := 0;
      Data.Action_Count      := 0;
      Data.Check_Count       := 0;
      Data.Label_Count       := 0;
      Data.If_Lexer_Present  := False;
      Data.If_Parser_Present := False;
      Data.Ignore_Lines      := False;
   end Reset;

   overriding procedure Initialize_Actions
     (Data : in out User_Data_Type;
      Tree : in     WisiToken.Syntax_Trees.Tree'Class)
   is begin
      Data.EBNF_Nodes.Clear;
      Data.EBNF_Nodes.Set_First_Last (Tree.First_Index, Tree.Last_Index);
   end Initialize_Actions;

   overriding
   procedure Lexer_To_Augmented
     (Data  : in out          User_Data_Type;
      Tree  : in out          WisiToken.Syntax_Trees.Tree'Class;
      Token : in              WisiToken.Base_Token;
      Lexer : not null access WisiToken.Lexer.Instance'Class)
   is
      pragma Unreferenced (Lexer);
      use all type Ada.Containers.Count_Type;
   begin
      if Token.ID < Wisitoken_Grammar_Actions.Descriptor.First_Terminal then
         --  Non-grammar token
         if Data.Terminals.Length = 0 then
            Data.Leading_Non_Grammar.Append (Token);
         else
            declare
               Containing_Aug : Augmented_Token_Access := Augmented_Token_Access
                 (Tree.Augmented (Data.Last_Terminal_Node));
            begin
               if Containing_Aug = null then
                  Containing_Aug := new Augmented_Token'
                    (Data.Terminals.all (Tree.First_Shared_Terminal (Data.Last_Terminal_Node)) with Non_Grammar => <>);
                  Tree.Set_Augmented (Data.Last_Terminal_Node, WisiToken.Base_Token_Class_Access (Containing_Aug));
               end if;

               Containing_Aug.Non_Grammar.Append (Token);
            end;
         end if;
      else
         Data.Last_Terminal_Node := Token.Tree_Index;
      end if;
   end Lexer_To_Augmented;

   procedure Start_If
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in     WisiToken.Syntax_Trees.Tree;
      Tokens    : in     WisiToken.Valid_Node_Index_Array)
   is begin
      --  all phases
      Start_If_1 (User_Data_Type (User_Data), Tree, Tokens (3), Tokens (5));
   end Start_If;

   procedure End_If (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class)
   is
      Data : User_Data_Type renames User_Data_Type (User_Data);
   begin
      --  all phases
      Data.Ignore_Lines := False;
   end End_If;

   procedure Add_Declaration
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in     WisiToken.Syntax_Trees.Tree;
      Tokens    : in     WisiToken.Valid_Node_Index_Array)
   is
      use all type WisiToken.Syntax_Trees.Node_Label;
      use all type Ada.Strings.Unbounded.Unbounded_String;

      Data : User_Data_Type renames User_Data_Type (User_Data);

      function Token (Index : in SAL.Peek_Type) return Base_Token
      is
         use all type SAL.Base_Peek_Type;
      begin
         if Tokens'Last < Index then
            raise SAL.Programmer_Error;
         elsif Tree.Label (Tokens (Index)) /= WisiToken.Syntax_Trees.Shared_Terminal then
            raise SAL.Programmer_Error with "token at " & Image (Tree.Byte_Region (Tokens (Index))) &
              " is a " & WisiToken.Syntax_Trees.Node_Label'Image (Tree.Label (Tokens (Index))) &
              ", expecting Shared_Terminal";
         else
            return Data.Terminals.all (Tree.Terminal (Tokens (Index)));
         end if;
      end Token;

      function Enum_ID (Index : in SAL.Peek_Type) return Token_Enum_ID
      is (To_Token_Enum (Token (Index).ID));

   begin
      if Data.Phase = Meta then
         if Tree.Label (Tokens (2)) = WisiToken.Syntax_Trees.Shared_Terminal then
            case Enum_ID (2) is
            when IDENTIFIER_ID =>
               declare
                  Kind : constant String := Data.Grammar_Lexer.Buffer_Text (Token (2).Byte_Region);
               begin
                  if Kind = "case_insensitive" then
                     Data.Language_Params.Case_Insensitive := True;

                  elsif Kind = "generate" then
                     declare
                        use all type SAL.Base_Peek_Type;
                        Children : constant Valid_Node_Index_Array := Tree.Get_Terminals (Tokens (3));
                        Tuple    : WisiToken.BNF.Generate_Tuple;
                     begin
                        Tuple.Gen_Alg  := WisiToken.BNF.To_Generate_Algorithm (Get_Text (Data, Tree, Children (1)));
                        if Children'Last >= 2 then
                           Tuple.Out_Lang := WisiToken.BNF.To_Output_Language (Get_Text (Data, Tree, Children (2)));
                        end if;
                        for I in 3 .. SAL.Base_Peek_Type (Children'Length) loop
                           declare
                              Text : constant String := Get_Text (Data, Tree, Children (I));
                           begin
                              if Text = "text_rep" then
                                 Tuple.Text_Rep := True;

                              elsif (for some I of WisiToken.BNF.Lexer_Image => Text = I.all) then
                                 Tuple.Lexer := WisiToken.BNF.To_Lexer (Text);

                              elsif WisiToken.BNF.Is_Valid_Interface (Text) then
                                 Tuple.Interface_Kind := WisiToken.BNF.Valid_Interface'Value (Text);
                              else
                                 declare
                                    Token : Base_Token renames Data.Terminals.all (Tree.Terminal (Children (I)));
                                 begin
                                    raise Grammar_Error with Error_Message
                                      (Data.Grammar_Lexer.File_Name, Token.Line, Token.Column,
                                       "invalid generate param '" & Text & "'");
                                 end;
                              end if;
                           end;
                        end loop;
                        WisiToken.BNF.Add (Data.Generate_Set, Tuple);
                     end;

                  elsif Kind = "meta_syntax" then
                     if Data.Meta_Syntax = Unknown then
                        --  Don't overwrite; somebody set it for a reason.
                        declare
                           Value_Str : constant String := WisiToken.BNF.To_Lower (Get_Text (Data, Tree, Tokens (3)));
                        begin
                           if Value_Str = "bnf" then
                              Data.Meta_Syntax := BNF_Syntax;
                           elsif Value_Str = "ebnf" then
                              Data.Meta_Syntax := EBNF_Syntax;
                              Data.EBNF_Nodes (Tree.Find_Ancestor (Tokens (2), +declaration_ID)) := True;

                           else
                              Put_Error ("invalid value for %meta_syntax; must be BNF | EBNF.");
                           end if;
                        end;
                     end if;
                  end if;
               end;
            when others =>
               null;
            end case;
         end if;
         return;
      end if;

      --  Add declaration to User_Data.Generate_Set, Language_Params,
      --  Tokens, Conflicts, or McKenzie_Recover.

      if Data.Ignore_Lines then
         return;
      end if;

      case Tree.Label (Tokens (2)) is
      when Syntax_Trees.Nonterm =>
         --  must be token_keyword_non_grammar
         declare
            Children_2 : constant Valid_Node_Index_Array := Tree.Children (Tokens (2));
            Child_1_ID : constant Token_Enum_ID := To_Token_Enum (Tree.ID (Children_2 (1)));
         begin
            case Child_1_ID is
            when Wisitoken_Grammar_Actions.TOKEN_ID =>
               declare
                  Children_4 : constant Valid_Node_Index_Array := Tree.Children (Tokens (4));
               begin
                  WisiToken.BNF.Add_Token
                    (Data.Tokens.Tokens,
                     Kind         => Get_Text (Data, Tree, Children_2 (3)),
                     Name         => Get_Text (Data, Tree, Tokens (3)),
                     Value        => Get_Text (Data, Tree, Children_4 (1)),
                     Repair_Image => (if Children_4'Length = 1 then "" else Get_Text (Data, Tree, Children_4 (2))));
               end;

            when KEYWORD_ID =>

               Data.Tokens.Keywords.Append
                 ((Name  => +Get_Text (Data, Tree, Tokens (3)),
                   Value => +Get_Text (Data, Tree, Tokens (4))));

            when NON_GRAMMAR_ID =>

               WisiToken.BNF.Add_Token
                 (Data.Tokens.Non_Grammar,
                  Kind  => Get_Text (Data, Tree, Children_2 (3)),
                  Name  => Get_Text (Data, Tree, Tokens (3)),
                  Value => Get_Text (Data, Tree, Tokens (4)));

            when others =>
               raise SAL.Programmer_Error;
            end case;
         end;

      when Syntax_Trees.Shared_Terminal =>
         case Enum_ID (2) is
         when CODE_ID =>
            declare
               Location : WisiToken.BNF.Raw_Code_Location;

               --  % code identifier_list raw_code
               --  1 2    3               4
               --
               --  identifier_list = "action spec context"
               --  identifier_list children = identifier_list IDENTIFIER_ID
               --  children = identifier_list IDENTIFIER_ID
               --  children = IDENTIFIER_ID
               function Get_Loc_List return Base_Token_Array
               with Pre => Tree.ID (Tokens (3)) = +identifier_list_ID
               is
                  use all type SAL.Base_Peek_Type;
                  use WisiToken.Syntax_Trees;
                  Node   : Valid_Node_Index := Tokens (3);
                  Result : Base_Token_Array (1 .. 3);
                  First  : SAL.Peek_Type    := Result'Last + 1;
               begin
                  loop
                     pragma Assert (Tree.ID (Node) = +identifier_list_ID);
                     exit when not Tree.Has_Children (Node);
                     declare
                        Children : constant Valid_Node_Index_Array := Tree.Children (Node);
                     begin
                        if Children'Length = 1 then
                           --  identifier_list : IDENTIFIER
                           First := First - 1;
                           Result (First) := Data.Terminals.all (Tree.Terminal (Children (1)));
                           exit;

                        elsif Children'Length = 2 then
                           --  identifier_list : identifier_list IDENTIFIER
                           First := First - 1;
                           Result (First) := Data.Terminals.all (Tree.Terminal (Children (2)));

                           Node := Children (1);
                        else
                           raise SAL.Programmer_Error;
                        end if;
                     end;
                  end loop;
                  return Result (First .. Result'Last);
               end Get_Loc_List;

               Loc_List : constant Base_Token_Array := Get_Loc_List;

               function Get_Loc (Index : in SAL.Peek_Type) return String
               is (Data.Grammar_Lexer.Buffer_Text (Loc_List (Index).Byte_Region));

            begin
               if Get_Loc (Loc_List'First) = "actions" then
                  Location :=
                    (if Get_Loc (2) = "spec" then
                       (if Get_Loc (3) = "context" then WisiToken.BNF.Actions_Spec_Context
                        elsif Get_Loc (3) = "pre" then WisiToken.BNF.Actions_Spec_Pre
                        elsif Get_Loc (3) = "post" then WisiToken.BNF.Actions_Spec_Post
                        else raise Grammar_Error with
                          Error_Message
                            (Data.Grammar_Lexer.File_Name, Loc_List (2).Line,
                             "expecting {context | pre | post}"))

                     elsif Get_Loc (2) = "body" then
                       (if Get_Loc (3) = "context" then WisiToken.BNF.Actions_Body_Context
                        elsif Get_Loc (3) = "pre" then WisiToken.BNF.Actions_Body_Pre
                        elsif Get_Loc (3) = "post" then WisiToken.BNF.Actions_Body_Post
                        else raise Grammar_Error with
                          Error_Message
                            (Data.Grammar_Lexer.File_Name, Loc_List (2).Line,
                             "expecting {context | pre | post}"))

                     else raise Grammar_Error);

               elsif Get_Loc (Loc_List'First) = "copyright_license" then
                  Location := WisiToken.BNF.Copyright_License;

               else
                  raise Grammar_Error with
                    Error_Message
                      (Data.Grammar_Lexer.File_Name, Loc_List (Loc_List'First).Line,
                       "expecting {actions | copyright_license}");
               end if;

               Data.Raw_Code (Location) := WisiToken.BNF.Split_Lines (Get_Text (Data, Tree, Tokens (4)));
            exception
            when Grammar_Error =>
               Put_Error
                 (Error_Message
                    (Data.Grammar_Lexer.File_Name, Token (2).Line, Token (2).Column,
                     "invalid raw code location; actions {spec | body} {context | pre | post}"));
            end;

         when IDENTIFIER_ID =>
            declare
               Kind : constant String := Data.Grammar_Lexer.Buffer_Text (Token (2).Byte_Region);
            begin
               --  Alphabetical by Kind

               if Kind = "case_insensitive" then
                  --  Not in phase Other
                  null;

               elsif Kind = "conflict" then
                  declare
                     Tree_Indices : constant Valid_Node_Index_Array := Tree.Get_Terminals
                       (Tokens (3));

                     --  Old format:
                     --   %conflict <action_a>/<action_b> in state <LHS_A>, <LHS_B> on token <on>
                     --              1        2 3         4  5      6     7  8      9  10     11

                     --  New format:
                     --  %conflict action LHS.rhs [| action LHS.rsh]* 'on token' on
                     --            1      2

                     Conflict : BNF.Conflict;
                  begin
                     Conflict.Source_Line := Data.Terminals.all (Tree.Terminal (Tree_Indices (1))).Line;

                     if Tree.ID (Tree_Indices (2)) = +SLASH_ID then
                        --  old format
                        Conflict.Items.Append
                          ((Name  => +Get_Text (Data, Tree, Tree_Indices (1)),
                            Value => +Get_Text (Data, Tree, Tree_Indices (6))));

                        Conflict.Items.Append
                          ((Name  => +Get_Text (Data, Tree, Tree_Indices (3)),
                            Value => +Get_Text (Data, Tree, Tree_Indices (8))));

                        Conflict.On := +Get_Text (Data, Tree, Tree_Indices (11));
                     else
                        --  new format
                        declare
                           use all type SAL.Base_Peek_Type;
                           I : SAL.Peek_Type := 1;
                        begin
                           loop
                              Conflict.Items.Append
                                ((Name  => +Get_Text (Data, Tree, Tree_Indices (I)),
                                  Value => +Get_Text (Data, Tree, Tree_Indices (I + 1))));

                              I := I + 2;
                              exit when Tree.ID (Tree_Indices (I)) /= +BAR_ID;
                              I := I + 1;
                           end loop;
                           Conflict.On := +Get_Text (Data, Tree, Tree_Indices (I + 2));
                        end;
                     end if;
                     Data.Conflicts.Append (Conflict);
                  end;

               elsif Kind = "end" then
                  --  matching '%if' specified current lexer.
                  null;

               elsif Kind = "elisp_face" then
                  Data.Tokens.Faces.Append (Get_Text (Data, Tree, Tokens (3), Strip_Quotes => True));

               elsif Kind = "elisp_indent" then
                  Data.Tokens.Indents.Append
                    ((Name  => +Get_Child_Text (Data, Tree, Tokens (3), 1, Strip_Quotes => True),
                      Value => +Get_Child_Text (Data, Tree, Tokens (3), 2)));

               elsif Kind = "elisp_action" then
                  Data.Tokens.Actions.Insert
                    (Key             => +Get_Child_Text (Data, Tree, Tokens (3), 2),
                     New_Item        =>
                       (Action_Label => +Get_Child_Text (Data, Tree, Tokens (3), 1),
                        Ada_Name     => +Get_Child_Text (Data, Tree, Tokens (3), 3)));

               elsif Kind = "end_names_optional_option" then
                  Data.Language_Params.End_Names_Optional_Option := +Get_Text (Data, Tree, Tokens (3));

               elsif Kind = "generate" then
                  --  Not in Other phase
                  null;

               elsif Kind = "language_runtime" then
                  Data.Language_Params.Language_Runtime_Name :=
                    +Get_Text (Data, Tree, Tokens (3), Strip_Quotes => True);

               elsif Kind = "mckenzie_check_limit" then
                  Data.Language_Params.Error_Recover := True;
                  Data.McKenzie_Recover.Check_Limit := Token_Index'Value (Get_Text (Data, Tree, Tokens (3)));

               elsif Kind = "mckenzie_check_delta_limit" then
                  Data.Language_Params.Error_Recover := True;
                  Data.McKenzie_Recover.Check_Delta_Limit := Integer'Value (Get_Text (Data, Tree, Tokens (3)));

               elsif Kind = "mckenzie_cost_default" then
                  if Tree.Get_Terminals (Tokens (3))'Length /= 4 then
                     raise Grammar_Error with
                       Error_Message
                         (Data.Grammar_Lexer.File_Name,
                          Data.Terminals.all (Tree.First_Shared_Terminal (Tokens (3))).Line,
                          "too " & (if Tree.Get_Terminals (Tokens (3))'Length > 4 then "many" else "few") &
                            " default costs; should be 'insert, delete, push back, ignore check fail'.");
                  end if;

                  Data.Language_Params.Error_Recover := True;
                  Data.McKenzie_Recover.Source_Line := Data.Terminals.all
                    (Tree.First_Shared_Terminal (Tokens (1))).Line;

                  Data.McKenzie_Recover.Default_Insert          := Natural'Value
                    (Get_Child_Text (Data, Tree, Tokens (3), 1));
                  Data.McKenzie_Recover.Default_Delete_Terminal := Natural'Value
                    (Get_Child_Text (Data, Tree, Tokens (3), 2));
                  Data.McKenzie_Recover.Default_Push_Back       := Natural'Value
                    (Get_Child_Text (Data, Tree, Tokens (3), 3));
                  Data.McKenzie_Recover.Ignore_Check_Fail       := Natural'Value
                    (Get_Child_Text (Data, Tree, Tokens (3), 4));

               elsif Kind = "mckenzie_cost_delete" then
                  Data.Language_Params.Error_Recover := True;
                  Data.McKenzie_Recover.Delete.Append
                    ((+Get_Child_Text (Data, Tree, Tokens (3), 1),
                      +Get_Child_Text (Data, Tree, Tokens (3), 2)));

               elsif Kind = "mckenzie_cost_fast_forward" then
                  Data.Language_Params.Error_Recover := True;
                  Data.McKenzie_Recover.Fast_Forward :=
                    Integer'Value (Get_Text (Data, Tree, Tokens (3)));

               elsif Kind = "mckenzie_cost_insert" then
                  Data.Language_Params.Error_Recover := True;
                  Data.McKenzie_Recover.Insert.Append
                    ((+Get_Child_Text (Data, Tree, Tokens (3), 1),
                      +Get_Child_Text (Data, Tree, Tokens (3), 2)));

               elsif Kind = "mckenzie_cost_matching_begin" then
                  Data.Language_Params.Error_Recover := True;
                  Data.McKenzie_Recover.Matching_Begin :=
                    Integer'Value (Get_Text (Data, Tree, Tokens (3)));

               elsif Kind = "mckenzie_cost_push_back" then
                  Data.Language_Params.Error_Recover := True;
                  Data.McKenzie_Recover.Push_Back.Append
                    ((+Get_Child_Text (Data, Tree, Tokens (3), 1),
                      +Get_Child_Text (Data, Tree, Tokens (3), 2)));

               elsif Kind = "mckenzie_cost_undo_reduce" then
                  Data.Language_Params.Error_Recover := True;
                  Data.McKenzie_Recover.Undo_Reduce.Append
                    ((+Get_Child_Text (Data, Tree, Tokens (3), 1),
                      +Get_Child_Text (Data, Tree, Tokens (3), 2)));

               elsif Kind = "mckenzie_enqueue_limit" then
                  Data.Language_Params.Error_Recover := True;
                  Data.McKenzie_Recover.Enqueue_Limit := Natural'Value (Get_Text (Data, Tree, Tokens (3)));

               elsif Kind = "mckenzie_minimal_complete_cost_delta" then
                  Data.Language_Params.Error_Recover := True;
                  Data.McKenzie_Recover.Minimal_Complete_Cost_Delta :=
                    Integer'Value (Get_Text (Data, Tree, Tokens (3)));

               elsif Kind = "meta_syntax" then
                  --  not in Other phase
                  null;

               elsif Kind = "no_enum" then
                  Data.Language_Params.Declare_Enums := False;

               elsif Kind = "no_language_runtime" then
                  Data.Language_Params.Use_Language_Runtime := False;

               elsif Kind = "partial_recursion" then
                  Data.Language_Params.Partial_Recursion := True;

               elsif Kind = "start" then
                  Data.Language_Params.Start_Token := +Get_Text (Data, Tree, Tokens (3));

               elsif Kind = "lexer_regexp" then
                  Data.Tokens.Lexer_Regexps.Append
                    ((+Get_Child_Text (Data, Tree, Tokens (3), 1),
                      +Get_Child_Text (Data, Tree, Tokens (3), 2)));

               else
                  raise Grammar_Error with Error_Message
                    (Data.Grammar_Lexer.File_Name, Token (2).Line, Token (2).Column, "unexpected syntax");

               end if;
            end;

         when others =>
            raise Grammar_Error with Error_Message
              (Data.Grammar_Lexer.File_Name, Token (2).Line, Token (2).Column, "unexpected syntax");
         end case;

      when Syntax_Trees.Virtual_Terminal | Syntax_Trees.Virtual_Identifier =>
         raise SAL.Programmer_Error;
      end case;
   end Add_Declaration;

   procedure Add_Nonterminal
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in     WisiToken.Syntax_Trees.Tree;
      Tokens    : in     WisiToken.Valid_Node_Index_Array)
   is
      use all type Ada.Containers.Count_Type;
      use WisiToken.Syntax_Trees;

      Data : User_Data_Type renames User_Data_Type (User_Data);

      LHS_Node   : constant Valid_Node_Index := Tokens (1);
      LHS_String : constant String           := Get_Text (Data, Tree, LHS_Node);

      Right_Hand_Sides : WisiToken.BNF.RHS_Lists.List;
      Labels           : WisiToken.BNF.String_Arrays.Vector;
   begin
      if Data.Phase = Meta or Data.Ignore_Lines then
         return;
      end if;

      Data.Rule_Count := Data.Rule_Count + 1;

      Get_Right_Hand_Sides (Data, Tree, Right_Hand_Sides, Labels, Tokens (3));

      if WisiToken.BNF.Is_Present (Data.Tokens.Rules, LHS_String) then
         case Tree.Label (LHS_Node) is
         when Shared_Terminal =>
            declare
               LHS_Token : Base_Token renames Data.Terminals.all (Tree.Terminal (LHS_Node));
            begin
               raise Grammar_Error with Error_Message
                 (Data.Grammar_Lexer.File_Name, LHS_Token.Line, LHS_Token.Column, "duplicate nonterm");
            end;

         when Virtual_Identifier =>
            raise Grammar_Error with Error_Message
              (Data.Grammar_Lexer.File_Name, 1, 1, "duplicate virtual nonterm '" & LHS_String & "'");

         when others =>
            Raise_Programmer_Error ("Add_Nonterminal", Data, Tree, LHS_Node);
         end case;
      else
         Data.Label_Count := Data.Label_Count + Labels.Length;

         Data.Tokens.Rules.Append
           ((+LHS_String, Right_Hand_Sides, Labels,
             Source_Line =>
               (case Tree.Label (LHS_Node) is
                when Shared_Terminal    => Data.Terminals.all (Tree.First_Shared_Terminal (LHS_Node)).Line,
                when Virtual_Identifier => Invalid_Line_Number, -- IMPROVEME: get line from Right_Hand_Sides
                when others             => raise SAL.Programmer_Error)));
      end if;
   end Add_Nonterminal;

   function Image_Grammar_Action (Action : in WisiToken.Syntax_Trees.Semantic_Action) return String
   is
      pragma Unreferenced (Action);
   begin
      return "action";
   end Image_Grammar_Action;

   procedure Check_EBNF
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in     WisiToken.Syntax_Trees.Tree;
      Tokens    : in     WisiToken.Valid_Node_Index_Array;
      Token     : in     WisiToken.Positive_Index_Type)
   is
      Data : User_Data_Type renames User_Data_Type (User_Data);
   begin
      if Data.EBNF_Ok then
         return;
      end if;

      case Data.Phase is
      when Meta =>
         Data.EBNF_Nodes (Tokens (Token)) := True;

         if Data.Meta_Syntax /= EBNF_Syntax then
            declare
               Tok  : Base_Token renames Data.Terminals.all (Tree.First_Shared_Terminal (Tokens (Token)));
            begin
               raise Grammar_Error with Error_Message
                 (Data.Grammar_Lexer.File_Name, Tok.Line, Tok.Column,
                  "EBNF syntax used, but BNF specified; set '%meta_syntax EBNF'");
            end;
         end if;
      when Other =>
         Raise_Programmer_Error ("untranslated EBNF node", Data, Tree, Tree.Parent (Tokens (Token)));
      end case;
   end Check_EBNF;

   procedure Raise_Programmer_Error
     (Label : in String;
      Data  : in User_Data_Type;
      Tree  : in WisiToken.Syntax_Trees.Tree;
      Node  : in WisiToken.Node_Index)
   is begin
      WisiToken.Syntax_Trees.LR_Utils.Raise_Programmer_Error
        (Label, Wisitoken_Grammar_Actions.Descriptor, Data.Grammar_Lexer, Tree, Data.Terminals.all, Node);
   end Raise_Programmer_Error;

end WisiToken_Grammar_Runtime;
