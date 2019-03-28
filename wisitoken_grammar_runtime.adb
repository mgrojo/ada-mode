--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018 - 2019 Free Software Foundation, Inc.
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
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with SAL.Generic_Decimal_Image;
with WisiToken.Generate;   use WisiToken.Generate;
with Wisitoken_Grammar_Actions; use Wisitoken_Grammar_Actions;
package body WisiToken_Grammar_Runtime is

   use WisiToken;

   ----------
   --  Body subprograms, misc order

   procedure Raise_Programmer_Error
     (Label : in String;
      Tree  : in WisiToken.Syntax_Trees.Tree;
      Node  : in WisiToken.Syntax_Trees.Node_Index);
   pragma No_Return (Raise_Programmer_Error);

   procedure Raise_Programmer_Error
     (Label : in String;
      Tree  : in WisiToken.Syntax_Trees.Tree;
      Node  : in WisiToken.Syntax_Trees.Node_Index)
   is begin
      raise SAL.Programmer_Error with Label & WisiToken.Syntax_Trees.Node_Index'Image (Node) &
        ":" & Tree.Image (Node, Wisitoken_Grammar_Actions.Descriptor, Include_Children => True);
   end Raise_Programmer_Error;

   function Get_Text
     (Data         : in User_Data_Type;
      Tree         : in Syntax_Trees.Tree;
      Tree_Index   : in Syntax_Trees.Valid_Node_Index;
      Strip_Quotes : in Boolean := False)
     return String
   is
      use all type Syntax_Trees.Node_Label;

      function Strip_Delimiters (Tree_Index : in Syntax_Trees.Valid_Node_Index) return String
      is
         Region : Buffer_Region renames Data.Terminals.all (Tree.Terminal (Tree_Index)).Byte_Region;
      begin
         if -Tree.ID (Tree_Index) in RAW_CODE_ID | REGEXP_ID | ACTION_ID then
            --  strip delimiters.
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
         return -Data.Tokens.Virtual_Nonterminals (Tree.Identifier (Tree_Index));

      when Nonterm =>
         declare
            use all type Ada.Strings.Unbounded.Unbounded_String;
            Result       : Ada.Strings.Unbounded.Unbounded_String;
            Tree_Indices : constant Syntax_Trees.Valid_Node_Index_Array := Tree.Get_Terminals (Tree_Index);
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

   function Get_Child_Text
     (Data         : in User_Data_Type;
      Tree         : in Syntax_Trees.Tree;
      Parent       : in Syntax_Trees.Valid_Node_Index;
      Child        : in SAL.Peek_Type;
      Strip_Quotes : in Boolean := False)
     return String
   is
      Tree_Indices : constant Syntax_Trees.Valid_Node_Index_Array := Tree.Get_Terminals (Parent);
   begin
      return Get_Text (Data, Tree, Tree_Indices (Child), Strip_Quotes);
   end Get_Child_Text;

   procedure Start_If_1
     (Data    : in out User_Data_Type;
      Tree    : in     Syntax_Trees.Tree;
      A_Index : in     Syntax_Trees.Valid_Node_Index;
      B_Index : in     Syntax_Trees.Valid_Node_Index)
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
             (Data.Grammar_Lexer.File_Name, Data.Terminals.all (Tree.Min_Terminal_Index (A_Index)).Line,
              "invalid '%if'; must be one of {lexer | parser}");
      end if;
   end Start_If_1;

   function Get_RHS
     (Data  : in out User_Data_Type;
      Tree  : in     Syntax_Trees.Tree;
      Token : in     Syntax_Trees.Valid_Node_Index)
     return WisiToken.BNF.RHS_Type
   is
      use all type SAL.Base_Peek_Type;
      Tokens : constant Syntax_Trees.Valid_Node_Index_Array := Tree.Children (Token);
   begin
      pragma Assert (-Tree.ID (Token) = rhs_ID);

      return RHS : WisiToken.BNF.RHS_Type do
         if Tree.Min_Terminal_Index (Token) = Invalid_Token_Index then
            --  Tokens is empty or all virtual_identifiers; parent is a possibly
            --  empty rhs_list; grandparent may be a non-empty rhs_list or
            --  nonterminal.
            declare
               Tok : constant Base_Token_Index := Tree.Min_Terminal_Index (Tree.Parent (Tree.Parent (Token)));
            begin
               if Tok = Invalid_Token_Index then
                  --  grandparent is all virtual
                  RHS.Source_Line := Invalid_Line_Number;
               else
                  RHS.Source_Line := Data.Terminals.all (Tok).Line;
               end if;
            end;
         else
            RHS.Source_Line := Data.Terminals.all (Tree.Min_Terminal_Index (Token)).Line;
         end if;

         if Tokens'Length > 0 then
            for I of Tree.Get_Terminals (Tokens (1)) loop
               RHS.Tokens.Append (Get_Text (Data, Tree, I));
            end loop;

            if Tokens'Last >= 2 then
               declare
                  Text : constant String := Get_Text (Data, Tree, Tokens (2));
               begin
                  if Text'Length > 0 and (for some C of Text => C /= ' ') then
                     RHS.Action := +Text;
                     Data.Action_Count := Data.Action_Count + 1;
                  end if;
               end;
            end if;

            if Tokens'Last >= 3 then
               RHS.Check := +Get_Text (Data, Tree, Tokens (3));
               Data.Check_Count := Data.Check_Count + 1;
            end if;
         end if;
      end return;
   end Get_RHS;

   procedure Get_Right_Hand_Sides
     (Data             : in out User_Data_Type;
      Tree             : in     WisiToken.Syntax_Trees.Tree;
      Right_Hand_Sides : in out WisiToken.BNF.RHS_Lists.List;
      Token            : in     WisiToken.Syntax_Trees.Valid_Node_Index)
   is
      Tokens : constant Syntax_Trees.Valid_Node_Index_Array := Tree.Children (Token);
   begin
      pragma Assert (-Tree.ID (Token) = rhs_list_ID);

      if Data.Ignore_Lines then
         return;
      end if;

      case Tokens'Length is
      when 1 =>
         --  | rhs
         Right_Hand_Sides.Append (Get_RHS (Data, Tree, Tokens (1)));

      when 3 =>
         --  | rhs_list BAR rhs
         Get_Right_Hand_Sides (Data, Tree, Right_Hand_Sides, Tokens (1));

         Right_Hand_Sides.Append (Get_RHS (Data, Tree, Tokens (3)));

      when others =>
         Raise_Programmer_Error ("Get_Right_Hand_Sides", Tree, Token);
      end case;
   end Get_Right_Hand_Sides;

   ----------
   --  Public subprograms, declaration order

   overriding
   procedure Set_Lexer_Terminals
     (User_Data : in out User_Data_Type;
      Lexer     : in     WisiToken.Lexer.Handle;
      Terminals : in     Base_Token_Array_Access)
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
      Data.Language_Params   := (others => <>);
      Data.Tokens            :=
        (Virtual_Nonterminals => Data.Tokens.Virtual_Nonterminals,
         others => <>);
      Data.Conflicts.Clear;
      Data.McKenzie_Recover  := (others => <>);
      Data.Rule_Count        := 0;
      Data.Action_Count      := 0;
      Data.Check_Count       := 0;
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
      Token : in              WisiToken.Base_Token;
      Lexer : not null access WisiToken.Lexer.Instance'Class)
   is
      pragma Unreferenced (Lexer);
      use all type Ada.Containers.Count_Type;
   begin
      if Token.ID < Wisitoken_Grammar_Actions.Descriptor.First_Terminal then
         --  Non-grammar token
         if Data.Non_Grammar.Length = 0 then
            Data.Non_Grammar.Set_First_Last (0, 0);
         end if;

         if Data.Terminals.Length = 0 then
            Data.Non_Grammar (0).Append (Token);
         else
            Data.Non_Grammar.Set_Last (Data.Terminals.Last_Index);
            Data.Non_Grammar (Data.Terminals.Last_Index).Append (Token);
         end if;
      end if;
   end Lexer_To_Augmented;

   procedure Start_If
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in     WisiToken.Syntax_Trees.Tree;
      Tokens    : in     WisiToken.Syntax_Trees.Valid_Node_Index_Array)
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
      Tokens    : in     WisiToken.Syntax_Trees.Valid_Node_Index_Array)
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
        is (Token_Enum_ID'(-Token (Index).ID));

   begin
      if Data.Phase = Meta then
         if Tree.Label (Tokens (2)) = WisiToken.Syntax_Trees.Shared_Terminal then
            case Enum_ID (2) is
            when IDENTIFIER_ID =>
               declare
                  Kind : constant String := Data.Grammar_Lexer.Buffer_Text (Token (2).Byte_Region);
               begin
                  if Kind = "generate" then
                     declare
                        Children : constant Syntax_Trees.Valid_Node_Index_Array := Tree.Get_Terminals (Tokens (3));
                        Tuple     : WisiToken.BNF.Generate_Tuple;
                     begin
                        Tuple.Gen_Alg  := WisiToken.BNF.Generate_Algorithm'Value (Get_Text (Data, Tree, Children (1)));
                        Tuple.Out_Lang := WisiToken.BNF.To_Output_Language (Get_Text (Data, Tree, Children (2)));
                        for I in 3 .. SAL.Base_Peek_Type (Children'Length) loop
                           declare
                              Text : constant String := Get_Text (Data, Tree, Children (I));
                           begin
                              if Text = "text_rep" then
                                 Tuple.Text_Rep := True;

                              elsif (for some I of WisiToken.BNF.Lexer_Image => Text = I.all) then
                                 Tuple.Lexer := WisiToken.BNF.To_Lexer (Text);

                              elsif (for some I in WisiToken.BNF.Valid_Interface =>
                                       WisiToken.BNF.To_Lower (Text) = WisiToken.BNF.To_Lower
                                         (WisiToken.BNF.Valid_Interface'Image (I)))
                              then
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
            Children : Syntax_Trees.Valid_Node_Index_Array renames Tree.Children (Tokens (2));
            Child_1  : Base_Token renames Data.Terminals.all (Tree.Terminal (Children (1)));
         begin
            case Token_Enum_ID'(-Child_1.ID) is
            when Wisitoken_Grammar_Actions.TOKEN_ID =>

               WisiToken.BNF.Add_Token
                 (Data.Tokens.Tokens,
                  Kind  => Get_Text (Data, Tree, Children (3)),
                  Name  => Get_Text (Data, Tree, Tokens (3)),
                  Value => Get_Text (Data, Tree, Tokens (4)));

            when KEYWORD_ID =>

               Data.Tokens.Keywords.Append
                 ((Name  => +Get_Text (Data, Tree, Tokens (3)),
                   Value => +Get_Text (Data, Tree, Tokens (4))));

            when NON_GRAMMAR_ID =>

               WisiToken.BNF.Add_Token
                 (Data.Tokens.Non_Grammar,
                  Kind  => Get_Text (Data, Tree, Children (3)),
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
               is
                  use all type SAL.Base_Peek_Type;
                  use WisiToken.Syntax_Trees;
                  Node   : Valid_Node_Index := Tokens (3);
                  Result : Base_Token_Array (1 .. 3);
                  First  : SAL.Peek_Type    := Result'Last + 1;
               begin
                  loop
                     pragma Assert (-Tree.ID (Node) = identifier_list_ID);
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
                  Data.Language_Params.Case_Insensitive := True;

               elsif Kind = "conflict" then
                  declare
                     Tree_Indices : constant Syntax_Trees.Valid_Node_Index_Array := Tree.Get_Terminals
                       (Tokens (3));
                     --   %conflict <action_a>/<action_b> in state <LHS_A>, <LHS_B> on token <on>
                     --              1        2 3         4  5      6     7  8      9  10     11
                  begin
                     Data.Conflicts.Append
                       ((Source_Line => Data.Terminals.all (Tree.Terminal (Tree_Indices (1))).Line,
                         Action_A    => +Get_Text (Data, Tree, Tree_Indices (1)),
                         LHS_A       => +Get_Text (Data, Tree, Tree_Indices (6)),
                         Action_B    => +Get_Text (Data, Tree, Tree_Indices (3)),
                         LHS_B       => +Get_Text (Data, Tree, Tree_Indices (8)),
                         On          => +Get_Text (Data, Tree, Tree_Indices (11))));
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

               elsif Kind = "embedded_quote_escape_doubled" then
                  Data.Language_Params.Embedded_Quote_Escape_Doubled := True;

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
                         (Data.Grammar_Lexer.File_Name, Data.Terminals.all (Tree.Min_Terminal_Index (Tokens (3))).Line,
                          "too " & (if Tree.Get_Terminals (Tokens (3))'Length > 4 then "many" else "few") &
                            " default costs; should be 'insert, delete, push back, ignore check fail'.");
                  end if;

                  Data.Language_Params.Error_Recover := True;
                  Data.McKenzie_Recover.Source_Line := Data.Terminals.all (Tree.Min_Terminal_Index (Tokens (1))).Line;

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

               elsif Kind = "mckenzie_cost_insert" then
                  Data.Language_Params.Error_Recover := True;
                  Data.McKenzie_Recover.Insert.Append
                    ((+Get_Child_Text (Data, Tree, Tokens (3), 1),
                      +Get_Child_Text (Data, Tree, Tokens (3), 2)));

               elsif Kind = "mckenzie_cost_limit" then
                  Data.Language_Params.Error_Recover := True;
                  Data.McKenzie_Recover.Cost_Limit := Natural'Value (Get_Text (Data, Tree, Tokens (3)));

               elsif Kind = "mckenzie_cost_push_back" then
                  Data.Language_Params.Error_Recover := True;
                  Data.McKenzie_Recover.Push_Back.Append
                    ((+Get_Child_Text (Data, Tree, Tokens (3), 1),
                      +Get_Child_Text (Data, Tree, Tokens (3), 2)));

               elsif Kind = "mckenzie_enqueue_limit" then
                  Data.Language_Params.Error_Recover := True;
                  Data.McKenzie_Recover.Enqueue_Limit := Natural'Value (Get_Text (Data, Tree, Tokens (3)));

               elsif Kind = "meta_syntax" then
                  --  not in Other phase
                  null;

               elsif Kind = "no_language_runtime" then
                  Data.Language_Params.Use_Language_Runtime := False;

               elsif Kind = "no_enum" then
                  Data.Language_Params.Declare_Enums := False;

               elsif Kind = "start" then
                  Data.Language_Params.Start_Token := +Get_Text (Data, Tree, Tokens (3));

               elsif Kind = "re2c_regexp" then
                  Data.Tokens.re2c_Regexps.Append
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
      Tokens    : in     WisiToken.Syntax_Trees.Valid_Node_Index_Array)
   is
      use WisiToken.Syntax_Trees;

      Data : User_Data_Type renames User_Data_Type (User_Data);

      LHS_Node   : constant Valid_Node_Index := Tokens (1);
      LHS_String : constant String           := Get_Text (Data, Tree, LHS_Node);

      Right_Hand_Sides : WisiToken.BNF.RHS_Lists.List;
   begin
      if Data.Phase = Meta then
         return;
      end if;

      Data.Rule_Count := Data.Rule_Count + 1;

      Get_Right_Hand_Sides (Data, Tree, Right_Hand_Sides, Tokens (3));

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
            Raise_Programmer_Error ("Add_Nonterminal", Tree, LHS_Node);
         end case;
      else
         Data.Tokens.Rules.Append
           ((+LHS_String, Right_Hand_Sides,
             Source_Line =>
               (case Tree.Label (LHS_Node) is
                when Shared_Terminal    => Data.Terminals.all (Tree.Min_Terminal_Index (LHS_Node)).Line,
                when Virtual_Identifier => Invalid_Line_Number, -- IMPROVEME: get line from Right_Hand_Sides
                when others             => raise SAL.Programmer_Error)));
      end if;
   end Add_Nonterminal;

   procedure Check_EBNF
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in     WisiToken.Syntax_Trees.Tree;
      Tokens    : in     WisiToken.Syntax_Trees.Valid_Node_Index_Array)
   is
      Data  : User_Data_Type renames User_Data_Type (User_Data);
      Token : Base_Token renames Data.Terminals.all (Tree.Min_Terminal_Index (Tokens (1)));
   begin
      if Data.Phase = Meta then
         Data.EBNF_Nodes (Tokens (1)) := True;

         if Data.Meta_Syntax /= EBNF_Syntax then
            raise Grammar_Error with Error_Message
              (Data.Grammar_Lexer.File_Name, Token.Line, Token.Column,
               "EBNF syntax used, but BNF specified; set '%meta_syntax EBNF'");
         end if;
      end if;
   end Check_EBNF;

   procedure Rewrite_EBNF_To_BNF
     (Tree : in out WisiToken.Syntax_Trees.Tree;
      Data : in out User_Data_Type)
   is
      use WisiToken.Syntax_Trees;

      Comp_Unit_List_Tail : Valid_Node_Index := Tree.Parent
        (Tree.Find_Descendant (Tree.Root, +compilation_unit_ID));

      function Parent (Node : in Valid_Node_Index; Count : in Positive) return Valid_Node_Index
      is
         Result : Node_Index := Node;
      begin
         for I in 1 .. Count loop
            Result := Tree.Parent (Result);
         end loop;
         if Result = Invalid_Node_Index then
            raise SAL.Programmer_Error with "invalid parent: node" & Node_Index'Image (Node) &
              " count" & Positive'Image (Count);
         else
            return Result;
         end if;
      end Parent;

      function Child (Node : in Valid_Node_Index; Index : in Positive_Index_Type) return Node_Index
      is
         Children : Valid_Node_Index_Array renames Tree.Children (Node);
      begin
         if Index in Children'Range then
            return Tree.Children (Node)(Index);
         else
            return Invalid_Node_Index;
         end if;
      end Child;

      function Next_Nonterm_Name (Suffix : in String := "") return Identifier_Index
      is
         function Image is new SAL.Generic_Decimal_Image (Identifier_Index);
         ID : constant Identifier_Index := Identifier_Index (Data.Tokens.Virtual_Nonterminals.Length) + 1;
      begin

         if ID > 999 then
            --  We assume 3 digits below
            raise SAL.Programmer_Error with "more than 3 digits needed for virtual identifiers in EBNF rewrite";
         end if;

         Data.Tokens.Virtual_Nonterminals.Append (+("nonterminal_" & Image (ID, Width => 3) & Suffix));

         return ID;
      end Next_Nonterm_Name;

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

      procedure Add_Compilation_Unit (Unit : in Valid_Node_Index)
      is
         --  Unit is a declaration_ID or nonterminal_ID

         Old_Comp_Unit_List_Tail : constant Valid_Node_Index := Comp_Unit_List_Tail;
      begin
         --  current tree:
         --  compilation_unit_list: Comp_Unit_List_Tail
         --  | compilation_unit: Comp_Unit_List_Tail.children (1)

         --  new tree:
         --  compilation_unit_list: old Comp_Unit_List_Tail
         --  | compilation_unit_list: new, new Comp_Unit_List_Tail
         --  | | compilation_unit: Unit
         --  | compilation_unit: Comp_Unit_List_Tail.children (1)

         Comp_Unit_List_Tail := Tree.Add_Nonterm
           ((+compilation_unit_list_ID, 0),
            (1 => Tree.Add_Nonterm ((+compilation_unit_ID, 1), (1 => Unit))));

         Tree.Set_Children
           (Old_Comp_Unit_List_Tail,
            (+compilation_unit_list_ID, 1),
            (1 => Comp_Unit_List_Tail,
             2 => Tree.Children (Old_Comp_Unit_List_Tail)(1)));
      end Add_Compilation_Unit;

      procedure Convert_RHS_Alternative (RHS_Alt_Node : in Valid_Node_Index)
      is
         --  Common part of New_Nonterminal*; convert subtree rooted at
         --  RHS_Alt_Node from an rhs_alternative_list to an rhs_list.
         RHS_Alt_Children : constant Valid_Node_Index_Array := Tree.Children (RHS_Alt_Node);
      begin
         --  wisitoken_grammary.wy rhs_alternative_list has 1 or 3 children

         if RHS_Alt_Children'Length = 3 then
            --  current tree:
            --  rhs_alternative_list : RHS_Alt_Node
            --  | rhs_alternative_list: RHS_Alt_Node.Children (1)
            --  | | rhs_item_list: RHS_Alt_Node.Children (1).Children (1)
            --  | terminal: BAR RHS_Alt_Node.Children (2)
            --  | rhs_item_list: RHS_Alt_Node.Children (3)

            --  new tree:
            --  rhs_list: RHS_Alt_Node
            --  | rhs_list: RHS_Alt_Children (1)
            --  | | rhs: new
            --  | | | rhs_item_list: RHS_Alt_Children (1).Children (1)
            --  | BAR: new
            --  | rhs: new
            --  | | rhs_item_list: RHS_Alt_Children (3)

            Tree.Set_Children
              (RHS_Alt_Children (1),
               (+rhs_list_ID, 1),
               (1 => Tree.Add_Nonterm
                  ((+rhs_ID, 1),
                   (1 => Tree.Children (RHS_Alt_Children (1))(1)))));

            Tree.Set_Children
              (RHS_Alt_Node,
               (+rhs_list_ID, 1),
               (1 => RHS_Alt_Children (1),
                2 => Tree.Add_Terminal (+BAR_ID),
                3 => Tree.Add_Nonterm
                  ((+rhs_ID, 1),
                   (1 => RHS_Alt_Children (3)))));

         else
            --  current tree:
            --  rhs_alternative_list : RHS_Alt_Node
            --  | rhs_item_list: RHS_Alt_Node.Children (1)

            --  new tree:
            --  rhs_list: RHS_Alt_Node
            --  | rhs: new
            --  | | rhs_item_list: RHS_Alt_Node.Children (1)

            Tree.Set_Children
              (RHS_Alt_Node,
               (+rhs_list_ID, 0),
               (1 => Tree.Add_Nonterm ((+rhs_ID, 1), (1 => RHS_Alt_Children (1)))));
         end if;

         Data.EBNF_Nodes (RHS_Alt_Node) := False;
      end Convert_RHS_Alternative;

      procedure New_Nonterminal
        (New_Identifier : in Identifier_Index;
         RHS_Alt_Node   : in Valid_Node_Index)
      is
         --  Convert subtree rooted at RHS_Alt_Node from an
         --  rhs_alternative_list to an rhs_list contained by a new nonterm
         --  named New_Identifier.
         New_Nonterm : constant Valid_Node_Index := Tree_Add_Nonterminal
           (Child_1   => Tree.Add_Identifier (+IDENTIFIER_ID, New_Identifier),
            Child_2   => Tree.Add_Terminal (+COLON_ID),
            Child_3   => RHS_Alt_Node,
            Child_4   => Tree.Add_Nonterm
              ((+semicolon_opt_ID, 0),
               (1     => Tree.Add_Terminal (+SEMICOLON_ID))));
      begin
         Add_Compilation_Unit (New_Nonterm);
         Convert_RHS_Alternative (RHS_Alt_Node);
      end New_Nonterminal;

      procedure New_Nonterminal_Opt
        (New_Identifier : in Identifier_Index;
         RHS_Alt_Node   : in Valid_Node_Index)
      is
         --  Convert subtree rooted at RHS_Alt_Node from an
         --  rhs_alternative_list to an rhs_list contained by a new nonterm
         --  named New_Identifier, with a sibling empty rhs.

         Bar_Node : constant Valid_Node_Index := Tree.Add_Terminal (+BAR_ID);
         RHS_Node : constant Valid_Node_Index := Tree.Add_Nonterm ((+rhs_ID, 0), (1 .. 0 => Invalid_Node_Index));

         New_Nonterm : constant Valid_Node_Index := Tree_Add_Nonterminal
           (Child_1   => Tree.Add_Identifier (+IDENTIFIER_ID, New_Identifier),
            Child_2   => Tree.Add_Terminal (+COLON_ID),
            Child_3   => Tree.Add_Nonterm
              ((+rhs_list_ID, 1),
               (1     => RHS_Alt_Node,
                2     => Bar_Node,
                3     => RHS_Node)),
            Child_4   => Tree.Add_Nonterm
              ((+semicolon_opt_ID, 0),
               (1     => Tree.Add_Terminal (+SEMICOLON_ID))));
      begin
         Add_Compilation_Unit (New_Nonterm);
         Convert_RHS_Alternative (RHS_Alt_Node);
      end New_Nonterminal_Opt;

      procedure New_Nonterminal_Opt
        (Nonterm : in Identifier_Index;
         Content : in Identifier_Index)
      is
         --  Add new nonterminal:
         --
         --    Nonterm_opt
         --    : ;; empty
         --    | Content
         --
         --  nonterminal: nonterm_opt
         --  | IDENTIFIER: Nonterm
         --  | COLON:
         --  | rhs_list:
         --  | | rhs_list: RHS_List_2
         --  | | | rhs: RHS_1
         --  | | BAR:
         --  | | rhs: RHS_2
         --  | | | rhs_item_list: RHS_Item_List_1
         --  | | | | rhs_item: RHS_Item_1
         --  | | | | | IDENTIFIER: Content
         --  | semicolon_opt:
         --  | | SEMICOLON:

         RHS_Item_1 : constant Valid_Node_Index := Tree.Add_Nonterm
           ((+rhs_item_ID, 2), (1 => Tree.Add_Identifier (+IDENTIFIER_ID, Content)));

         RHS_Item_List_1 : constant Valid_Node_Index := Tree.Add_Nonterm ((+rhs_item_list_ID, 0), (1 => RHS_Item_1));

         RHS_1 : constant Valid_Node_Index := Tree.Add_Nonterm ((+rhs_ID, 0), (1 .. 0 => Invalid_Node_Index));
         RHS_2 : constant Valid_Node_Index := Tree.Add_Nonterm ((+rhs_ID, 1), (1 => RHS_Item_List_1));

         RHS_List_2 : constant Valid_Node_Index := Tree.Add_Nonterm ((+rhs_list_ID, 1), (1 => RHS_1));

         Bar_Node : constant Valid_Node_Index := Tree.Add_Terminal (+BAR_ID);

         New_Nonterm : constant Valid_Node_Index := Tree_Add_Nonterminal
           (Child_1   => Tree.Add_Identifier (+IDENTIFIER_ID, Nonterm),
            Child_2   => Tree.Add_Terminal (+COLON_ID),
            Child_3   => Tree.Add_Nonterm
              ((+rhs_list_ID, 1),
               (1     => RHS_List_2,
                2     => Bar_Node,
                3     => RHS_2)),
            Child_4   => Tree.Add_Nonterm
              ((+semicolon_opt_ID, 0),
               (1     => Tree.Add_Terminal (+SEMICOLON_ID))));
      begin
         Add_Compilation_Unit (New_Nonterm);
      end New_Nonterminal_Opt;

      procedure New_Nonterminal_List
        (List_Nonterm : in Identifier_Index;
         List_Element : in Identifier_Index)
      is
         --  nonterminal: foo_list
         --  | IDENTIFIER: "foo_list" List_Nonterm
         --  | COLON:
         --  | rhs_list:
         --  | | rhs_list: RHS_List_2
         --  | | | rhs: RHS_2
         --  | | | | rhs_item_list: RHS_Item_List_1
         --  | | | | | rhs_item: RHS_Item_1
         --  | | | | | | IDENTIFIER: List_Element
         --  | | BAR:
         --  | | rhs: RHS_3
         --  | | | rhs_item_list: RHS_Item_List_2
         --  | | | | | rhs_item_list: RHS_Item_List_3
         --  | | | | | | rhs_item: RHS_Item_2
         --  | | | | | | | IDENTIFIER: List_Nonterm
         --  | | | | rhs_item: RHS_Item_3
         --  | | | | | IDENTIFIER: List_Element
         --  | semicolon_opt:
         --  | | SEMICOLON:

         RHS_Item_1 : constant Valid_Node_Index := Tree.Add_Nonterm
           ((+rhs_item_ID, 2), (1 => Tree.Add_Identifier (+IDENTIFIER_ID, List_Element)));

         RHS_Item_2 : constant Valid_Node_Index := Tree.Add_Nonterm
           ((+rhs_item_ID, 2), (1 => Tree.Add_Identifier (+IDENTIFIER_ID, List_Nonterm)));

         RHS_Item_3 : constant Valid_Node_Index := Tree.Add_Nonterm
           ((+rhs_item_ID, 2), (1 => Tree.Add_Identifier (+IDENTIFIER_ID, List_Element)));

         RHS_Item_List_1 : constant Valid_Node_Index := Tree.Add_Nonterm ((+rhs_item_list_ID, 0), (1 => RHS_Item_1));

         RHS_Item_List_3 : constant Valid_Node_Index := Tree.Add_Nonterm ((+rhs_item_list_ID, 0), (1 => RHS_Item_2));

         RHS_Item_List_2 : constant Valid_Node_Index := Tree.Add_Nonterm
           ((+rhs_item_list_ID, 1), (1 => RHS_Item_List_3, 2 => RHS_Item_3));

         RHS_2 : constant Valid_Node_Index := Tree.Add_Nonterm ((+rhs_ID, 1), (1 => RHS_Item_List_1));
         RHS_3 : constant Valid_Node_Index := Tree.Add_Nonterm ((+rhs_ID, 1), (1 => RHS_Item_List_2));

         Bar_1 : constant Valid_Node_Index := Tree.Add_Terminal (+BAR_ID);

         RHS_List_2 : constant Valid_Node_Index := Tree.Add_Nonterm ((+rhs_list_ID, 1), (1 => RHS_2));

         List_Nonterminal : constant Valid_Node_Index := Tree_Add_Nonterminal
           (Child_1   => Tree.Add_Identifier (+IDENTIFIER_ID, List_Nonterm),
            Child_2   => Tree.Add_Terminal (+COLON_ID),
            Child_3   => Tree.Add_Nonterm
              ((+rhs_list_ID, 1),
               (1     => RHS_List_2,
                2     => Bar_1,
                3     => RHS_3)),
            Child_4   => Tree.Add_Nonterm
              ((+semicolon_opt_ID, 0),
               (1     => Tree.Add_Terminal (+SEMICOLON_ID))));
      begin
         Add_Compilation_Unit (List_Nonterminal);
      end New_Nonterminal_List;

      procedure Process_Node (Node : in Valid_Node_Index)
      is begin
         case To_Token_Enum (Tree.ID (Node)) is
         --  Token_ID alphabetical order
         when rhs_alternative_list_ID =>
            --  All handled by New_Nonterminal*
            raise SAL.Not_Implemented with Tree.Image (Node, Wisitoken_Grammar_Actions.Descriptor);

         when rhs_group_item_ID =>
            --  Current tree:
            --
            --  rhs_item: Parent (Node, 1)
            --  | rhs_group_item: Node
            --  | | LEFT_PAREN
            --  | | rhs_alternative_list: Child (Node, 2)
            --  | | RIGHT_PAREN
            declare
               New_Identifier : constant Identifier_Index := Next_Nonterm_Name;
            begin
               New_Nonterminal (New_Identifier, Child (Node, 2));

               Tree.Set_Node_Identifier (Node, +IDENTIFIER_ID, New_Identifier);
               Tree.Set_Children (Parent (Node, 1), (+rhs_item_ID, 1), (1 => Node));
               Data.EBNF_Nodes (Node) := False;
            end;

         when rhs_multiple_item_ID =>
            --  We could reuse some nodes by recognizing common special cases like
            --
            --  foo_list : foo {<separator> foo} ;
            --
            --  | ...  {foo} ...
            --
            --  But the code is tedious, complex, and easy to get wrong; not worth it.


            --  We have:
            --
            --  | ... { ... }  ...
            --  | ... { ... } - ...
            --
            --  Replace it with a new canonical list nonterminal.

            --  Current tree:
            --
            --  rhs_item: Parent (Node, 1)
            --  | rhs_multiple_item: Node
            --  | | LEFT_BRACE | LEFT_PAREN
            --  | | rhs_alternative_list
            --  | | | ...
            --  | | RIGHT_BRACE | RIGHT_PAREN
            --  | | [MINUS | PLUS | STAR]
            --  | ...

            declare
               Plus_Minus_Star  : constant Node_Index       := Child (Node, 4);
               Allow_Empty      : constant Boolean          := Plus_Minus_Star = Invalid_Node_Index or else
                 Tree.ID (Plus_Minus_Star) in +STAR_ID;
               Parent_RHS_Item  : constant Valid_Node_Index := Parent (Node, 1);
               List_Nonterm     : constant Identifier_Index := Next_Nonterm_Name ("_list");
               List_Opt_Nonterm : constant Base_Identifier_Index :=
                 (if Allow_Empty then Next_Nonterm_Name ("_list_opt") else Invalid_Identifier_Index);
               List_Element     : constant Identifier_Index := Next_Nonterm_Name;
            begin
               New_Nonterminal (List_Element, Child (Node, 2));

               if Allow_Empty then
                  New_Nonterminal_Opt (List_Opt_Nonterm, List_Nonterm);
               end if;

               New_Nonterminal_List (List_Nonterm, List_Element);

               Tree.Set_Children
                 (Parent_RHS_Item,
                  (+rhs_item_list_ID, 0),
                  (1 => Tree.Add_Identifier
                     (+IDENTIFIER_ID, (if Allow_Empty then List_Opt_Nonterm else List_Nonterm))));

               Data.EBNF_Nodes (Node) := False;
            end;

         when rhs_optional_item_ID =>
            --  ada_lite_ebnf.wy aspect_specification
            --
            --  current tree:
            --  compilation_unit_list
            --  ...
            --  | | | | rhs_item: Parent (Node, 1)
            --  | | | | | rhs_optional_item: Node
            --  | | | | | | LEFT_BRACKET: Node.Children (1)
            --  | | | | | | rhs_alternative_item_list: Node.Children (2)
            --  | | | | | | RIGHT_BRACKET: Node.Children (3)
            --  | <next rhs_item, semicolon_opt>

            --  new tree:
            --  compilation_unit_list
            --  | nonterminal: new
            --  | | identifier: new New_Identifier "$nonterminal_nnn_opt"
            --  | | colon: new
            --  | | rhs_list: new
            --  | | | rhs_list: new
            --  | | | | rhs: new
            --  | | | | | rhs_item: new
            --  | | | | | | rhs_alternative_item_list : Node.Children (2)
            --  | | | BAR: new
            --  | | | rhs: new, empty
            --  | | semicolon_opt
            --  ...
            --  | | | | | rhs_item: Parent (Node, 1)
            --  | | | | | | IDENTIFIER: Node New_Identifier
            --  | <next rhs, semicolon_opt>
            declare
               New_Identifier : constant Identifier_Index := Next_Nonterm_Name ("_opt");
            begin
               New_Nonterminal_Opt (New_Identifier, Child (Node, 2));

               Tree.Set_Node_Identifier (Node, +IDENTIFIER_ID, New_Identifier);
               Data.EBNF_Nodes (Node) := False;

               Tree.Set_Children (Parent (Node, 1), (+rhs_item_ID, 2), (1 => Node));
            end;

         when others =>
            Raise_Programmer_Error ("not an EBNF node", Tree, Node);
         end case;
      end Process_Node;

   begin
      --  We can't use Process_Tree because we are editing the tree.
      for I in Data.EBNF_Nodes.First_Index .. Data.EBNF_Nodes.Last_Index loop
         if Data.EBNF_Nodes (I) then
            if Trace_Generate > Detail then
               Ada.Text_IO.Put_Line ("rewrite node" & Node_Index'Image (I));
            end if;
            Process_Node (I);
         end if;
      end loop;

      Data.Meta_Syntax := BNF_Syntax;

   exception
   when E : SAL.Not_Implemented =>
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error, "Rewrite_EBNF_To_BNF not implemented: " & Ada.Exceptions.Exception_Message (E));
   end Rewrite_EBNF_To_BNF;

   procedure Print_Source
     (File_Name : in String;
      Tree      : in WisiToken.Syntax_Trees.Tree;
      Data      : in User_Data_Type)
   is
      use Ada.Text_IO;
      use WisiToken.Syntax_Trees;
      File : File_Type;

      procedure Put_Comments (Node : in Valid_Node_Index)
      is
         Token : constant Base_Token_Index := Tree.Max_Terminal_Index (Node);
      begin
         --  Not all tokens have trailing non_grammar, so Data.Non_Grammar may
         --  have entries for every token.
         if Token /= Invalid_Token_Index and then
           Token in Data.Non_Grammar.First_Index .. Data.Non_Grammar.Last_Index
         then
            declare
               Tokens : Base_Token_Arrays.Vector renames Data.Non_Grammar (Token);
            begin
               for Token of Tokens loop
                  Put (File, Data.Grammar_Lexer.Buffer_Text (Token.Byte_Region));
               end loop;
            end;
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

      procedure Put_RHS_Item (Node : in Valid_Node_Index)
      is
         Children : constant Valid_Node_Index_Array := Tree.Children (Node);
      begin
         --  We don't raise an exception for errors here; it's easier to debug from the
         --  mangled source listing.

         case To_Token_Enum (Tree.ID (Children (1))) is
         when IDENTIFIER_ID =>
            if Children'Length = 1 then
               Put (File, Get_Text (Data, Tree, Children (1)));
            else
               New_Line (File);
               Put (File, " ;; not translated: " & Tree.Image (Node, Wisitoken_Grammar_Actions.Descriptor));
            end if;

         when LESS_ID | STRING_LITERAL_2_ID | rhs_optional_item_ID | rhs_multiple_item_ID | rhs_group_item_ID =>
            New_Line (File);
            Put (File, " ;; not translated: " & Tree.Image (Children (1), Wisitoken_Grammar_Actions.Descriptor));

         when others =>
            New_Line (File);
            Put (File, " ;; bad translation: " & Node_Index'Image (Node) & ":" &
              Tree.Image (Node, Wisitoken_Grammar_Actions.Descriptor, Include_Children => True));
         end case;
      end Put_RHS_Item;

      procedure Put_RHS_Item_List (Node : in Valid_Node_Index)
      is
         Children : constant Valid_Node_Index_Array := Tree.Children (Node);
      begin
         if Children'Length = 1 then
            Put_RHS_Item (Children (1));
         else
            Put_RHS_Item_List (Children (1));
            Put (File, ' ');
            Put_RHS_Item (Children (2));
         end if;
      end Put_RHS_Item_List;

      procedure Put_RHS
        (Node    : in Valid_Node_Index;
         First   : in Boolean;
         Virtual : in Boolean)
      is
         Children : constant Valid_Node_Index_Array := Tree.Children (Node);
      begin
         Put (File, (if First then "  : " else "  | "));
         case Tree.RHS_Index (Node) is
         when 0 =>
            if Virtual then
               Put_Line (File, ";; empty");
            else
               Put_Comments (Tree.Parent (Node));
            end if;

         when 1 .. 3 =>
            Put_RHS_Item_List (Children (1));
            if Virtual then
               New_Line (File);
            else
               Put_Comments (Children (1));
            end if;

            if Tree.RHS_Index (Node) > 1 then
               Put (File, "    %(" & Get_Text (Data, Tree, Children (2)) & ")%"); -- action
               if Virtual then
                  New_Line (File);
               else
                  Put_Comments (Children (2));
               end if;
               if Tree.RHS_Index (Node) > 2 then
                  Put (File, "    %(" & Get_Text (Data, Tree, Children (3)) & ")%"); -- check
                  if Virtual then
                     New_Line (File);
                  else
                     Put_Comments (Children (3));
                  end if;
               end if;
            end if;

         when 4 =>
            Put
              (File, "%if " & Get_Text (Data, Tree, Children (3)) & " = " & Get_Text (Data, Tree, Children (4)));
            Put_Comments (Node);

         when 5 =>
            Put (File, "%end if");
            Put_Comments (Node);

         when others =>
            Raise_Programmer_Error ("Put_RHS", Tree, Node);
         end case;
      end Put_RHS;

      procedure Put_RHS_List
        (Node    : in     Valid_Node_Index;
         First   : in out Boolean;
         Virtual : in     Boolean)
      is
         Children : constant Valid_Node_Index_Array := Tree.Children (Node);
      begin
         case Children'Length is
         when 1 =>
            Put_RHS (Children (1), First, Virtual);
            First := False;
         when 3 =>
            Put_RHS_List (Children (1), First, Virtual);
            Put_RHS (Children (3), First => False, Virtual => Virtual);
         when others =>
            Raise_Programmer_Error ("Put_RHS_List", Tree, Node);
         end case;
      end Put_RHS_List;

      procedure Process_Node (Node : in Valid_Node_Index)
      is begin
         case To_Token_Enum (Tree.ID (Node)) is
         --  Token_ID alphabetical order
         when compilation_unit_ID =>
            Process_Node (Tree.Children (Node)(1));

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
                     Put (File, "%non_grammar <" & Get_Text (Data, Tree, Tree.Children (Children (2))(3)) & ">");
                  when 2 =>
                     Put (File, "%token <" & Get_Text (Data, Tree, Tree.Children (Children (2))(3)) & ">");
                  when others =>
                     raise SAL.Programmer_Error;
                  end case;

                  Put (File, " " & Get_Text (Data, Tree, Children (3)));
                  Put_Declaration_Item_List (Children (4));
                  Put_Comments (Children (4));

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

               First : Boolean := True;

            begin
               Put (File, Get_Text (Data, Tree, Children (1)));
               if Virtual then
                  New_Line (File);
               else
                  Put_Comments (Children (1));
               end if;

               Put_RHS_List (Children (3), First, Virtual);

               if Tree.Children (Children (4))'Length > 0 then
                  if Virtual then
                     Put_Line (File, "  ;");
                  else
                     Put (File, "  ;");
                     Put_Comments (Children (4));
                  end if;
               else
                  Put_Comments (Children (3));
               end if;
            end;

         when wisitoken_accept_ID =>
            Process_Node (Tree.Children (Node)(1));

         when others =>
            raise SAL.Not_Implemented with Image (Tree.ID (Node), Wisitoken_Grammar_Actions.Descriptor);
         end case;
      end Process_Node;
   begin
      Create (File, Out_File, File_Name);
      Put_Line (File, ";;; generated from " & Data.Grammar_Lexer.File_Name);
      Put_Line (File, ";;;");

      declare
         Tokens : Base_Token_Arrays.Vector renames Data.Non_Grammar (0);
      begin
         for Token of Tokens loop
            Put (File, Data.Grammar_Lexer.Buffer_Text (Token.Byte_Region));
         end loop;
      end;

      Process_Node (Tree.Root);
   exception
   when E : SAL.Not_Implemented =>
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error, "Print_Source not implemented: " & Ada.Exceptions.Exception_Message (E));
   end Print_Source;

end WisiToken_Grammar_Runtime;
