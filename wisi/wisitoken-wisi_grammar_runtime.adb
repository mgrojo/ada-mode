--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018 Stephen Leake All Rights Reserved.
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

with Ada.Strings.Fixed;
with Wisi_Grammar; use Wisi_Grammar;
package body WisiToken.Wisi_Grammar_Runtime is

   function Get_Text
     (Data        : in User_Data_Type;
      Tree        : in Syntax_Trees.Tree;
      Tree_Index : in Syntax_Trees.Valid_Node_Index)
     return String
   is
      use all type Syntax_Trees.Node_Label;
   begin
      case Tree.Label (Tree_Index) is
      when Shared_Terminal =>
         return Data.Lexer.Buffer_Text (Data.Terminals.all (Tree.Terminal (Tree_Index)).Byte_Region);
      when Virtual_Terminal =>
         raise Programmer_Error;
      when Nonterm =>
         declare
            use all type Ada.Strings.Unbounded.Unbounded_String;
            Result       : Ada.Strings.Unbounded.Unbounded_String;
            Tree_Indices : constant Syntax_Trees.Valid_Node_Index_Array := Tree.Get_Terminals (Tree_Index);
            Need_Space   : Boolean                                      := False;
         begin
            for Tree_Index of Tree_Indices loop
               Result := Result & (if Need_Space then " " else "") & Data.Lexer.Buffer_Text
                 (Data.Terminals.all (Tree.Terminal (Tree_Index)).Byte_Region);
               Need_Space := True;
            end loop;
            return -Result;
         end;
      end case;
   end Get_Text;

   function Get_Child_Text
     (Data   : in User_Data_Type;
      Tree   : in Syntax_Trees.Tree;
      Parent : in Syntax_Trees.Valid_Node_Index;
      Child  : in SAL.Peek_Type)
     return String
   is
      Tree_Indices : constant Syntax_Trees.Valid_Node_Index_Array := Tree.Get_Terminals (Parent);
   begin
      return Get_Text (Data, Tree, Tree_Indices (Child));
   end Get_Child_Text;

   function Get_RHS
     (Data  : in out User_Data_Type;
      Tree  : in     Syntax_Trees.Tree;
      Token : in     Syntax_Trees.Valid_Node_Index)
     return Wisi.RHS_Type
   is
      use all type SAL.Base_Peek_Type;
      Tokens : constant Syntax_Trees.Valid_Node_Index_Array := Tree.Children (Token);
   begin
      return RHS : Wisi.RHS_Type do
         for I of Tree.Get_Terminals (Tokens (1)) loop
            RHS.Production.Append (Get_Text (Data, Tree, I));
         end loop;

         if Tokens'Last >= 2 then
            RHS.Action.Append (Get_Text (Data, Tree, Tokens (2)));
            Data.Action_Count := Data.Action_Count + 1;
         end if;
         if Tokens'Last >= 3 then
            RHS.Check.Append (Get_Text (Data, Tree, Tokens (3)));
            Data.Check_Count := Data.Check_Count + 1;
         end if;
         RHS.Source_Line := 1; -- FIXME: need augmented token
      end return;
   end Get_RHS;

   procedure Get_Right_Hand_Sides
     (Data             : in out User_Data_Type;
      Tree             : in     WisiToken.Syntax_Trees.Tree;
      Right_Hand_Sides : in out Wisi.RHS_Lists.List;
      Token            : in     WisiToken.Syntax_Trees.Valid_Node_Index)
   is
      use all type SAL.Base_Peek_Type;

      Tokens : constant Syntax_Trees.Valid_Node_Index_Array := Tree.Children (Token);
   begin
      if Tokens'Last = 1 then
         --  | rhs
         if Tree.Has_Children (Tokens (1)) and then
           +token_list_ID = Tree.ID (Tree.Children (Tokens (1))(1))
         then
            Right_Hand_Sides.Append (Get_RHS (Data, Tree, Tokens (1)));

            --  else empty, %if, or %end if
         end if;
      else
         --  | rhs_list BAR rhs
         Get_Right_Hand_Sides (Data, Tree, Right_Hand_Sides, Tokens (1));
         Right_Hand_Sides.Append (Get_RHS (Data, Tree, Tokens (3)));

      end if;
   end Get_Right_Hand_Sides;

   ----------
   --  Public subprograms, declaration order

   overriding
   procedure Set_Lexer_Terminals
     (User_Data : in out User_Data_Type;
      Lexer     : in     WisiToken.Lexer.Handle;
      Terminals : in     Base_Token_Array_Access)
   is begin
      User_Data.Lexer     := Lexer;
      User_Data.Terminals := Terminals;
   end Set_Lexer_Terminals;

   overriding procedure Reset (Data : in out User_Data_Type)
   is begin
      --  Preserve Lexer, Terminals
      Data.Prologues        := (others => <>);
      Data.Generate_Params  := (others => <>);
      Data.Tokens           := (others => <>);
      Data.Elisp_Names      := (others => <>);
      Data.Conflicts.Clear;
      Data.McKenzie_Recover := (others => <>);
      --  Preserve Input_File_Name
      Data.Rule_Count       := 0;
      Data.Action_Count     := 0;
      Data.Check_Count      := 0;
      Data.Ignore_Lines     := False;
   end Reset;

   procedure Add_Preamble
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in     WisiToken.Syntax_Trees.Tree;
      Tokens    : in     WisiToken.Syntax_Trees.Valid_Node_Index_Array)
   is
      use Ada.Strings.Fixed;
      Data  : User_Data_Type renames User_Data_Type (User_Data);
      Text  : constant String := Get_Text (Data, Tree, Tokens (1));
      First : Integer;
      Last  : Integer         := Index (Text, "%%");
   begin
      if Last = 0 then
         Data.Prologues.Spec_Context_Clause.Append (Text); --  FIXME: change to unbounded string
         return;
      end if;

      Data.Prologues.Spec_Context_Clause.Append (Text (Text'First .. Last - 1));
      First := Last + 3; -- skip newline
      Last  := Index (Text, "%%", First);

      if Last = 0 then
         Data.Prologues.Spec_Declarations.Append (Text (First .. Text'Last));
         return;
      end if;

      Data.Prologues.Spec_Declarations.Append (Text (First .. Last - 1));
      First := Last + 3; -- skip newline
      Last  := Index (Text, "%%", First);

      if Last = 0 then
         Data.Prologues.Body_Context_Clause.Append (Text (First .. Text'Last));
         return;
      end if;
      Data.Prologues.Body_Context_Clause.Append (Text (First .. Last - 1));

      Data.Prologues.Body_Declarations.Append (Text (Last + 3 .. Text'Last));
   end Add_Preamble;

   procedure Start_If
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in     WisiToken.Syntax_Trees.Tree;
      Tokens    : in     WisiToken.Syntax_Trees.Valid_Node_Index_Array)
   is
      use all type Wisi.Lexer_Type;
      Data : User_Data_Type renames User_Data_Type (User_Data);
   begin
      if "lexer" = Get_Text (Data, Tree, Tokens (3)) then
         Data.Ignore_Lines := Data.Generate_Params.Lexer /= Wisi.To_Lexer (Get_Text (Data, Tree, Tokens (5)));
      else
         raise Grammar_Error with "invalid '%if'; only 'lexer' supported";
      end if;
   end Start_If;

   procedure End_If (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class)
   is
      Data : User_Data_Type renames User_Data_Type (User_Data);
   begin
      Data.Ignore_Lines := False;
   end End_If;

   procedure Add_Declaration
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in     WisiToken.Syntax_Trees.Tree;
      Tokens    : in     WisiToken.Syntax_Trees.Valid_Node_Index_Array)
   is
      use all type Ada.Strings.Unbounded.Unbounded_String;
      use all type Wisi.Lexer_Type;
      use all type Wisi.Interface_Type;
      use all type Wisi.Output_Language_Type;
      use all type Wisi.Parser_Algorithm_Type;

      Data : User_Data_Type renames User_Data_Type (User_Data);
   begin
      --  Add declaration to User_Data.Generate_Params, Tokens, Conflicts,
      --  or McKenzie_Recover.

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
            when Wisi_Grammar.TOKEN_ID =>

               Wisi.Add_Token
                 (Data.Tokens.Tokens,
                  Kind  => Get_Text (Data, Tree, Children (3)),
                  Name  => Get_Text (Data, Tree, Tokens (3)),
                  Value => Get_Text (Data, Tree, Tokens (4)));

            when KEYWORD_ID =>

               Data.Tokens.Keywords.Append
                 ((Name  => +Get_Text (Data, Tree, Tokens (3)),
                   Value => +Get_Text (Data, Tree, Tokens (4))));

            when NON_GRAMMAR_ID =>

               Wisi.Add_Token
                 (Data.Tokens.Non_Grammar,
                  Kind  => Get_Text (Data, Tree, Children (3)),
                  Name  => Get_Text (Data, Tree, Tokens (3)),
                  Value => Get_Text (Data, Tree, Tokens (4)));

            when others =>
               raise Programmer_Error;
            end case;
         end;

      when Syntax_Trees.Shared_Terminal =>
         declare
            Token : Base_Token renames Data.Terminals.all (Tree.Terminal (Tokens (2)));
         begin
            case Token_Enum_ID'(-Token.ID) is
            when IDENTIFIER_ID =>
               declare
                  Kind : constant String := Get_Text (Data, Tree, Tokens (2));
               begin
                  if Kind = "case_insensitive" then
                     Data.Generate_Params.Case_Insensitive := True;

                  elsif Kind = "conflict" then
                     declare
                        Tree_Indices : constant Syntax_Trees.Valid_Node_Index_Array := Tree.Get_Terminals
                          (Tokens (3));
                     --   %conflict <action_a>/<action_b> in state <LHS_A>, <LHS_B> on token <on>
                     --              1        2 3         4  5      6     7  8      9  10     11
                     begin
                        Data.Conflicts.Append
                          ((Source_Line => 1, -- FIXME: need augmented tokens
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
                     Data.Elisp_Names.Faces.Append (Get_Text (Data, Tree, Tokens (3)));

                  elsif Kind = "elisp_indent" then
                     Data.Elisp_Names.Indents.Append
                       ((Name  => +Get_Child_Text (Data, Tree, Tokens (3), 1),
                         Value => +Get_Child_Text (Data, Tree, Tokens (3), 2)));

                  elsif Kind = "elisp_regexp" then
                     Data.Elisp_Names.Regexps.Append
                       ((Name  => +Get_Child_Text (Data, Tree, Tokens (3), 1),
                         Value => +Get_Child_Text (Data, Tree, Tokens (3), 2)));

                  elsif Kind = "end_names_optional_option" then
                     Data.Generate_Params.End_Names_Optional_Option := +Get_Text (Data, Tree, Tokens (3));

                  elsif Kind = "first_parser_label" then
                     Data.Generate_Params.First_Parser_Label := Integer'Value (Get_Text (Data, Tree, Tokens (3)));

                  elsif Kind = "first_state_index" then
                     Data.Generate_Params.First_State_Index := Integer'Value (Get_Text (Data, Tree, Tokens (3)));

                  elsif Kind = "if" then
                     Data.Ignore_Lines :=
                       Data.Generate_Params.Lexer /= Wisi.To_Lexer (Get_Child_Text (Data, Tree, Tokens (3), 3));

                  elsif Kind = "interface" then
                     if Data.Generate_Params.Interface_Kind = None then
                        Data.Generate_Params.Interface_Kind := Wisi.Valid_Interface'Value
                          (Get_Text (Data, Tree, Tokens (3)));
                     end if;

                  elsif Kind = "lexer" then
                     if Data.Generate_Params.Lexer = None then
                        Data.Generate_Params.Lexer := Wisi.To_Lexer (Get_Text (Data, Tree, Tokens (3)));
                     end if;

                  elsif Kind = "mckenzie_check_limit" then
                     Data.McKenzie_Recover.Check_Limit := Integer'Value (Get_Text (Data, Tree, Tokens (3)));

                  elsif Kind = "mckenzie_cost_default" then
                     Data.McKenzie_Recover.Default_Insert          := Natural'Value
                       (Get_Child_Text (Data, Tree, Tokens (3), 1));
                     Data.McKenzie_Recover.Default_Delete_Terminal := Natural'Value
                       (Get_Child_Text (Data, Tree, Tokens (3), 2));
                     Data.McKenzie_Recover.Default_Delete_Nonterminal  := Natural'Value
                       (Get_Child_Text (Data, Tree, Tokens (3), 3));
                     Data.McKenzie_Recover.Default_Push_Back       := Natural'Value
                       (Get_Child_Text (Data, Tree, Tokens (3), 4));
                     Data.McKenzie_Recover.Default_Undo_Reduce     := Natural'Value
                       (Get_Child_Text (Data, Tree, Tokens (3), 5));

                  elsif Kind = "mckenzie_cost_delete" then
                     Data.McKenzie_Recover.Delete.Append
                       ((+Get_Child_Text (Data, Tree, Tokens (3), 1),
                         +Get_Child_Text (Data, Tree, Tokens (3), 2)));

                  elsif Kind = "mckenzie_cost_insert" then
                     Data.McKenzie_Recover.Insert.Append
                       ((+Get_Child_Text (Data, Tree, Tokens (3), 1),
                         +Get_Child_Text (Data, Tree, Tokens (3), 2)));

                  elsif Kind = "mckenzie_cost_limit" then
                     Data.McKenzie_Recover.Cost_Limit := Natural'Value (Get_Text (Data, Tree, Tokens (3)));

                  elsif Kind = "output_language" then
                     if Data.Generate_Params.Output_Language = None then
                        Data.Generate_Params.Output_Language := Wisi.Valid_Output_Language'Value
                          (Get_Text (Data, Tree, Tokens (3)));
                     end if;

                  elsif Kind = "parser_algorithm" then
                     if Data.Generate_Params.Parser_Algorithm = None then
                        Data.Generate_Params.Parser_Algorithm := Wisi.Valid_Parser_Algorithm'Value
                          (Get_Text (Data, Tree, Tokens (3)));
                     end if;

                  elsif Kind = "start" then
                     Data.Generate_Params.Start_Token := +Get_Text (Data, Tree, Tokens (3));

                  elsif Kind = "re2c_regexp" then
                     Data.Tokens.Regexps.Append
                       ((+Get_Child_Text (Data, Tree, Tokens (3), 1),
                         +Get_Child_Text (Data, Tree, Tokens (3), 2)));

                  else
                     Put_Error (Error_Message (-Data.Input_File_Name, 1, 0, "unexpected syntax"));
                     --  FIXME: need Augmented_token
                     raise WisiToken.Grammar_Error;

                  end if;
               end;

            when others =>
               raise Grammar_Error;
            end case;
         end;

      when Syntax_Trees.Virtual_Terminal =>
         raise Programmer_Error;
      end case;
   end Add_Declaration;

   procedure Add_Nonterminal
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in     WisiToken.Syntax_Trees.Tree;
      Tokens    : in     WisiToken.Syntax_Trees.Valid_Node_Index_Array)
   is
      Data : User_Data_Type renames User_Data_Type (User_Data);

      Right_Hand_Sides : Wisi.RHS_Lists.List;
   begin
      Data.Rule_Count := Data.Rule_Count + 1;

      Get_Right_Hand_Sides (Data, Tree, Right_Hand_Sides, Tokens (3));

      Data.Tokens.Rules.Append ((+Get_Text (Data, Tree, Tokens (1)), Right_Hand_Sides));
   end Add_Nonterminal;

end WisiToken.Wisi_Grammar_Runtime;
