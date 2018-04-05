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
         if -Tree.ID (Tree_Index) in REGEXP_ID | ACTION_ID then
            --  strip delimiters.
            return Data.Lexer.Buffer_Text ((Region.First + 2, Region.Last - 2));

         elsif -Tree.ID (Tree_Index) in STRING_LITERAL_ID | STRING_LITERAL_CASE_INS_ID and Strip_Quotes then
            return Data.Lexer.Buffer_Text ((Region.First + 1, Region.Last - 1));
         else
            return Data.Lexer.Buffer_Text (Region);
         end if;
      end Strip_Delimiters;

   begin
      case Tree.Label (Tree_Index) is
      when Shared_Terminal =>
         return Strip_Delimiters (Tree_Index);

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
               Result := Result & (if Need_Space then " " else "") & Strip_Delimiters (Tree_Index);
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
      use all type Wisi.Lexer_Type;
   begin
      if "lexer" = Get_Text (Data, Tree, A_Index) then
         Data.Ignore_Lines := Data.Generate_Params.Lexer /= Wisi.To_Lexer (Get_Text (Data, Tree, B_Index));
      else
         raise Grammar_Error with "invalid '%if'; only 'lexer' supported";
      end if;
   end Start_If_1;

   function Get_RHS
     (Data  : in out User_Data_Type;
      Tree  : in     Syntax_Trees.Tree;
      Token : in     Syntax_Trees.Valid_Node_Index)
     return Wisi.RHS_Type
   is
      use all type SAL.Base_Peek_Type;
      Tokens : constant Syntax_Trees.Valid_Node_Index_Array := Tree.Children (Token);
   begin
      pragma Assert (-Tree.ID (Token) = rhs_ID);

      if Tokens'Length = 0 then
         return Wisi.RHS_Type'(others => <>);
      end if;

      return RHS : Wisi.RHS_Type do
         for I of Tree.Get_Terminals (Tokens (1)) loop
            RHS.Production.Append (Get_Text (Data, Tree, I));
         end loop;

         if Tokens'Last >= 2 then
            declare
               Text : constant String := Get_Text (Data, Tree, Tokens (2));
            begin
               if Text'Length > 0 then
                  RHS.Action := +Text;
                  Data.Action_Count := Data.Action_Count + 1;
               end if;
            end;
         end if;

         if Tokens'Last >= 3 then
            RHS.Check := +Get_Text (Data, Tree, Tokens (3));
            Data.Check_Count := Data.Check_Count + 1;
         end if;
         RHS.Source_Line := Data.Terminals.all (Tree.Min_Terminal_Index (Token)).Line;
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
      pragma Assert (-Tree.ID (Token) = rhs_list_ID);

      if Tokens'Last = 1 then
         --  | rhs
         if not Data.Ignore_Lines then
            Right_Hand_Sides.Append (Get_RHS (Data, Tree, Tokens (1)));
         end if;
      else
         --  | rhs_list BAR rhs
         --  | rhs_list PERCENT IF IDENTIFIER EQUAL IDENTIFIER
         --  | rhs_list PERCENT END IF
         Get_Right_Hand_Sides (Data, Tree, Right_Hand_Sides, Tokens (1));

         case Token_Enum_ID'(-Tree.ID (Tokens (3))) is
         when rhs_ID =>
            if not Data.Ignore_Lines then
               Right_Hand_Sides.Append (Get_RHS (Data, Tree, Tokens (3)));
            end if;

         when IF_ID =>
            Start_If_1 (Data, Tree, Tokens (4), Tokens (6));

         when END_ID =>
            Data.Ignore_Lines := False;

         when others =>
            raise Programmer_Error;
         end case;
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
      Data.Prologues := (others => <>);

      --  Preserve Generate_Params items set by wisi-generate command line options
      Data.Generate_Params.Case_Insensitive := False;
      Data.Generate_Params.End_Names_Optional_Option := +"";
      --  First_Parser_Label          := 0;
      --  First_State_Index          := 0;
      --  Interface_Kind             := None;
      --  Lexer                      := None;
      --  Output_Language            := None;
      --  Parser_Algorithm           := None;
      Data.Generate_Params.Start_Token := +"";

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
      First : Integer         := Text'First + 3;
      Last  : Integer         := Index (Text, "%%");
   begin
      --  <delimited-text> token includes the delimiters. We also strip
      --  newlines at the delimiters.
      if Last = 0 then
         Data.Prologues.Spec_Context_Clause := Wisi.Split_Lines (Text (First .. Text'Last - 3));
         return;
      end if;

      Data.Prologues.Spec_Context_Clause := Wisi.Split_Lines (Text (First .. Last - 1));
      First := Last + 3;
      Last  := Index (Text, "%%", First);

      if Last = 0 then
         Data.Prologues.Spec_Declarations := Wisi.Split_Lines (Text (First .. Text'Last - 3));
         return;
      end if;

      Data.Prologues.Spec_Declarations := Wisi.Split_Lines (Text (First .. Last - 1));
      First := Last + 3;
      Last  := Index (Text, "%%", First);

      if Last = 0 then
         Data.Prologues.Body_Context_Clause := Wisi.Split_Lines (Text (First .. Text'Last - 3));
         return;
      end if;
      Data.Prologues.Body_Context_Clause := Wisi.Split_Lines (Text (First .. Last - 1));

      Data.Prologues.Body_Declarations := Wisi.Split_Lines (Text (Last + 3 .. Text'Last - 3));
   end Add_Preamble;

   procedure Start_If
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in     WisiToken.Syntax_Trees.Tree;
      Tokens    : in     WisiToken.Syntax_Trees.Valid_Node_Index_Array)
   is begin
      Start_If_1 (User_Data_Type (User_Data), Tree, Tokens (3), Tokens (5));
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
                  Kind : constant String := Data.Lexer.Buffer_Text (Token.Byte_Region);
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
                     Data.Elisp_Names.Faces.Append (Get_Text (Data, Tree, Tokens (3), Strip_Quotes => True));

                  elsif Kind = "elisp_indent" then
                     Data.Elisp_Names.Indents.Append
                       ((Name  => +Get_Child_Text (Data, Tree, Tokens (3), 1, Strip_Quotes => True),
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
                     Put_Error (Error_Message (-Data.Input_File_Name, Token.Line, Token.Col, "unexpected syntax"));
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
