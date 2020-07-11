--  Abstract :
--
--  see spec.
--
--  Copyright (C) 2017 - 2020 Free Software Foundation, Inc.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada_Annex_P_Process_Actions; --  token_enum_id
package body Wisi.Ada_Annex_P is
   use WisiToken;

   ----------
   --  body local subprograms

   function Indent_Record
     (Data              : in out Parse_Data_Type;
      Tree              : in     Syntax_Trees.Tree;
      Anchor_Token      : in     Augmented_Token;
      Record_Token      : in     Augmented_Token;
      Indenting_Token   : in     Augmented_Token;
      Indenting_Comment : in     Boolean;
      Offset            : in     Integer)
     return Wisi.Delta_Type
   is
      use Ada_Annex_P_Process_Actions;
   begin
      if not Indenting_Comment and Indenting_Token.ID = +RECORD_ID then
         --  Indenting 'record'
         return Indent_Anchored_2
           (Data, Anchor_Token.Line, Record_Token.Last_Line (Indenting_Comment), Ada_Indent_Record_Rel_Type,
            Accumulate => True);

      elsif Indenting_Comment and Indenting_Token.ID = +WITH_ID then
         --  comment before 'record'. test/ada_mode-nominal-child.ads Child_Type_1
         return Indent_Anchored_2
           (Data, Anchor_Token.Line, Indenting_Token.Last_Line (Indenting_Comment), Ada_Indent_Record_Rel_Type,
            Accumulate => True);

      elsif Indenting_Comment and Indenting_Token.ID = +IS_ID then
         --  comment after 'is'
         if Record_Token.ID = +RECORD_ID then
            --  before 'record'. test/ada_mode-nominal.ads Record_Type_1
            return Indent_Anchored_2
              (Data, Anchor_Token.Line, Indenting_Token.Last_Line (Indenting_Comment), Ada_Indent_Record_Rel_Type,
               Accumulate => True);
         else
            --  not before 'record'. test/ada_mode-nominal-child.ads Child_Type_1
            return (Simple, (Int, Offset));
         end if;

      else
         --  Indenting comment after 'record', other comment, component or 'end'
         --
         --  Ensure 'record' line is anchored to Anchor_Token.
         if not (Data.Indents (Record_Token.Line).Label = Anchored or
                   Data.Indents (Record_Token.Line).Label = Anchor_Anchored)
         then
            if Anchor_Token.Line /= Record_Token.Line then
               --  We don't pass Indenting_Comment here, because 'record' is code.
               Indent_Token_1
                 (Data,
                  Tree,
                  Indenting_Token         => Record_Token,
                  Delta_Indent            => Indent_Anchored_2
                    (Data,
                     Anchor_Token.Line,
                     Record_Token.Last_Line
                       (Indenting_Comment => False),
                     Ada_Indent_Record_Rel_Type,
                     Accumulate           => True),
                  Indenting_Comment       => False);
            end if;
         end if;

         return Indent_Anchored_2
           (Data,
            Anchor_Line => Anchor_Token.Line,
            Last_Line   => Indenting_Token.Last_Line (Indenting_Comment),
            Offset      => Current_Indent_Offset
              (Data, Anchor_Token,
               Offset   =>
                 (if Anchor_Token.Line = Record_Token.Line
                  then Offset
                  else Offset + Ada_Indent_Record_Rel_Type)),
            Accumulate => True);
      end if;
   end Indent_Record;

   ----------
   --  Refactor body subprograms

   function Find_ID_At
     (Tree       : in WisiToken.Syntax_Trees.Tree;
      ID         : in Token_ID;
      Edit_Begin : in WisiToken.Buffer_Pos)
     return WisiToken.Node_Index
   is
      function Match (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Boolean
        is (Tree.ID (Node) = ID and then Get_Aug_Token_Const_1 (Tree, Node).Byte_Region.First = Edit_Begin);
   begin
      return Tree.Find_Descendant (Tree.Root, Predicate => Match'Access);
   end Find_ID_At;

   procedure Unrecognized
     (Expecting  : in String;
      Found      : in WisiToken.Valid_Node_Index;
      Edit_Begin : in WisiToken.Buffer_Pos)
   with No_Return
   is begin
      raise SAL.Parameter_Error with "unrecognized subprogram call at byte_pos" & Edit_Begin'Image &
        "; expecting " & Expecting & " found node" & Found'Image;
   end Unrecognized;

   procedure Method_Object_To_Object_Method
     (Tree       : in     WisiToken.Syntax_Trees.Tree;
      Data       : in out Parse_Data_Type;
      Edit_Begin : in     WisiToken.Buffer_Pos)
   is
      pragma Unreferenced (Data);
      --  Data.Tree contains one statement or declaration; Edit_Begin is at
      --  start of a subprogram call. Convert the subprogram call from
      --  Prefix.Method (Object, ...) to Object.Method (...).

      use Ada_Annex_P_Process_Actions;

      Call : constant Node_Index := Find_ID_At (Tree, +name_ID, Edit_Begin);
   begin
      Unrecognized ("not implemented", Call, Edit_Begin);
   end Method_Object_To_Object_Method;

   procedure Object_Method_To_Method_Object
     (Tree       : in     WisiToken.Syntax_Trees.Tree;
      Data       : in out Parse_Data_Type;
      Edit_Begin : in     WisiToken.Buffer_Pos)
   is
      pragma Unreferenced (Data);
      --  Data.Tree contains one statement or declaration; Edit_Begin is at
      --  start of a subprogram call. Convert the subprogram call from
      --  Object.Method (...) to Method (Object, ...).
      use Ada_Annex_P_Process_Actions;

      Call : constant Node_Index := Find_ID_At (Tree, +name_ID, Edit_Begin);
   begin
      Unrecognized ("not implemented", Call, Edit_Begin);
   end Object_Method_To_Method_Object;

   procedure Element_Object_To_Object_Index
     (Tree       : in     WisiToken.Syntax_Trees.Tree;
      Data       : in out Parse_Data_Type;
      Edit_Begin : in     WisiToken.Buffer_Pos)
   is
      pragma Unreferenced (Data);
      --  Data.Tree contains one statement or declaration; Edit_Begin is at
      --  start of a subprogram call. Convert the subprogram call from
      --  Prefix.Element (Object, Index) to Object (Index).

      use Ada_Annex_P_Process_Actions;

      Call             : constant Node_Index := Find_ID_At (Tree, +name_ID, Edit_Begin);
   begin
      Unrecognized ("not implemented", Call, Edit_Begin);
   end Element_Object_To_Object_Index;

   procedure Object_Index_To_Element_Object
     (Tree       : in     WisiToken.Syntax_Trees.Tree;
      Data       : in out Parse_Data_Type;
      Edit_Begin : in     WisiToken.Buffer_Pos)
   is
      pragma Unreferenced (Data);
      --  Data.Tree contains one statement or declaration; Edit_Begin is at
      --  start of a subprogram call. Convert the subprogram call from
      --  Object (Index) to Element (Object, Index).

      use Ada_Annex_P_Process_Actions;

      Call             : constant Node_Index := Find_ID_At (Tree, +name_ID, Edit_Begin);
   begin
      Unrecognized ("not implemented", Call, Edit_Begin);
   end Object_Index_To_Element_Object;

   procedure Format_Parameter_List
     (Tree       : in out WisiToken.Syntax_Trees.Tree;
      Data       : in out Parse_Data_Type;
      Edit_Begin : in     WisiToken.Buffer_Pos)
   is separate;
   --  Data.Tree contains a subprogram declaration or body; Edit_Begin is
   --  at the start of a parameter list. Format the parameter list.
   --
   --  Handle virtual tokens as much as possible; at least closing paren.

   ----------
   --  Public subprograms, declaration order

   overriding
   procedure Initialize
     (Data              : in out Parse_Data_Type;
      Lexer             : in     WisiToken.Lexer.Handle;
      Descriptor        : access constant WisiToken.Descriptor;
      Base_Terminals    : in     WisiToken.Base_Token_Array_Access;
      Post_Parse_Action : in     Post_Parse_Action_Type;
      Begin_Line        : in     WisiToken.Line_Number_Type;
      End_Line          : in     WisiToken.Line_Number_Type;
      Begin_Indent      : in     Integer;
      Params            : in     String)
   is
      use Standard.Ada.Strings.Fixed;
      use all type Ada_Annex_P_Process_Actions.Token_Enum_ID;
      First : Integer := Params'First;
      Last  : Integer := Index (Params, " ");
   begin
      Wisi.Initialize
        (Wisi.Parse_Data_Type (Data), Lexer, Descriptor, Base_Terminals, Post_Parse_Action, Begin_Line, End_Line,
         Begin_Indent, "");

      Data.First_Comment_ID := +COMMENT_ID;
      Data.Last_Comment_ID  := WisiToken.Invalid_Token_ID;
      Data.Left_Paren_ID    := +LEFT_PAREN_ID;
      Data.Right_Paren_ID   := +RIGHT_PAREN_ID;

      Data.Embedded_Quote_Escape_Doubled := True;

      if Params /= "" then
         Ada_Indent := Integer'Value (Params (First .. Last - 1));

         First := Last + 1;
         Last := Index (Params, " ", First);
         Ada_Indent_Broken := Integer'Value (Params (First .. Last - 1));

         First := Last + 1;
         Last := First + 1;
         Ada_Indent_Comment_Col_0 := Params (First) = '1';

         First := Last + 1;
         Last := First + 1;
         Ada_Indent_Comment_GNAT := Params (First) = '1';

         First := Last + 1;
         Last := Index (Params, " ", First);
         Ada_Indent_Label := Integer'Value (Params (First .. Last - 1));

         First := Last + 1;
         Last := Index (Params, " ", First);
         Ada_Indent_Record_Rel_Type := Integer'Value (Params (First .. Last - 1));

         First := Last + 1;
         Last := Index (Params, " ", First);
         Ada_Indent_Renames := Integer'Value (Params (First .. Last - 1));

         First := Last + 1;
         Last := Index (Params, " ", First);
         Ada_Indent_Return := Integer'Value (Params (First .. Last - 1));

         First := Last + 1;
         Last := Index (Params, " ", First);
         Ada_Indent_Use := Integer'Value (Params (First .. Last - 1));

         First := Last + 1;
         Last := Index (Params, " ", First);
         Ada_Indent_When := Integer'Value (Params (First .. Last - 1));

         First := Last + 1;
         Last := Index (Params, " ", First);
         Ada_Indent_With := Integer'Value (Params (First .. Last - 1));

         First := Last + 1;
         Last := First + 1;
         Ada_Indent_Hanging_Rel_Exp := Params (First) = '1';

         First := Last + 1;
         Last := First + 1;
         End_Names_Optional := Params (First) = '1';
      end if;

      Data.Indent_Comment_Col_0 := Ada_Indent_Comment_Col_0;
   end Initialize;

   overriding function Insert_After
     (User_Data            : in out Parse_Data_Type;
      Tree                 : in     WisiToken.Syntax_Trees.Tree'Class;
      Token                : in     WisiToken.Valid_Node_Index;
      Insert_On_Blank_Line : in     Boolean)
     return Boolean
   is
      pragma Unreferenced (User_Data);
      use Ada_Annex_P_Process_Actions;

      --  We return True if Token affects indent (ie it is a block boundary)
      --  and normally has no code following it on the same line.
      --
      --  'end' is not really an exception, it is normally followed by
      --  <name> and ';', but no more code. Except when indenting a blank
      --  line; see test/ada_mode-interactive_2.adb Record_1.
      --
      --  RIGHT_PAREN is an exception; it is often followed by more code,
      --  but clearly belongs on the same line as the preceding token (often
      --  other ')').
      --
      --  COLON is similar to RIGHT_PAREN.

      ID : constant Token_ID := Tree.ID (Token);

      Result : constant array (Ada_Annex_P_Process_Actions.Token_Enum_ID) of Boolean :=
        (BEGIN_ID |         -- test/ada_mode-recover_exception_1.adb, test/ada_mode-recover_extra_declare.adb
           COLON_ID |       -- test/ada_mode-recover_partial_22.adb
           DECLARE_ID |
           RIGHT_PAREN_ID | -- test/ada_mode-recover_20.adb
           SEMICOLON_ID |   -- test/ada_mode-recover_13.adb
           THEN_ID          -- test/ada_mode-recover_19
                => True,
         others => False);
   begin
      case To_Token_Enum (ID) is
      when CASE_ID | IF_ID | LOOP_ID | RECORD_ID | RETURN_ID | SELECT_ID =>
         return -Tree.ID (Tree.Prev_Terminal (Token)) = END_ID;

      when END_ID =>
         --  test/ada_mode-recover_20.adb, test/ada_mode-interactive_2.adb Record_1.
         return not Insert_On_Blank_Line;

      when IDENTIFIER_ID =>
         return -Tree.ID (Tree.Prev_Terminal (Token)) in END_ID | COLON_ID;

      when others =>
         return Result (-ID);
      end case;
   end Insert_After;

   overriding
   procedure Refactor
     (Data       : in out Parse_Data_Type;
      Tree       : in out WisiToken.Syntax_Trees.Tree;
      Action     : in     Positive;
      Edit_Begin : in     WisiToken.Buffer_Pos)
   is
      --  Must match "ada-refactor-*" in ada-wisi.el
      Method_Object_To_Object_Method : constant Positive := 1;
      Object_Method_To_Method_Object : constant Positive := 2;
      Element_Object_To_Object_Index : constant Positive := 3;
      Object_Index_To_Element_Object : constant Positive := 4;
      Format_Parameter_List          : constant Positive := 5;

   begin
      if WisiToken.Trace_Action > Detail then
         Tree.Print_Tree (Data.Descriptor.all);
      end if;
      case Action is
      when Method_Object_To_Object_Method =>
         Wisi.Ada_Annex_P.Method_Object_To_Object_Method (Tree, Data, Edit_Begin);
      when Object_Method_To_Method_Object =>
         Wisi.Ada_Annex_P.Object_Method_To_Method_Object (Tree, Data, Edit_Begin);
      when Element_Object_To_Object_Index =>
         Wisi.Ada_Annex_P.Element_Object_To_Object_Index (Tree, Data, Edit_Begin);
      when Object_Index_To_Element_Object =>
         Wisi.Ada_Annex_P.Object_Index_To_Element_Object (Tree, Data, Edit_Begin);
      when Format_Parameter_List =>
         Wisi.Ada_Annex_P.Format_Parameter_List (Tree, Data, Edit_Begin);

      when others =>
         Standard.Ada.Text_IO.Put_Line ("(error ""unrecognized refactor action " & Action'Image & """)");
      end case;
   end Refactor;

   function Ada_Indent_Aggregate
     (Data              : in out Wisi.Parse_Data_Type'Class;
      Tree              : in     Syntax_Trees.Tree;
      Tokens            : in     Valid_Node_Index_Array;
      Tree_Indenting    : in     Valid_Node_Index;
      Indenting_Comment : in     Boolean;
      Args              : in     Wisi.Indent_Arg_Arrays.Vector)
     return Wisi.Delta_Type
   is
      pragma Unreferenced (Tree_Indenting);
      pragma Unreferenced (Tree);
      pragma Unreferenced (Data);
      pragma Unreferenced (Indenting_Comment);
      pragma Unreferenced (Args);
      pragma Unreferenced (Tokens);

      --  In our grammar, 'aggregate' can be an Ada aggregate, or a
      --  parenthesized expression.
      --
      --  We always want an 'aggregate' to be indented by ada-indent-broken.
      --  However, in some places in the grammar, 'aggregate' is indented by
      --  ada-indent. The following checks for those places, and returns a
      --  correction value. The aggregate may be nested inside a conditional
      --  expression, so we search for 'name' as well; see
      --  test/ada_mode-conditional_expressions-more_1.adb.

   begin
      --  FIXME: delete if not needed.
      return Null_Delta;
   end Ada_Indent_Aggregate;

   function Ada_Indent_Renames_0
     (Data              : in out Wisi.Parse_Data_Type'Class;
      Tree              : in     Syntax_Trees.Tree;
      Tokens            : in     Valid_Node_Index_Array;
      Tree_Indenting    : in     Valid_Node_Index;
      Indenting_Comment : in     Boolean;
      Args              : in     Indent_Arg_Arrays.Vector)
     return Wisi.Delta_Type
   is
      Subp_Node   : constant Valid_Node_Index := Tokens (Positive_Index_Type (Integer'(Args (1))));
      Subp_Tok    : Aug_Token_Const_Ref renames Get_Aug_Token_Const_1 (Tree, Subp_Node);
      Renames_Tok : Aug_Token_Const_Ref renames Get_Aug_Token_Const_1 (Tree, Tree_Indenting);
      Paren_I     : Node_Index;
   begin
      Paren_I := Tree.Find_Descendant (Subp_Node, Data.Left_Paren_ID);

      if Paren_I /= Invalid_Node_Index then
         --  paren is present
         if Ada_Indent_Renames > 0 then
            return Indent_Anchored_2
              (Data,
               Anchor_Line => Subp_Tok.Line,
               Last_Line   => Renames_Tok.Last_Line (Indenting_Comment),
               Offset      => Ada_Indent_Renames,
               Accumulate  => True);
         else
            declare
               Paren_Tok : Aug_Token_Const_Ref renames Get_Aug_Token_Const_1 (Tree, Paren_I);
            begin
               return Indent_Anchored_2
                 (Data,
                  Anchor_Line => Paren_Tok.Line,
                  Last_Line   => Renames_Tok.Last_Line (Indenting_Comment),
                  Offset      => Current_Indent_Offset (Data, Paren_Tok, abs Ada_Indent_Renames),
                  Accumulate  => True);
            end;
         end if;
      else
         return Indent_Anchored_2
           (Data,
            Anchor_Line => Subp_Tok.Line,
            Last_Line   => Renames_Tok.Last_Line (Indenting_Comment),
            Offset      => Ada_Indent_Broken,
            Accumulate  => True);
      end if;
   end Ada_Indent_Renames_0;

   function Ada_Indent_Return_0
     (Data              : in out Wisi.Parse_Data_Type'Class;
      Tree              : in     Syntax_Trees.Tree;
      Tokens            : in     Valid_Node_Index_Array;
      Tree_Indenting    : in     Valid_Node_Index;
      Indenting_Comment : in     Boolean;
      Args              : in     Wisi.Indent_Arg_Arrays.Vector)
     return Wisi.Delta_Type
   is
      use all type Ada_Annex_P_Process_Actions.Token_Enum_ID;
      --  Tokens (Args (1)) = 'formal_part'
      --  Indenting = 'result_profile'
      --  Args (2) = delta (= 0!)
      --
      --  We are indenting 'result_profile' in
      --  'parameter_and_result_profile'. The indent depends on whether the
      --  'formal_part' is present, and the location of 'FUNCTION'.

      Parameter_And_Result_Profile : constant Valid_Node_Index := Tree.Parent (Tree_Indenting);

      Indenting : Aug_Token_Const_Ref renames Get_Aug_Token_Const_1 (Tree, Tree_Indenting);
   begin
      if Indenting.Line = Indenting.First_Indent_Line then
         if Ada_Indent_Return <= 0 then
            declare
               Anchor_Token : Aug_Token_Const_Ref renames Get_Aug_Token_Const_1
                 (Tree, Tokens (Positive_Index_Type (Integer'(Args (1)))));
            begin
               return Indent_Anchored_2
                 (Data,
                  Anchor_Line => Anchor_Token.Line,
                  Last_Line   => Indenting.Last_Line (Indenting_Comment),
                  Offset      => Current_Indent_Offset (Data, Anchor_Token, Args (2) + abs Ada_Indent_Return),
                  Accumulate  => True);
            end;
         else
            declare
               Function_N   : constant Valid_Node_Index := Tree.Find_Sibling
                 (Parameter_And_Result_Profile, +FUNCTION_ID);
               Anchor_Token : Aug_Token_Const_Ref renames Get_Aug_Token_Const_1 (Tree, Function_N);
            begin
               return Indent_Anchored_2
                 (Data,
                  Anchor_Line => Anchor_Token.Line,
                  Last_Line   => Indenting.Last_Line (Indenting_Comment),
                  Offset      => Current_Indent_Offset (Data, Anchor_Token, Args (2) + abs Ada_Indent_Return),
                  Accumulate  => True);
            end;
         end if;

      else
         return Null_Delta;
      end if;
   end Ada_Indent_Return_0;

   function Ada_Indent_Record_0
     (Data              : in out Wisi.Parse_Data_Type'Class;
      Tree              : in     Syntax_Trees.Tree;
      Tokens            : in     Valid_Node_Index_Array;
      Tree_Indenting    : in     Valid_Node_Index;
      Indenting_Comment : in     Boolean;
      Args              : in     Wisi.Indent_Arg_Arrays.Vector)
     return Wisi.Delta_Type
   is begin
      return Indent_Record
        (Parse_Data_Type (Data),
         Tree,
         Anchor_Token      => Get_Aug_Token_Const_1 (Tree, Tokens (Positive_Index_Type (Integer'(Args (1))))),
         Record_Token      => Get_Aug_Token_Const_1 (Tree, Tokens (Positive_Index_Type (Integer'(Args (2))))),
         Offset            => Args (3),
         Indenting_Token   => Get_Aug_Token_Const_1 (Tree, Tree_Indenting),
         Indenting_Comment => Indenting_Comment);
   end Ada_Indent_Record_0;

   function Ada_Indent_Record_1
     (Data              : in out Wisi.Parse_Data_Type'Class;
      Tree              : in     Syntax_Trees.Tree;
      Tokens            : in     Valid_Node_Index_Array;
      Tree_Indenting    : in     Valid_Node_Index;
      Indenting_Comment : in     Boolean;
      Args              : in     Wisi.Indent_Arg_Arrays.Vector)
     return Wisi.Delta_Type
   is
      --  We are indenting a token in record_definition or
      --  record_representation_clause, or a comment before 'record'.
      --
      --  Args (1) is the token ID of the anchor. If record_definition, this
      --  is TYPE; it appears as a direct child in an ancestor
      --  full_type_declaration. If record_representation_clause, args (1)
      --  is FOR, child of record_representation_clause.

      use Ada_Annex_P_Process_Actions;

      Anchor : constant Token_ID := Token_ID (Integer'(Args (1)));

      Declaration : constant Valid_Node_Index := Tree.Find_Ancestor
        (Tree_Indenting,
         (if To_Token_Enum (Anchor) = TYPE_ID
          then +full_type_declaration_ID
          else +record_representation_clause_ID));

      Tree_Anchor : constant Valid_Node_Index := Tree.Find_Child (Declaration, Anchor);

      --  Args (2) is the index of RECORD (or a nonterminal possibly
      --  starting with RECORD) in Tokens
      Record_Token_Tree_Index : constant Node_Index := Tokens (Positive_Index_Type (Integer'(Args (2))));
   begin
      --  Args (3) is the offset
      return Indent_Record
        (Parse_Data_Type (Data),
         Tree,
         Anchor_Token      => Get_Aug_Token_Const_1 (Tree, Tree_Anchor),
         Record_Token      => Get_Aug_Token_Const_1 (Tree, Tree.First_Terminal (Record_Token_Tree_Index)),
         Indenting_Token   => Get_Aug_Token_Const_1 (Tree, Tree_Indenting),
         Indenting_Comment => Indenting_Comment,
         Offset            => Args (3));
   end Ada_Indent_Record_1;

   function Ada_Indent_Anchored_Expression
     (Data              : in out Wisi.Parse_Data_Type'Class;
      Tree              : in     Syntax_Trees.Tree;
      Tokens            : in     Valid_Node_Index_Array;
      Tree_Indenting    : in     Valid_Node_Index;
      Indenting_Comment : in     Boolean;
      Args              : in     Wisi.Indent_Arg_Arrays.Vector)
     return Wisi.Delta_Type
   is
      Anchor : constant SAL.Base_Peek_Type := SAL.Base_Peek_Type (Args.Element (1));

      Param : constant Indent_Param :=
        (Hanging_3,
         Hanging_Delta_1 => (Anchored_1, Anchor, Ada_Indent_Broken),
         Hanging_Delta_2 => (Anchored_1, Anchor, 2 * Ada_Indent_Broken));
   begin
      return Indent_Compute_Delta (Data, Tree, Tokens, Param, Tree_Indenting, Indenting_Comment);
   end Ada_Indent_Anchored_Expression;

end Wisi.Ada_Annex_P;
--  Local Variables:
--  ada-case-strict: nil
--  End:
