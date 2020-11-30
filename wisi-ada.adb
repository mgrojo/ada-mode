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
package body Wisi.Ada is
   use WisiToken;

   ----------
   --  body local subprograms

   function Indent_Record
     (Data                   : in out Parse_Data_Type;
      Tree                   : in     Syntax_Trees.Tree;
      Controlling_Token_Line : in     Line_Number_Type;
      Anchor_Token           : in     Base_Token;
      Record_Token           : in     Augmented_Token;
      Indenting_Token        : in     Augmented_Token;
      Indenting_Comment      : in     Boolean;
      Offset                 : in     Integer)
     return Wisi.Delta_Type
   is
      use Ada_Annex_P_Process_Actions;
   begin
      if not Indenting_Comment and Indenting_Token.Base.ID = +RECORD_ID then
         --  Indenting 'record'
         return Indent_Anchored_2
           (Data, Anchor_Token.Line, Last_Line (Record_Token, Indenting_Comment),
            Ada_Indent_Record_Rel_Type);

      elsif Indenting_Comment and Indenting_Token.Base.ID = +WITH_ID then
         --  comment before 'record'. test/ada_mode-nominal-child.ads Child_Type_1
         return Indent_Anchored_2
           (Data, Anchor_Token.Line, Last_Line (Indenting_Token, Indenting_Comment),
            Ada_Indent_Record_Rel_Type);

      elsif Indenting_Comment and Indenting_Token.Base.ID = +IS_ID then
         --  comment after 'is'
         if Record_Token.Base.ID = +RECORD_ID then
            --  before 'record'. test/ada_mode-nominal.ads Record_Type_1
            return Indent_Anchored_2
              (Data, Anchor_Token.Line, Last_Line (Indenting_Token, Indenting_Comment),
               Ada_Indent_Record_Rel_Type);
         else
            --  not before 'record'. test/ada_mode-nominal-child.ads Child_Type_1
            return (Simple, (Int, Controlling_Token_Line, Offset));
         end if;

      else
         --  Indenting comment after 'record', other comment, component or 'end'
         --
         --  Ensure 'record' line is anchored to Anchor_Token.
         if not (Data.Indents (Record_Token.Base.Line).Label = Anchored or
                   Data.Indents (Record_Token.Base.Line).Label = Anchor_Anchored)
         then
            if Anchor_Token.Line /= Record_Token.Base.Line then
               --  We don't pass Indenting_Comment here, because 'record' is code.
               Indent_Token_1
                 (Data,
                  Tree,
                  Indenting_Token         => Record_Token,
                  Delta_Indent            => Indent_Anchored_2
                    (Data, Anchor_Token.Line,
                     Last_Line
                       (Record_Token,
                        Indenting_Comment => False),
                     Ada_Indent_Record_Rel_Type),
                  Indenting_Comment       => False);
            end if;
         end if;

         return Indent_Anchored_2
           (Data,
            Anchor_Line => Anchor_Token.Line,
            Last_Line   => Last_Line (Indenting_Token, Indenting_Comment),
            Offset      => Current_Indent_Offset
              (Tree, Anchor_Token,
               Offset   =>
                 (if Anchor_Token.Line = Record_Token.Base.Line
                  then Offset
                  else Offset + Ada_Indent_Record_Rel_Type)));
      end if;
   end Indent_Record;

   ----------
   --  Refactor body subprograms

   function Find_ID_At
     (Tree       : in WisiToken.Syntax_Trees.Tree;
      ID         : in Token_ID;
      Edit_Begin : in WisiToken.Buffer_Pos)
     return WisiToken.Syntax_Trees.Node_Access
   is
      function Match (Tree : in Syntax_Trees.Tree; Node : in Syntax_Trees.Valid_Node_Access) return Boolean
        is (Tree.ID (Node) = ID and then Tree.Byte_Region (Node).First = Edit_Begin);
   begin
      return Tree.Find_Descendant (Tree.Root, Predicate => Match'Access);
   end Find_ID_At;

   procedure Unrecognized
     (Expecting : in     String;
      Tree      : in     WisiToken.Syntax_Trees.Tree;
      Data      : in out Parse_Data_Type;
      Found     : in     WisiToken.Syntax_Trees.Valid_Node_Access)
   with No_Return
   is begin
      raise SAL.Parameter_Error with "expecting '" & Expecting & "'; found " &
        Tree.Image (Found, Node_Numbers => True) & " '" & Elisp_Escape_Quotes (Data.Get_Text (Tree, Found)) & "'";
   end Unrecognized;

   procedure Method_Object_To_Object_Method
     (Tree       : in     WisiToken.Syntax_Trees.Tree;
      Data       : in out Parse_Data_Type;
      Edit_Begin : in     WisiToken.Buffer_Pos)
   --  Data.Tree contains one statement or declaration; Edit_Begin is at
   --  start of a subprogram call. Convert the subprogram call from
   --  Prefix.Method (Object, ...) to Object.Method (...).
   is
      use Ada_Annex_P_Process_Actions;
      use Standard.Ada.Strings.Unbounded;
      use Standard.Ada.Text_IO;
      use WisiToken.Syntax_Trees;

      Call : constant Node_Access := Find_ID_At (Tree, +function_call_ID, Edit_Begin);

      Edit_End              : WisiToken.Buffer_Pos;
      Actual_Parameter_Part : Node_Access;
      Association_List      : Node_Access;
      Method                : Node_Access;

   begin
      if Call = Invalid_Node_Access then
         --  Most likely the edit point is wrong.
         raise SAL.Parameter_Error with "no subprogram call found at byte_pos" & Edit_Begin'Image;
      end if;

      if WisiToken.Trace_Action > Detail then
         Put_Line
           (";; refactoring node " & Tree.Image (Call, Node_Numbers => True) & " '" & Data.Get_Text (Tree, Call) & "'");
      end if;

      Actual_Parameter_Part := Tree.Child (Call, 2);
      pragma Assert (Tree.ID (Actual_Parameter_Part) = +actual_parameter_part_ID);

      Association_List := Tree.Child (Actual_Parameter_Part, 2);
      pragma Assert (Tree.ID (Association_List) = +parameter_association_list_ID);

      Edit_End := Tree.Byte_Region (Call).Last;

      Method := Tree.Child (Tree.Child (Call, 1), 1);

      case To_Token_Enum (Tree.ID (Method)) is
      when selected_component_ID =>
         Method := Tree.Child (Method, 3);
         pragma Assert (Tree.ID (Method) = +selector_name_ID);

         Method := Tree.Child (Method, 1);

      when attribute_reference_ID =>
         case To_Token_Enum (Tree.ID (Tree.Child (Method, 1))) is
         when name_ID =>
            Method := Tree.Child (Method, 3);
            pragma Assert (Tree.ID (Method) = +attribute_designator_ID);

            Method := Tree.Child (Method, 1);

         when reduction_attribute_reference_ID =>
            Unrecognized ("subprogram call", Tree, Data, Method);

         when others =>
            raise SAL.Programmer_Error;
         end case;

      when qualified_expression_ID =>
         raise SAL.Not_Implemented; -- need use case

      when direct_name_ID =>
         Method := Tree.Child (Method, 1);

      when others =>
         Unrecognized ("supported token", Tree, Data, Method);
      end case;

      pragma Assert (To_Token_Enum (Tree.ID (Method)) in IDENTIFIER_ID | STRING_LITERAL_ID);

      declare
         Object : constant Node_Access := Tree.Find_Descendant (Association_List, +expression_ID);
         Result : Unbounded_String;
      begin
         if Object = Invalid_Node_Access then
            Unrecognized ("expression", Tree, Data, Association_List);
         end if;

         --  Build remaining arg list in Result.
         loop
            if Tree.RHS_Index (Association_List) = 1 then
               Result := Get_Text (Data, Tree, Tree.Child (Association_List, 3)) &
                 (if Length (Result) = 0 then "" else ", ") &
                 Result;
               Association_List := Tree.Child (Association_List, 1);
            else
               --  The remaining element in Association_List is the first one, which is Object.
               if Length (Result) > 0 then
                  Result := " (" & Result & ")";
               end if;
               exit;
            end if;
         end loop;
         Result := (Get_Text (Data, Tree, Object) & "." & Get_Text (Data, Tree, Method)) & Result;
         Put_Line ("[" & Edit_Action_Code & Edit_Begin'Image & Edit_End'Image & " """ &
                     Elisp_Escape_Quotes (To_String (Result)) & """]");
      end;
   end Method_Object_To_Object_Method;

   procedure Object_Method_To_Method_Object
     (Tree       : in     WisiToken.Syntax_Trees.Tree;
      Data       : in out Parse_Data_Type;
      Edit_Begin : in     WisiToken.Buffer_Pos)
   is
      --  Data.Tree contains one statement or declaration; Edit_Begin is at
      --  start of a subprogram call. Convert the subprogram call from
      --  Object.Method (...) to Method (Object, ...).
      use Ada_Annex_P_Process_Actions;
      use Standard.Ada.Strings.Unbounded;
      use Standard.Ada.Text_IO;
      use WisiToken.Syntax_Trees;

      Call          : Node_Access := Find_ID_At (Tree, +name_ID, Edit_Begin);
      Edit_End      : WisiToken.Buffer_Pos;
      Object_Method : Node_Access;
      Args          : Node_Access := Invalid_Node_Access;
      Method        : Unbounded_String;
      Object        : Unbounded_String;
      Result        : Unbounded_String;
   begin
      if Call = Invalid_Node_Access then
         --  Most likely the edit point is wrong.
         raise SAL.Parameter_Error with "no 'name' at byte_pos" & Edit_Begin'Image;
      end if;

      if WisiToken.Trace_Action > Detail then
         Put_Line
           (";; refactoring node " & Tree.Image (Call, Node_Numbers => True) & " '" & Data.Get_Text (Tree, Call) & "'");
      end if;

      if Tree.ID (Tree.Child (Call, 1)) = +attribute_reference_ID then
         --  Code looks like: Container.Length'Old. We only want to edit
         --  'Container.Length', keeping the trailing 'Old.
         Call := Tree.Child (Tree.Child (Call, 1), 1);
      end if;

      Edit_End      := Tree.Byte_Region (Call).Last;
      Object_Method := Tree.Child (Call, 1);
      loop
         case To_Token_Enum (Tree.ID (Object_Method)) is
         when function_call_ID =>
            --  Object_Method looks like:
            --  Object.Method (Args)
            --  test/ada_mode-refactor_object_method_to_method_object.adb
            Args := Tree.Child (Tree.Child (Object_Method, 2), 2);
            pragma Assert (Tree.ID (Args) = +parameter_association_list_ID);

            Object_Method := Tree.Child (Object_Method, 1);

         when name_ID =>
            Object_Method := Tree.Child (Object_Method, 1);

         when selected_component_ID =>
            Object := +Get_Text (Data, Tree, Tree.Child (Object_Method, 1));
            Method := +Get_Text (Data, Tree, Tree.Child (Object_Method, 3));
            exit;

         when others =>
            Unrecognized ("supported token", Tree, Data, Object_Method);
         end case;
      end loop;

      Result := Method & " (" & Object;
      if Args /= Invalid_Node_Access then
         Result := Result & ", " & Get_Text (Data, Tree, Args);
      end if;
      Result := Result & ")";
      Put_Line ("[" & Edit_Action_Code & Edit_Begin'Image & Edit_End'Image & " """ &
                  Elisp_Escape_Quotes (To_String (Result)) & """]");
   end Object_Method_To_Method_Object;

   procedure Element_Object_To_Object_Index
     (Tree       : in     WisiToken.Syntax_Trees.Tree;
      Data       : in out Parse_Data_Type;
      Edit_Begin : in     WisiToken.Buffer_Pos)
   --  Data.Tree contains one statement or declaration; Edit_Begin is at
   --  start of a subprogram call. Convert the subprogram call from
   --  Prefix.Element (Object, Index) to Object (Index).
   is
      use Ada_Annex_P_Process_Actions;
      use Standard.Ada.Text_IO;
      use WisiToken.Syntax_Trees;

      Call             : constant Node_Access := Find_ID_At (Tree, +function_call_ID, Edit_Begin);
      Edit_End         : WisiToken.Buffer_Pos;
      Temp             : Node_Access;
      Association_List : Node_Access;
      Object           : Node_Access;
      Index            : Node_Access;
   begin
      if Call = Invalid_Node_Access then
         --  Most likely the edit point is wrong.
         raise SAL.Parameter_Error with "no subprogram call found at byte_pos" & Edit_Begin'Image;
      end if;

      if WisiToken.Trace_Action > Detail then
         Put_Line
           (";; refactoring node " & Tree.Image (Call, Node_Numbers => True) & " '" & Data.Get_Text (Tree, Call) & "'");
      end if;

      Association_List := Tree.Child (Tree.Child (Call, 2), 2);
      pragma Assert (Tree.ID (Association_List) = +parameter_association_list_ID);

      Edit_End := Tree.Byte_Region (Call).Last;

      if Tree.RHS_Index (Association_List) /= 1 then
         Unrecognized ("two args", Tree, Data, Association_List);
      end if;

      Temp := Tree.Find_Descendant (Association_List, +expression_ID);
      if Temp = Invalid_Node_Access then
         Unrecognized ("expression", Tree, Data, Association_List);
      else
         Object := Temp;
      end if;

      Temp := Tree.Find_Descendant (Tree.Child (Association_List, 3), +expression_ID);
      if Temp = Invalid_Node_Access then
         Unrecognized ("expression", Tree, Data, Association_List);
      else
         Index := Temp;
      end if;

      Put_Line
        ("[" & Edit_Action_Code & Edit_Begin'Image & Edit_End'Image & " """ &
           Elisp_Escape_Quotes (Get_Text (Data, Tree, Object) & " (" & Get_Text (Data, Tree, Index) & ")") &
           """]");
   end Element_Object_To_Object_Index;

   procedure Object_Index_To_Element_Object
     (Tree       : in     WisiToken.Syntax_Trees.Tree;
      Data       : in out Parse_Data_Type;
      Edit_Begin : in     WisiToken.Buffer_Pos)
   --  Data.Tree contains one statement or declaration; Edit_Begin is at
   --  start of a subprogram call. Convert the subprogram call from
   --  Object (Index) to Element (Object, Index).
   is
      use Ada_Annex_P_Process_Actions;
      use Standard.Ada.Text_IO;
      use WisiToken.Syntax_Trees;

      Call             : constant Node_Access := Find_ID_At (Tree, +function_call_ID, Edit_Begin);
      Edit_End         : WisiToken.Buffer_Pos;
      Temp             : Node_Access;
      Association_List : Node_Access;
      Object           : Node_Access;
      Index            : Node_Access;
   begin
      if Call = Invalid_Node_Access then
         --  Most likely the edit point is wrong.
         raise SAL.Parameter_Error with "no subprogram_call found at byte_pos" & Edit_Begin'Image;
      end if;

      if WisiToken.Trace_Action > Detail then
         Put_Line
           (";; refactoring node " & Tree.Image (Call, Node_Numbers => True) & " '" & Data.Get_Text (Tree, Call) & "'");
      end if;

      Object := Tree.Child (Call, 1);
      pragma Assert (Tree.ID (Object) = +name_ID);

      Association_List := Tree.Child (Tree.Child (Call, 2), 2);
      pragma Assert (Tree.ID (Association_List) = +parameter_association_list_ID);

      Edit_End := Tree.Byte_Region (Call).Last;

      if Tree.RHS_Index (Association_List) /= 0 then
         Unrecognized ("one arg", Tree, Data, Association_List);
      end if;

      Temp := Tree.Find_Descendant (Tree.Child (Association_List, 1), +expression_ID);
      if Temp = Invalid_Node_Access then
         Unrecognized ("expression", Tree, Data, Association_List);
      else
         Index := Temp;
      end if;

      Put_Line
        ("[" & Edit_Action_Code & Edit_Begin'Image & Edit_End'Image & " """ &
           Elisp_Escape_Quotes
             ("Element (" & Get_Text (Data, Tree, Object) & ", " & Get_Text (Data, Tree, Index) & ")") &
           """]");
   end Object_Index_To_Element_Object;

   procedure Format_Parameter_List
     (Tree       : in out WisiToken.Syntax_Trees.Tree;
      Edit_Begin : in     WisiToken.Buffer_Pos)
   is separate;
   --  Tree contains a subprogram declaration or body; Edit_Begin is
   --  at the start of the parameter list. Format the parameter list.
   --
   --  Handle virtual tokens as much as possible; at least closing paren.

   ----------
   --  Public subprograms, declaration order

   overriding
   procedure Initialize
     (Data              : in out Parse_Data_Type;
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
      Wisi.Initialize (Wisi.Parse_Data_Type (Data), Post_Parse_Action, Begin_Line, End_Line, Begin_Indent, "");

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
         Last := Index (Params, " ", First);
         Ada_Indent_Subprogram_Is := Integer'Value (Params (First .. Last - 1));

         First := Last + 1;
         Last := First + 1;
         End_Names_Optional := Params (First) = '1';
      end if;

      Data.Indent_Comment_Col_0 := Ada_Indent_Comment_Col_0;
   end Initialize;

   overriding function Insert_After
     (User_Data            : in out Parse_Data_Type;
      Tree                 : in     WisiToken.Syntax_Trees.Tree'Class;
      Insert_Token         : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Insert_Before_Token  : in     WisiToken.Syntax_Trees.Valid_Node_Access;
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

      Insert_ID        : constant Token_ID := Tree.ID (Insert_Token);
      Insert_Before_ID : constant Token_ID := Tree.ID (Insert_Before_Token);

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
      case To_Token_Enum (Insert_Before_ID) is
      when BEGIN_ID | DECLARE_ID | PACKAGE_ID | PROCEDURE_ID | FUNCTION_ID |
        END_ID =>
         --  test/ada_mode-interactive_2.adb Record_1: BEGIN_ID
         --  test/ada_mode-recover_03.adb: BEGIN_ID
         --  test/ada_mode-recover_37.adb: END_ID
         return not Insert_On_Blank_Line;

      when others =>
         case To_Token_Enum (Insert_ID) is
         when CASE_ID | IF_ID | LOOP_ID | RECORD_ID | RETURN_ID | SELECT_ID =>
            return -Tree.ID (Tree.Prev_Terminal (Insert_Token)) = END_ID;

         when END_ID =>
            --  test/ada_mode-recover_20.adb, test/ada_mode-interactive_2.adb Record_1.
            return not Insert_On_Blank_Line;

         when IDENTIFIER_ID =>
            return -Tree.ID (Tree.Prev_Terminal (Insert_Token)) in END_ID | COLON_ID;

         when others =>
            return Result (-Insert_ID);
         end case;
      end case;
   end Insert_After;

   overriding
   procedure Refactor_Help (Data : in Parse_Data_Type)
   is
      use Standard.Ada.Text_IO;
   begin
      --  Must match "ada-refactor-*" in ada-wisi.el
      Put ("1 Method_Object_To_Object_Method");
      Put ("2 Object_Method_To_Method_Object");
      Put ("3 Element_Object_To_Object_Index");
      Put ("4 Object_Index_To_Element_Object");
      Put ("5 Format_Parameter_List         ");
   end Refactor_Help;

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
      if WisiToken.Trace_Action > Extra then
         Tree.Print_Tree;
      end if;
      case Action is
      when Method_Object_To_Object_Method =>
         Wisi.Ada.Method_Object_To_Object_Method (Tree, Data, Edit_Begin);
      when Object_Method_To_Method_Object =>
         Wisi.Ada.Object_Method_To_Method_Object (Tree, Data, Edit_Begin);
      when Element_Object_To_Object_Index =>
         Wisi.Ada.Element_Object_To_Object_Index (Tree, Data, Edit_Begin);
      when Object_Index_To_Element_Object =>
         Wisi.Ada.Object_Index_To_Element_Object (Tree, Data, Edit_Begin);
      when Format_Parameter_List =>
         Wisi.Ada.Format_Parameter_List (Tree, Edit_Begin);

      when others =>
         Standard.Ada.Text_IO.Put_Line ("(error ""unrecognized refactor action " & Action'Image & """)");
      end case;
   end Refactor;

   function Ada_Indent_Aggregate
     (Data              : in out Wisi.Parse_Data_Type'Class;
      Tree              : in     Syntax_Trees.Tree;
      Nonterm           : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Tokens            : in     Syntax_Trees.Valid_Node_Access_Array;
      Tree_Indenting    : in     Syntax_Trees.Valid_Node_Access;
      Indenting_Comment : in     Boolean;
      Args              : in     Wisi.Indent_Arg_Arrays.Vector)
     return Wisi.Delta_Type
   is
      pragma Unreferenced (Nonterm);
      pragma Unreferenced (Data);
      pragma Unreferenced (Indenting_Comment);
      pragma Unreferenced (Args);
      pragma Unreferenced (Tokens);

      use Ada_Annex_P_Process_Actions;
      use Syntax_Trees;

      --  In our grammar, 'aggregate' can be an Ada aggregate, or a
      --  parenthesized expression.
      --
      --  We always want an 'aggregate' to be indented by ada-indent-broken.
      --  However, in some places in the grammar, 'aggregate' is indented by
      --  ada-indent. The following checks for those places, and returns a
      --  correction value. The aggregate may be nested inside a conditional
      --  expression, so we search for 'name' as well; see
      --  test/ada_mode-conditional_expressions-more_1.adb.

      Expression : constant Node_Access := Tree.Find_Ancestor (Tree_Indenting, (+expression_ID, +name_ID));
   begin
      if Expression = Invalid_Node_Access or else
        Tree.Parent (Expression) = Invalid_Node_Access
      then
         return Null_Delta;
      elsif Tree.ID (Tree.Parent (Expression)) in +if_expression_ID | +elsif_expression_item_ID |
        +case_expression_alternative_ID
      then
         --  test/ada_mode-conditional_expressions.adb K; value expression in
         --  if_expression is indented by ada-indent. Invalid
         --  Controlling_Token_Line, so this correction is always added.
         return (Simple, (Int, Invalid_Line_Number, Ada_Indent_Broken - Ada_Indent));
      else
         return Null_Delta;
      end if;
   end Ada_Indent_Aggregate;

   function Ada_Indent_Aspect
     (Data              : in out Wisi.Parse_Data_Type'Class;
      Tree              : in     Syntax_Trees.Tree;
      Nonterm           : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Tokens            : in     Syntax_Trees.Valid_Node_Access_Array;
      Tree_Indenting    : in     Syntax_Trees.Valid_Node_Access;
      Indenting_Comment : in     Boolean;
      Args              : in     Wisi.Indent_Arg_Arrays.Vector)
     return Delta_Type
   is
      pragma Unreferenced (Nonterm, Tokens, Args);

      use all type SAL.Base_Peek_Type;
      use Ada_Annex_P_Process_Actions;
      use all type WisiToken.Syntax_Trees.Node_Access;

      pragma Assert (Tree.ID (Tree_Indenting) = +aspect_definition_ID);

      Anchor_Token    : constant WisiToken.Base_Token := Tree.Base_Token (Tree.Child (Tree.Parent (Tree_Indenting), 2));
      Indenting_Token : constant Augmented_Token      := Get_Augmented_Token (Tree, Tree_Indenting);
   begin
      if Indenting_Token.Base.Line = Indenting_Token.Aug.First_Indent_Line then
         --  aspect_definition starts a line; anchor the aspect_definition to
         --  the line containing '=>' with offset ada_indent_broken.
         return
           (Simple,
            Indent_Anchored_2
              (Data,
               Anchor_Line => Anchor_Token.Line,
               Last_Line   => Last_Line (Indenting_Token, Indenting_Comment),
               Offset      => Ada_Indent_Broken).Simple_Delta);
      else
         --  aspect_definition starts on same line as '=>'; anchor the aspect_definition to '=>' with offset 3
         declare
            Offset : constant Integer := Current_Indent_Offset (Tree, Anchor_Token, 3);
         begin
            return
              (Simple,
               Indent_Anchored_2
                 (Data,
                  Anchor_Line => Anchor_Token.Line,
                  Last_Line   => Last_Line (Indenting_Token, Indenting_Comment),
                  Offset      => Offset).Simple_Delta);
         end;
      end if;
   end Ada_Indent_Aspect;

   function Ada_Indent_Renames_0
     (Data              : in out Wisi.Parse_Data_Type'Class;
      Tree              : in     Syntax_Trees.Tree;
      Nonterm           : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Tokens            : in     Syntax_Trees.Valid_Node_Access_Array;
      Tree_Indenting    : in     Syntax_Trees.Valid_Node_Access;
      Indenting_Comment : in     Boolean;
      Args              : in     Indent_Arg_Arrays.Vector)
     return Wisi.Delta_Type
   is
      pragma Unreferenced (Nonterm);
      use all type WisiToken.Syntax_Trees.Node_Access;

      Subp_Node   : constant Syntax_Trees.Valid_Node_Access := Tokens (Positive_Index_Type (Integer'(Args (1))));
      Subp_Tok    : constant WisiToken.Base_Token           := Tree.Base_Token (Subp_Node);
      Renames_Tok : constant Augmented_Token                := Get_Augmented_Token (Tree, Tree_Indenting);
      Paren_I     : Syntax_Trees.Node_Access;
   begin
      Paren_I := Tree.Find_Descendant (Subp_Node, Data.Left_Paren_ID);

      if Paren_I /= Syntax_Trees.Invalid_Node_Access then
         --  paren is present
         if Ada_Indent_Renames > 0 then
            return Indent_Anchored_2
              (Data,
               Anchor_Line => Subp_Tok.Line,
               Last_Line   => Last_Line (Renames_Tok, Indenting_Comment),
               Offset      => Ada_Indent_Renames);
         else
            declare
               Paren_Tok : constant WisiToken.Base_Token :=  Tree.Base_Token (Paren_I);
            begin
               return Indent_Anchored_2
                 (Data,
                  Anchor_Line => Paren_Tok.Line,
                  Last_Line   => Last_Line (Renames_Tok, Indenting_Comment),
                  Offset      => Current_Indent_Offset (Tree, Paren_Tok, abs Ada_Indent_Renames));
            end;
         end if;
      else
         return Indent_Anchored_2
           (Data,
            Anchor_Line => Subp_Tok.Line,
            Last_Line   => Last_Line (Renames_Tok, Indenting_Comment),
            Offset      => Ada_Indent_Broken);
      end if;
   end Ada_Indent_Renames_0;

   function Ada_Indent_Return_0
     (Data              : in out Wisi.Parse_Data_Type'Class;
      Tree              : in     Syntax_Trees.Tree;
      Nonterm           : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Tokens            : in     Syntax_Trees.Valid_Node_Access_Array;
      Tree_Indenting    : in     Syntax_Trees.Valid_Node_Access;
      Indenting_Comment : in     Boolean;
      Args              : in     Wisi.Indent_Arg_Arrays.Vector)
     return Wisi.Delta_Type
   is
      pragma Unreferenced (Nonterm);
      use all type Ada_Annex_P_Process_Actions.Token_Enum_ID;
      --  Tokens (Args (1)) = 'formal_part'
      --  Indenting = 'result_profile'
      --  Args (2) = delta (= 0!)
      --
      --  We are indenting 'result_profile' in
      --  'parameter_and_result_profile'. The indent depends on whether the
      --  'formal_part' is present, and the location of 'FUNCTION'.

      Parameter_And_Result_Profile : constant Syntax_Trees.Valid_Node_Access := Tree.Parent (Tree_Indenting);

      Indenting : constant Augmented_Token := Get_Augmented_Token (Tree, Tree_Indenting);
   begin
      if Indenting.Base.Line = Indenting.Aug.First_Indent_Line then
         if Ada_Indent_Return <= 0 then
            declare
               Anchor_Token : constant Base_Token := Tree.Base_Token
                 (Tokens (Positive_Index_Type (Integer'(Args (1)))));
            begin
               return Indent_Anchored_2
                 (Data,
                  Anchor_Line => Anchor_Token.Line,
                  Last_Line   => Last_Line (Indenting, Indenting_Comment),
                  Offset      => Current_Indent_Offset (Tree, Anchor_Token, Args (2) + abs Ada_Indent_Return));
            end;
         else
            declare
               Function_N   : constant Syntax_Trees.Valid_Node_Access := Tree.Find_Sibling
                 (Parameter_And_Result_Profile, +FUNCTION_ID);
               Anchor_Token : constant Base_Token := Tree.Base_Token (Function_N);
            begin
               return Indent_Anchored_2
                 (Data,
                  Anchor_Line => Anchor_Token.Line,
                  Last_Line   => Last_Line (Indenting, Indenting_Comment),
                  Offset      => Current_Indent_Offset (Tree, Anchor_Token, Args (2) + abs Ada_Indent_Return));
            end;
         end if;

      else
         return Null_Delta;
      end if;
   end Ada_Indent_Return_0;

   function Ada_Indent_Record_0
     (Data              : in out Wisi.Parse_Data_Type'Class;
      Tree              : in     Syntax_Trees.Tree;
      Nonterm           : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Tokens            : in     Syntax_Trees.Valid_Node_Access_Array;
      Tree_Indenting    : in     Syntax_Trees.Valid_Node_Access;
      Indenting_Comment : in     Boolean;
      Args              : in     Wisi.Indent_Arg_Arrays.Vector)
     return Wisi.Delta_Type
   is begin
      return Indent_Record
        (Parse_Data_Type (Data),
         Tree,
         Tree.Base_Token (Nonterm).Line,
         Anchor_Token      => Tree.Base_Token (Tokens (Positive_Index_Type (Integer'(Args (1))))),
         Record_Token      => Get_Augmented_Token (Tree, Tokens (Positive_Index_Type (Integer'(Args (2))))),
         Offset            => Args (3),
         Indenting_Token   => Get_Augmented_Token (Tree, Tree_Indenting),
         Indenting_Comment => Indenting_Comment);
   end Ada_Indent_Record_0;

   function Ada_Indent_Record_1
     (Data              : in out Wisi.Parse_Data_Type'Class;
      Tree              : in     Syntax_Trees.Tree;
      Nonterm           : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Tokens            : in     Syntax_Trees.Valid_Node_Access_Array;
      Tree_Indenting    : in     Syntax_Trees.Valid_Node_Access;
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

      Declaration : constant Syntax_Trees.Valid_Node_Access := Tree.Find_Ancestor
        (Tree_Indenting,
         (if To_Token_Enum (Anchor) = TYPE_ID
          then +full_type_declaration_ID
          else +record_representation_clause_ID));

      Tree_Anchor : constant Syntax_Trees.Valid_Node_Access := Tree.Find_Child (Declaration, Anchor);

      --  Args (2) is the index of RECORD (or a nonterminal possibly
      --  starting with RECORD) in Tokens
      Record_Token_Tree_Index : constant Syntax_Trees.Node_Access := Tokens (Positive_Index_Type (Integer'(Args (2))));
   begin
      --  Args (3) is the offset
      return Indent_Record
        (Parse_Data_Type (Data),
         Tree,
         Tree.Base_Token (Nonterm).Line,
         Anchor_Token      => Tree.Base_Token (Tree_Anchor),
         Record_Token      => Get_Augmented_Token (Tree, Tree.First_Terminal (Record_Token_Tree_Index)),
         Indenting_Token   => Get_Augmented_Token (Tree, Tree_Indenting),
         Indenting_Comment => Indenting_Comment,
         Offset            => Args (3));
   end Ada_Indent_Record_1;

end Wisi.Ada;
--  Local Variables:
--  ada-case-strict: nil
--  End:
