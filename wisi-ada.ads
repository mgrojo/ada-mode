--  Abstract :
--
--  Ada language specific indent options and functions
--
--  [2] ada-indent-user-options.el
--
--  Copyright (C) 2017 - 2020 Free Software Foundation, Inc.
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

package Wisi.Ada is

   Language_Protocol_Version : constant String := "4";
   --  Defines the data passed to Initialize in Params.
   --
   --  This value must match ada-mode.el
   --  ada-wisi-language-protocol-version.
   --
   --  Only changes once per ada-mode release. Increment as soon as
   --  required, record new version in NEWS-ada-mode.text.

   --  Indent parameters from [2]
   Ada_Indent                 : Integer := 3;
   Ada_Indent_Broken          : Integer := 2;
   Ada_Indent_Comment_Col_0   : Boolean := False;
   Ada_Indent_Comment_GNAT    : Boolean := False;
   Ada_Indent_Label           : Integer := -3;
   Ada_Indent_Record_Rel_Type : Integer := 3;
   Ada_Indent_Renames         : Integer := 2;
   Ada_Indent_Return          : Integer := 0;
   Ada_Indent_Use             : Integer := 2;
   Ada_Indent_When            : Integer := 3;
   Ada_Indent_With            : Integer := 2;
   --  ada-indent-after-trailing-comment is implemented in elisp
   Ada_Indent_Subprogram_Is   : Integer := 2;

   --  Other parameters
   End_Names_Optional : Boolean := False;

   type Parse_Data_Type is new Wisi.Parse_Data_Type with null record;

   overriding
   function New_User_Data (Template : in Parse_Data_Type) return WisiToken.Syntax_Trees.User_Data_Access
   is (new Parse_Data_Type);

   overriding
   procedure Initialize
     (Data              : in out Parse_Data_Type;
      Post_Parse_Action : in     Post_Parse_Action_Type;
      Begin_Line        : in     WisiToken.Line_Number_Type;
      End_Line          : in     WisiToken.Line_Number_Type;
      Begin_Indent      : in     Integer;
      Params            : in     String);
   --  Call Wisi_Runtime.Initialize, then:
   --
   --  If Params /= "", set all language-specific parameters from Params,
   --  in declaration order; otherwise keep default values. Boolean is
   --  represented by 0 | 1. Parameter values are space delimited.
   --
   --  Also do any other initialization that Data needs.

   overriding
   function Insert_After
     (User_Data            : in out Parse_Data_Type;
      Tree                 : in     WisiToken.Syntax_Trees.Tree'Class;
      Insert_Token         : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Insert_Before_Token  : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Insert_On_Blank_Line : in     Boolean)
     return Boolean;

   overriding
   procedure Refactor_Help (Data : in Parse_Data_Type);

   overriding
   procedure Refactor
     (Data       : in out Parse_Data_Type;
      Tree       : in out WisiToken.Syntax_Trees.Tree;
      Action     : in     Positive;
      Edit_Begin : in     WisiToken.Buffer_Pos);

   ----------
   --  The following are declared in ada.wy %elisp_indent, and must match
   --  Wisi.Language_Indent_Function.

   function Ada_Indent_Aggregate
     (Data              : in out Wisi.Parse_Data_Type'Class;
      Tree              : in     WisiToken.Syntax_Trees.Tree;
      Nonterm           : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Tokens            : in     WisiToken.Syntax_Trees.Valid_Node_Access_Array;
      Tree_Indenting    : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Indenting_Comment : in     Boolean;
      Args              : in     Wisi.Indent_Arg_Arrays.Vector)
     return Wisi.Delta_Type;
   --  ada-indent-aggregate
   --  Args: none

   function Ada_Indent_Aspect
     (Data              : in out Wisi.Parse_Data_Type'Class;
      Tree              : in     WisiToken.Syntax_Trees.Tree;
      Nonterm           : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Tokens            : in     WisiToken.Syntax_Trees.Valid_Node_Access_Array;
      Tree_Indenting    : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Indenting_Comment : in     Boolean;
      Args              : in     Wisi.Indent_Arg_Arrays.Vector)
     return Delta_Type;
   --  ada-indent-aspect
   --  Args: none

   function Ada_Indent_Renames_0
     (Data              : in out Wisi.Parse_Data_Type'Class;
      Tree              : in     WisiToken.Syntax_Trees.Tree;
      Nonterm           : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Tokens            : in     WisiToken.Syntax_Trees.Valid_Node_Access_Array;
      Tree_Indenting    : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Indenting_Comment : in     Boolean;
      Args              : in     Wisi.Indent_Arg_Arrays.Vector)
     return Wisi.Delta_Type;
   --  ada-indent-renames
   --  Args: subprogram_token_index

   function Ada_Indent_Return_0
     (Data              : in out Wisi.Parse_Data_Type'Class;
      Tree              : in     WisiToken.Syntax_Trees.Tree;
      Nonterm           : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Tokens            : in     WisiToken.Syntax_Trees.Valid_Node_Access_Array;
      Tree_Indenting    : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Indenting_Comment : in     Boolean;
      Args              : in     Wisi.Indent_Arg_Arrays.Vector)
     return Wisi.Delta_Type;
   --  ada-indent-return
   --  Args: formal_part_token_index, offset

   function Ada_Indent_Record_0
     (Data              : in out Wisi.Parse_Data_Type'Class;
      Tree              : in     WisiToken.Syntax_Trees.Tree;
      Nonterm           : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Tokens            : in     WisiToken.Syntax_Trees.Valid_Node_Access_Array;
      Tree_Indenting    : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Indenting_Comment : in     Boolean;
      Args              : in     Wisi.Indent_Arg_Arrays.Vector)
     return Wisi.Delta_Type;
   --  ada-indent-record
   --  Args: anchor_token_index, record_token_index, offset

   function Ada_Indent_Record_1
     (Data              : in out Wisi.Parse_Data_Type'Class;
      Tree              : in     WisiToken.Syntax_Trees.Tree;
      Nonterm           : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Tokens            : in     WisiToken.Syntax_Trees.Valid_Node_Access_Array;
      Tree_Indenting    : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Indenting_Comment : in     Boolean;
      Args              : in     Wisi.Indent_Arg_Arrays.Vector)
     return Wisi.Delta_Type;
   --  ada-indent-record*
   --  Args: anchor_token_ID, record_token_index, offset

end Wisi.Ada;
