--  Abstract :
--
--  Ada implementation of:
--
--  [1] ada-wisi-elisp-parse.el
--  [2] ada-indent-user-options.el
--
--  Copyright (C) 2017 Stephen Leake All Rights Reserved.
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

package WisiToken.Wisi_Runtime.Ada is

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
   Ada_Indent_Hanging_Rel_Exp : Boolean := False;

   type Parse_Data_Type is new Wisi_Runtime.Parse_Data_Type with private;

   overriding
   procedure Initialize
     (Data             : in out Parse_Data_Type;
      Semantic_State   : in     WisiToken.Token_Line_Comment.State_Access;
      Source_File_Name : in     String;
      Parse_Action     : in     Parse_Action_Type;
      Line_Count       : in     Line_Number_Type;
      Params           : in     String);
   --  Call Wisi_Runtime.Initialize, then:
   --
   --  If Params /= "", set all indent parameters from Params, in
   --  declaration order; otherwise keep default values. Boolean is
   --  represented by 0 | 1. Parameter values are space delimited.
   --
   --  Also do any other initialization that Data needs.

   overriding
   function Indent_Hanging_1
     (Data              : in out Parse_Data_Type;
      Tokens            : in     Semantic_State.Augmented_Token_Array;
      Indenting_Token   : in     Token_Line_Comment.Token;
      Indenting_Comment : in     Boolean;
      Delta_1           : in     Simple_Indent_Param;
      Delta_2           : in     Simple_Indent_Param;
      Option            : in     Boolean;
      Accumulate        : in     Boolean)
     return Delta_Type;
   --  [1] ada-wisi-elisp-parse--indent-hanging

   ----------
   --  The following are declared in ada.wy %elisp_indent, and must match
   --  Language_Indent_Function.

   function Ada_Indent_Aggregate
     (Data              : in out Wisi_Runtime.Parse_Data_Type'Class;
      Tokens            : in     Semantic_State.Augmented_Token_Array;
      Indenting         : in     Token_Line_Comment.Token;
      Indenting_Comment : in     Boolean;
      Args              : in     WisiToken.Wisi_Runtime.Indent_Arg_Arrays.Vector)
     return WisiToken.Wisi_Runtime.Delta_Type;
   --  [1] ada-indent-aggregate

   function Ada_Indent_Renames_0
     (Data              : in out Wisi_Runtime.Parse_Data_Type'Class;
      Tokens            : in     Semantic_State.Augmented_Token_Array;
      Indenting         : in     Token_Line_Comment.Token;
      Indenting_Comment : in     Boolean;
      Args              : in     WisiToken.Wisi_Runtime.Indent_Arg_Arrays.Vector)
     return WisiToken.Wisi_Runtime.Delta_Type;
   --  [1] ada-indent-renames

   function Ada_Indent_Return_0
     (Data              : in out Wisi_Runtime.Parse_Data_Type'Class;
      Tokens            : in     Semantic_State.Augmented_Token_Array;
      Indenting         : in     Token_Line_Comment.Token;
      Indenting_Comment : in     Boolean;
      Args              : in     WisiToken.Wisi_Runtime.Indent_Arg_Arrays.Vector)
     return WisiToken.Wisi_Runtime.Delta_Type;
   --  [1] ada-indent-return

   function Ada_Indent_Record_0
     (Data              : in out Wisi_Runtime.Parse_Data_Type'Class;
      Tokens            : in     Semantic_State.Augmented_Token_Array;
      Indenting         : in     Token_Line_Comment.Token;
      Indenting_Comment : in     Boolean;
      Args              : in     WisiToken.Wisi_Runtime.Indent_Arg_Arrays.Vector)
     return WisiToken.Wisi_Runtime.Delta_Type;
   --  [1] ada-indent-record

   function Ada_Indent_Record_1
     (Data              : in out Wisi_Runtime.Parse_Data_Type'Class;
      Tokens            : in     Semantic_State.Augmented_Token_Array;
      Indenting         : in     Token_Line_Comment.Token;
      Indenting_Comment : in     Boolean;
      Args              : in     WisiToken.Wisi_Runtime.Indent_Arg_Arrays.Vector)
     return WisiToken.Wisi_Runtime.Delta_Type;
   --  [1] ada-indent-record*

private

   type Parse_Data_Type is new Wisi_Runtime.Parse_Data_Type with
   record
      Record_ID : Token_ID;
   end record;

end WisiToken.Wisi_Runtime.Ada;
