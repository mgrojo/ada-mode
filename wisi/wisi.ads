--  Abstract :
--
--  Root package for Wisi grammar parser/code generator; see [2]
--
--  "wisi" used to be short for "Wisent Indentation engine", but now
--  it's just a name.
--
--  The input file syntax is based on Gnu bison syntax [1] with
--  some additions. See [2] for the syntax accepted by wisi-generate.
--
--  The Elisp and Ada_Emacs output languages are for use with the
--  Emacs wisi package.
--
--  Reference :
--
--  [1] http://www.gnu.org/software/bison/manual/ (info "(bison)Top")
--  [2] http://www.nongnu.org/ada-mode/wisi/wisi-user_guide.html, (info "(wisi-user_guide)Top")
--
--  Copyright (C) 2012 - 2015, 2017, 2018 Stephen Leake.  All Rights Reserved.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. This library is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with Ada.Characters.Handling;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Strings.Unbounded;
with WisiToken;
package Wisi is

   User_Error : exception; -- error in command line options or parameters

   --  See also WisiToken.Grammar_Error, Syntax_Error

   Not_Found : exception;
   --  something not found; should be handled and converted to Syntax_ or Programmer_Error

   Programmer_Error : exception; -- Error in Wisi Ada code

   type Generator_Algorithm_Type is (None, LALR_LR1, LALR, LR1, Packrat);
   subtype Valid_Generator_Algorithm is Generator_Algorithm_Type range LALR_LR1 .. Packrat;
   subtype LR_Single_Generator_Algorithm is Generator_Algorithm_Type range LALR .. LR1;
   subtype LR_Generator_Algorithm is Generator_Algorithm_Type range LALR_LR1 .. LR1;

   type Output_Language_Type is (None, Ada, Ada_Emacs, Elisp);
   subtype Valid_Output_Language is Output_Language_Type range Ada .. Elisp;
   subtype Ada_Output_Language is Output_Language_Type range Ada .. Ada_Emacs;

   type Lexer_Type is (None, Elisp_Lexer, re2c_Lexer);
   subtype Valid_Lexer is Lexer_Type range Elisp_Lexer .. Lexer_Type'Last;
   --  We append "_Lexer" to these names to avoid colliding with the
   --  similarly-named WisiToken packages. In the grammar file, they
   --  are named by:
   Lexer_Names : constant array (Valid_Lexer) of access constant String :=
     (Elisp_Lexer => new String'("elisp"),
      re2c_Lexer  => new String'("re2c"));

   function To_Lexer (Item : in String) return Lexer_Type;
   --  Raises User_Error for invalid Item

   type Interface_Type is (None, Process, Module);
   subtype Valid_Interface is Interface_Type range Process .. Module;

   package String_Lists is new Standard.Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   type Generate_Param_Type is record
      --  Set by grammar file declarations. Error recover parameters
      --  are in McKenzie_Recover_Param_Type below.
      Case_Insensitive              : Boolean                  := False;
      Embedded_Quote_Escape_Doubled : Boolean                  := False;
      End_Names_Optional_Option     : Standard.Ada.Strings.Unbounded.Unbounded_String;
      First_Parser_Label            : Integer                  := 0;
      First_State_Index             : Integer                  := 0;
      Interface_Kind                : Interface_Type           := None;
      Lexer                         : Lexer_Type               := None;
      Language_Runtime              : Boolean                  := True;
      Error_Recover                 : Boolean                  := False;
      Output_Language               : Output_Language_Type     := None;
      Generator_Algorithm           : Generator_Algorithm_Type := None;
      Start_Token                   : Standard.Ada.Strings.Unbounded.Unbounded_String;
   end record;

   type Raw_Code_Location is
     (Copyright_License,
      Actions_Spec_Context, Actions_Spec_Pre, Actions_Spec_Post,
      Actions_Body_Context, Actions_Body_Pre, Actions_Body_Post);
   --  So far we have not needed raw code other than license in the main
   --  package.

   type Raw_Code is array (Raw_Code_Location) of String_Lists.List;

   subtype String_2 is String (1 .. 2);

   Ada_Comment   : constant String_2 := "--";
   C_Comment     : constant String_2 := "//";
   Elisp_Comment : constant String_2 := ";;";

   function Split_Lines (Item : in String) return String_Lists.List;

   function Trim (Item : in String_Lists.List; Comment_Start : in String) return String_Lists.List;
   --  From each element, delete trailing comments starting with
   --  Comment_Start; delete leading and trailing spaces.

   procedure Put_Raw_Code
     (Comment_Syntax : in String_2;
      Code           : in String_Lists.List;
      Comment_Only   : in Boolean := False);
   --  Output Code to Ada.Text_IO.Current_Output.
   --
   --  If first two characters of a line are the same and not ' ', it is
   --  assumed to be a comment; ensure the output line has
   --  Comment_Syntax.
   --
   --  If Comment_Only is True, or if the comment syntax used in Code
   --  does not equal Comment_Syntax, only output comment lines.
   --
   --  If Comment_Syntax is Elisp_Comment, only output lines that are
   --  valid elisp comments or forms (ie start with ';;' or '(').
   --
   --  Otherwise output all lines.

   procedure Put_File_Header
     (Comment_Syntax : in String_2;
      Emacs_Mode     : in String := "");
   --  Output "parser support file <emacs_mode> /n command line: " comment to Ada.Text_IO.Current_Output.

   type String_Pair_Type is record
      Name  : aliased Standard.Ada.Strings.Unbounded.Unbounded_String;
      Value : Standard.Ada.Strings.Unbounded.Unbounded_String;
   end record;

   package String_Pair_Lists is new Standard.Ada.Containers.Doubly_Linked_Lists (String_Pair_Type);

   function Is_Present (List : in String_Pair_Lists.List; Name : in String) return Boolean;
   function Value (List : in String_Pair_Lists.List; Name : in String) return String;

   type McKenzie_Recover_Param_Type is record
      Default_Insert             : Natural               := 0;
      Default_Delete_Terminal    : Natural               := 0;
      Default_Delete_Nonterminal : Natural               := 0;
      Default_Push_Back          : Natural               := 0;
      Delete                     : String_Pair_Lists.List;
      Insert                     : String_Pair_Lists.List;
      Cost_Limit                 : Natural               := Integer'Last;
      Check_Limit                : WisiToken.Token_Index := WisiToken.Token_Index'Last;
      Check_Delta_Limit          : Natural               := Integer'Last;
      Enqueue_Limit              : Natural               := Integer'Last;
   end record;

   type Token_Kind_Type is record
      Kind   : Standard.Ada.Strings.Unbounded.Unbounded_String;
      Tokens : String_Pair_Lists.List;
   end record;

   package Token_Lists is new Standard.Ada.Containers.Doubly_Linked_Lists (Token_Kind_Type);

   function Count (Tokens : in Token_Lists.List) return Integer;
   --  Count of all leaves.

   procedure Add_Token
     (Tokens : in out Token_Lists.List;
      Kind   : in     String;
      Name   : in     String;
      Value  : in     String);
   --  Add Name, Value to Kind list in Tokens.

   function Is_In (Tokens : in Token_Lists.List; Kind : in String) return Boolean;
   function Is_In
     (Tokens : in Token_Lists.List;
      Kind   : in String;
      Value  : in String)
     return Boolean;

   type Conflict is record
      Source_Line : WisiToken.Line_Number_Type;
      Action_A    : Standard.Ada.Strings.Unbounded.Unbounded_String;
      LHS_A       : Standard.Ada.Strings.Unbounded.Unbounded_String;
      Action_B    : Standard.Ada.Strings.Unbounded.Unbounded_String;
      LHS_B       : Standard.Ada.Strings.Unbounded.Unbounded_String;
      On          : Standard.Ada.Strings.Unbounded.Unbounded_String;
   end record;

   package Conflict_Lists is new Standard.Ada.Containers.Doubly_Linked_Lists (Conflict);

   type RHS_Type is record
      Tokens      : String_Lists.List;
      Action      : Standard.Ada.Strings.Unbounded.Unbounded_String;
      Check       : Standard.Ada.Strings.Unbounded.Unbounded_String;
      Source_Line : WisiToken.Line_Number_Type;
   end record;
   package RHS_Lists is new Standard.Ada.Containers.Doubly_Linked_Lists (RHS_Type, "=");

   type Rule_Type is record
      Left_Hand_Side   : aliased Standard.Ada.Strings.Unbounded.Unbounded_String;
      Right_Hand_Sides : RHS_Lists.List;
      Source_Line      : WisiToken.Line_Number_Type;
   end record;

   package Rule_Lists is new Standard.Ada.Containers.Doubly_Linked_Lists (Rule_Type);

   function Is_Present (Rules : in Rule_Lists.List; LHS : in String) return Boolean;

   type Tokens is record
      Non_Grammar      : Token_Lists.List;
      Keywords         : String_Pair_Lists.List;
      Tokens           : Token_Lists.List;
      Rules            : Rule_Lists.List;
      --  Rules included here because they define the nonterminal tokens, as
      --  well as the productions.

      Regexps : String_Pair_Lists.List;
      --  Regexps included here because they are used in defining the
      --  Tokens., 2018
   end record;

   type Elisp_Names is record
      --  specified in grammar file declarations; see
      --  wisi-output_elisp_common.ads Find_Name
      Faces   : String_Lists.List;
      Indents : String_Pair_Lists.List;
      Regexps : String_Pair_Lists.List;
   end record;

   type Names_Array is array (Integer range <>) of access constant String;
   type Names_Array_Access is access Names_Array;
   type Names_Array_Array is array (WisiToken.Token_ID range <>) of Names_Array_Access;

   function "+" (Item : in String) return Standard.Ada.Strings.Unbounded.Unbounded_String
     renames Standard.Ada.Strings.Unbounded.To_Unbounded_String;

   function "-" (Item : in Standard.Ada.Strings.Unbounded.Unbounded_String) return String
     renames Standard.Ada.Strings.Unbounded.To_String;

   function To_Lower (Item : in String) return String
     renames Standard.Ada.Characters.Handling.To_Lower;

   function To_Upper (Item : in String) return String
     renames Standard.Ada.Characters.Handling.To_Upper;

   function To_Upper (Item : in Character) return Character
     renames Standard.Ada.Characters.Handling.To_Upper;

   function "+" (List : in String_Lists.List; Item : in String) return String_Lists.List;

   function String_To_String_List (Item : in String) return String_Lists.List;
   function "+" (Item : in String) return String_Lists.List renames String_To_String_List;

   function RHS_To_RHS_List (Item : in RHS_Type) return RHS_Lists.List;
   function "+" (Item : in RHS_Type) return RHS_Lists.List renames RHS_To_RHS_List;

   function "+" (List : in RHS_Lists.List; Item : in RHS_Type) return RHS_Lists.List;

   function Image (Item : in Boolean) return String
     is (if Item then "True" else "False");
   --  Match casing in Standard.

   procedure Put_Command_Line (Comment_Prefix : in String);
   --  Put command line to current output

end Wisi;
