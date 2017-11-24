--  Abstract :
--
--  Types and operations shared by Ada and Ada_Emacs outputs.
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

with WisiToken.Parser.LR;
with WisiToken.Production;
with Wisi.Gen_Generate_Utils;
generic
   Prologues : in Wisi.Prologues;
   Tokens    : in Wisi.Tokens;
   Conflicts : in Wisi.Conflict_Lists.List;
   Params    : in Wisi.Generate_Param_Type;
package Wisi.Gen_Output_Ada_Common is

   EOI_Name : constant Standard.Ada.Strings.Unbounded.Unbounded_String := +"Wisi_EOI";
   --  EOI_Name must match wisi-output_elisp.adb EOI_Name, which must
   --  match Emacs ada-mode wisi.el wisi-eoi-term. It must
   --  be a valid Ada identifier when "_ID" is appended.

   WisiToken_Accept_Name : constant Standard.Ada.Strings.Unbounded.Unbounded_String := +"wisitoken_accept";

   function To_Token_Ada_Name (WY_Name : in String) return String;

   package Generate_Utils is new Wisi.Gen_Generate_Utils
     (Tokens, Conflicts, EOI_Name, WisiToken_Accept_Name);

   type Data_Type is record
      Parser_Algorithm : Valid_Parser_Algorithm;
      Lexer            : Valid_Lexer;
      Interface_Kind   : Valid_Interface;

      Accept_Reduce_Conflict_Count : Integer := -1;
      Shift_Reduce_Conflict_Count  : Integer := -1;
      Reduce_Reduce_Conflict_Count : Integer := -1;
      Table_Entry_Count            : Integer := -1;
      Parser_State_Count           : WisiToken.Parser.LR.Unknown_State_Index := 0;

      Grammar : WisiToken.Production.List.Instance;

      Package_Name_Root       : Standard.Ada.Strings.Unbounded.Unbounded_String;
      Lower_Package_Name_Root : Standard.Ada.Strings.Unbounded.Unbounded_String;

   end record;

   Data : Data_Type;

   procedure Initialize
     (Input_File_Name  : in String;
      Output_File_Root : in String;
      Check_Interface  : in Boolean);
   --  set Data

   function File_Name_To_Ada (File_Name : in String) return String;

   type Action_Name_List is array (Integer range <>) of access String;
   type Action_Name_List_Access is access Action_Name_List;
   type Nonterminal_Array_Action_Names is array (Generate_Utils.Nonterminal_ID) of Action_Name_List_Access;
   Ada_Action_Names : Nonterminal_Array_Action_Names;
   --  Ada names of subprograms for each grammar semantic action;
   --  non-null only if there is an action in the grammar.

   Parsers : array (Single_Parser_Algorithm) of WisiToken.Parser.LR.Parse_Table_Ptr;

   procedure Create_Ada_Spec
     (Input_File_Name  : in String;
      Output_File_Name : in String;
      Package_Name     : in String;
      Language_Name    : in String;
      Output_Language  : in Ada_Output_Language;
      Descriptor       : in WisiToken.Descriptor'Class;
      Interface_Kind   : in Interface_Type;
      Declare_Enum     : in Boolean);

   procedure Create_Create_Parser
     (Parser_Algorithm   : in Valid_Parser_Algorithm;
      Interface_Kind     : in Interface_Type;
      First_State_Index  : in Integer;
      First_Parser_Label : in Integer);

   procedure Create_Parser_Core (Table : in WisiToken.Parser.LR.Parse_Table_Ptr);

   procedure Create_re2c
     (Input_File_Name       : in String;
      Output_File_Name_Root : in String;
      Elisp_Regexps         : in Wisi.String_Pair_Lists.List);

end Wisi.Gen_Output_Ada_Common;
