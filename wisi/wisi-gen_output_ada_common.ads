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

with Wisi.Gen_Generate_Utils;
generic
   Keywords  : in Wisi.String_Pair_Lists.List;
   Tokens    : in Wisi.Token_Lists.List;
   Conflicts : in Wisi.Conflict_Lists.List;
   Rules     : in Wisi.Rule_Lists.List;
   Params    : in Wisi.Generate_Param_Type;

   with procedure Put_Ada_Prologue_Context_Clause;
   with procedure Put_Ada_Prologue_Declarations;
   with procedure Put_Aflex_Prologue;
package Wisi.Gen_Output_Ada_Common is

   EOI_Name : constant Standard.Ada.Strings.Unbounded.Unbounded_String := +"WISI_EOI";
   --  EOI_Name must match wisi-output_elisp.adb EOI_Name, which must
   --  match Emacs ada-mode wisi.el wisi-eoi-term. It must
   --  be a valid Ada identifier when "_ID" is appended.

   FastToken_Accept_Name : constant Standard.Ada.Strings.Unbounded.Unbounded_String := +"fasttoken_accept";

   function To_Token_Ada_Name (Item : in String) return String;
   function To_Token_Ada_Name (Item : in Standard.Ada.Strings.Unbounded.Unbounded_String) return String;

   package Generate_Utils is new Wisi.Gen_Generate_Utils
     (Keywords, Tokens, Conflicts, Rules, EOI_Name, FastToken_Accept_Name, Params.First_State_Index, To_Token_Ada_Name);

   type Data_Type is record
      Parser_Algorithm : Valid_Parser_Algorithm;
      Lexer            : Valid_Lexer;
      Interface_Kind   : Valid_Interface;

      --  These are set by *_Generator.Generate
      Accept_Reduce_Conflict_Count : Integer := -1;
      Shift_Reduce_Conflict_Count  : Integer := -1;
      Reduce_Reduce_Conflict_Count : Integer := -1;
      Table_Entry_Count            : Integer := -1;
      Parser_State_Count           : Generate_Utils.LR.Unknown_State_Index := 0;

      Grammar : Generate_Utils.Production.List.Instance;

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

   type Action_Name_List is array (Integer range <>) of access constant String;
   type Action_Name_List_Access is access Action_Name_List;
   Action_Names : array (Generate_Utils.Token_ID) of Action_Name_List_Access;
   --  Names of subprograms for each grammar action

   Parsers : array (Single_Parser_Algorithm) of Generate_Utils.LR.Parse_Table_Ptr;

   procedure Create_Ada_Spec
     (Input_File_Name    : in String;
      Output_File_Name   : in String;
      Package_Name       : in String;
      Interface_Kind     : in Valid_Interface;
      Lexer              : in Valid_Lexer;
      First_State_Index  : in Integer;
      First_Parser_Label : in Integer);

   procedure Create_Create_Parser
     (Input_File_Name  : in String;
      Parser_Algorithm : in Valid_Parser_Algorithm;
      Lexer            : in Valid_Lexer;
      Interface_Kind   : in Valid_Interface);

   procedure Create_Parser_Core
     (Input_File_Name : in String;
      Parser          : in Generate_Utils.LR.Parse_Table_Ptr);

   procedure Create_Aflex
     (Input_File_Name       : in String;
      Output_File_Name_Root : in String);

end Wisi.Gen_Output_Ada_Common;
