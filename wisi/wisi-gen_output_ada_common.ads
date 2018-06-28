--  Abstract :
--
--  Types and operations shared by Ada and Ada_Emacs outputs.
--
--  Copyright (C) 2017, 2018 Stephen Leake All Rights Reserved.
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

with WisiToken.LR;
with WisiToken.Productions;
with Wisi.Gen_Generate_Utils;
generic
   Raw_Code  : in Wisi.Raw_Code;
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

   package Generate_Utils is new Wisi.Gen_Generate_Utils (Tokens, Conflicts, EOI_Name, WisiToken_Accept_Name);

   type Data_Type is record
      Generator_Algorithm : Valid_Generator_Algorithm;
      Lexer               : Valid_Lexer;
      Interface_Kind      : Valid_Interface;

      Accept_Reduce_Conflict_Count : Integer := -1;
      Shift_Reduce_Conflict_Count  : Integer := -1;
      Reduce_Reduce_Conflict_Count : Integer := -1;
      Table_Entry_Count            : Integer := -1;
      Parser_State_Count           : WisiToken.Unknown_State_Index := 0;

      Grammar : WisiToken.Productions.Prod_Arrays.Vector;

      Package_Name_Root       : Standard.Ada.Strings.Unbounded.Unbounded_String;
      Lower_Package_Name_Root : Standard.Ada.Strings.Unbounded.Unbounded_String;

   end record;

   type LR_Parser_Array is array (Generator_Algorithm_Type range LALR .. LR1) of WisiToken.LR.Parse_Table_Ptr;

   procedure Initialize
     (Data             : in out Data_Type;
      Input_File_Name  : in     String;
      Output_File_Root : in     String;
      Check_Interface  : in     Boolean);
   --  set Data

   function File_Name_To_Ada (File_Name : in String) return String;

   subtype Nonterminal_Names_Array is Names_Array_Array (Generate_Utils.Nonterminal_ID);
   --  Ada names of subprograms for each grammar semantic action and check;
   --  non-null only if there is an action or check in the grammar.

   procedure Create_Ada_Actions_Spec
     (Output_File_Name : in String;
      Package_Name     : in String;
      Descriptor       : in WisiToken.Descriptor'Class;
      Declare_Enum     : in Boolean;
      Ada_Action_Names : in Nonterminal_Names_Array;
      Ada_Check_Names  : in Nonterminal_Names_Array;
      Actions_Present  : in Boolean;
      Checks_Present   : in Boolean);

   procedure Create_Ada_Main_Spec
     (Output_File_Name    : in String;
      Main_Package_Name   : in String;
      Generator_Algorithm : in Valid_Generator_Algorithm;
      Output_Language     : in Ada_Output_Language;
      Interface_Kind      : in Interface_Type);

   procedure LR_Create_Create_Parser
     (Data                : in out Data_Type;
      Parsers             : in     LR_Parser_Array;
      Generator_Algorithm : in     LR_Generator_Algorithm;
      Interface_Kind      : in     Interface_Type;
      First_State_Index   : in     Integer;
      First_Parser_Label  : in     Integer;
      Action_Names        : in     Nonterminal_Names_Array;
      Check_Names         : in     Nonterminal_Names_Array);

   procedure Packrat_Create_Create_Parser;

   procedure Create_re2c
     (Output_File_Name_Root : in String;
      Elisp_Regexps         : in Wisi.String_Pair_Lists.List);

end Wisi.Gen_Output_Ada_Common;
