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
with WisiToken.Wisi_Grammar_Runtime;
with Wisi.Generate_Utils;
package Wisi.Output_Ada_Common is

   function To_Token_Ada_Name (WY_Name : in String) return String;

   type Common_Data is limited record
      --  Validated versions of .wy values
      Generator_Algorithm : Valid_Generator_Algorithm;
      Lexer               : Valid_Lexer;
      Output_Language     : Ada_Output_Language;
      Interface_Kind      : Valid_Interface;

      --  LR parse table stats
      Table_Actions_Count          : Integer                       := -1; -- parse, not user, actions
      Parser_State_Count           : WisiToken.Unknown_State_Index := 0;
      Accept_Reduce_Conflict_Count : Integer;
      Shift_Reduce_Conflict_Count  : Integer;
      Reduce_Reduce_Conflict_Count : Integer;

      --  Various names
      Package_Name_Root    : Standard.Ada.Strings.Unbounded.Unbounded_String;
      Lower_File_Name_Root : Standard.Ada.Strings.Unbounded.Unbounded_String;

      Ada_Action_Names : access Names_Array_Array;
      Ada_Check_Names  : access Names_Array_Array;
      --  Ada names of subprograms for each grammar semantic action and check;
      --  non-null only if there is an action or check in the grammar.
   end record;

   type LR_Parser_Array is array (Generator_Algorithm_Type range LALR .. LR1) of WisiToken.LR.Parse_Table_Ptr;

   function Initialize
     (Generate_Params   : in Wisi.Generate_Param_Type;
      First_Nonterminal : in WisiToken.Token_ID;
      Last_Nonterminal  : in WisiToken.Token_ID;
      Input_File_Name   : in String;
      Output_File_Root  : in String;
      Check_Interface   : in Boolean)
     return Common_Data;

   function File_Name_To_Ada (File_Name : in String) return String;

   procedure Create_Ada_Actions_Spec
     (Output_File_Name :         in              String;
      Package_Name     :         in              String;
      Descriptor       :         in              WisiToken.Descriptor'Class;
      Input_Data       :         in              WisiToken.Wisi_Grammar_Runtime.User_Data_Type;
      Generate_Data    : aliased in              Wisi.Generate_Utils.Generate_Data;
      Ada_Action_Names :         not null access constant Names_Array_Array;
      Ada_Check_Names  :         not null access constant Names_Array_Array;
      Actions_Present  :         in              Boolean;
      Checks_Present   :         in              Boolean);

   procedure Create_Ada_Main_Spec
     (Output_File_Name  : in String;
      Main_Package_Name : in String;
      Input_Data        : in WisiToken.Wisi_Grammar_Runtime.User_Data_Type;
      Common_Data       : in Output_Ada_Common.Common_Data);

   procedure LR_Create_Create_Parser
     (Input_Data         :         in     WisiToken.Wisi_Grammar_Runtime.User_Data_Type;
      Common_Data        :         in out Output_Ada_Common.Common_Data;
      Generate_Data      : aliased in     Wisi.Generate_Utils.Generate_Data;
      Parsers            :         in     LR_Parser_Array;
      First_State_Index  :         in     Integer;
      First_Parser_Label :         in     Integer);

   procedure Packrat_Create_Create_Parser;

   procedure Create_re2c
     (Input_Data            :         in WisiToken.Wisi_Grammar_Runtime.User_Data_Type;
      Generate_Data         : aliased in Wisi.Generate_Utils.Generate_Data;
      Output_File_Name_Root :         in String;
      Elisp_Regexps         :         in Wisi.String_Pair_Lists.List);

   procedure Put_Stats
     (Input_Data  : in WisiToken.Wisi_Grammar_Runtime.User_Data_Type;
      Common_Data : in Output_Ada_Common.Common_Data);

end Wisi.Output_Ada_Common;
