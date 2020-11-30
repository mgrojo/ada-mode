--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2020 Free Software Foundation All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (Modified_GPL);

with Ada.Finalization;
with SAL.Gen_Unbounded_Definite_Red_Black_Trees;
package body Wisi_Parse_Context is

   function Source_File_Name (Item : in Parse_Context_Access) return String
   is (WisiToken.Parse.Source_File_Name (Item.Parser));

   package File_Parse_Context_Maps is new SAL.Gen_Unbounded_Definite_Red_Black_Trees
     (Element_Type => Parse_Context_Access,
      Key_Type     => String,
      Key          => Source_File_Name,
      Key_Compare  => SAL.String_Compare);

   Map : File_Parse_Context_Maps.Tree;

   function Find_Create
     (File_Name : in String;
      Language  : in Wisi_Parse_Context.Language;
      Trace     : in WisiToken.Trace_Access)
     return Parse_Context_Access
   is
      use File_Parse_Context_Maps;
      Found : constant Cursor := Map.Find (File_Name);
   begin
      if Has_Element (Found) then
         return Element (Found);
      end if;

      return Result : constant Parse_Context_Access :=
        (new Parse_Context'
           (Text_Buffer                       => null,
            Parser                            => WisiToken.Parse.LR.Parser.Parser'
              (Ada.Finalization.Limited_Controlled with
               Trace                          => Trace,
               User_Data                      => Wisi.New_User_Data (Language.Parse_Data_Template.all),
               Table                          => Language.Table,
               Language_Fixes                 => Language.Fixes,
               Language_Matching_Begin_Tokens => Language.Matching_Begin_Tokens,
               Language_String_ID_Set         => Language.String_ID_Set,
               others                         => <>)))
      do
         Result.Parser.Tree.Lexer := Language.Lexer;
         Map.Insert (Result);
      end return;
   end Find_Create;

   procedure Clear
   is begin
      Map.Clear;
   end Clear;

end Wisi_Parse_Context;
