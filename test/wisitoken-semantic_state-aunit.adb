--  Abstract :
--
--  AUnit checks for parent
--
--  Copyright (C) 2017, 2018 Stephen Leake All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL); -- AUnit is not Modified_GPL

with AUnit.Assertions;
with Ada.Tags;
with AUnit.Checks.Text_IO;
with WisiToken.AUnit;
package body WisiToken.Semantic_State.AUnit is

   procedure Check
     (Label    : in String;
      Computed : in Semantic_Action;
      Expected : in Semantic_Action)
   is begin
      Standard.AUnit.Assertions.Assert (Computed = Expected, Label & ": access type mismatch");
   end Check;

   procedure Check
     (Label              : in String;
      Computed           : in Recover_Data_Access;
      Expected           : in Recover_Data_Access;
      Check_Recover_Data : in Check_Recover_Type)
   is
      use all type Ada.Tags.Tag;
      use Standard.AUnit.Assertions;
   begin
      if Expected = null then
         if Check_Recover_Data = null then
            --  not checking Recover_Data
            null;
         else
            if Computed /= null then
               Assert (False, Label & ": expecting null");
            end if;
         end if;
      else
         if Computed = null then
            Assert (False, Label & ": got null");
         else
            if Check_Recover_Data = null then
               Assert (False, Label & "Check_Recover_Data = null");
            else
               Assert (Computed.all'Tag = Expected.all'Tag, "Recover_Data'Class mismatch");
               Check_Recover_Data (Label, Computed.all, Expected.all);
            end if;
         end if;
      end if;
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in Augmented_Token;
      Expected : in Augmented_Token)
   is
      use WisiToken.AUnit;
      use Standard.AUnit.Checks.Text_IO;
   begin
      Check (Label & ".ID", Computed.ID, Expected.ID);
      Check (Label & ".Line", Computed.Line, Expected.Line);
      Check (Label & ".Col", Computed.Col, Expected.Col);
      Check (Label & ".Char_Region", Computed.Char_Region, Expected.Char_Region);
      Check (Label & ".Byte_Region", Computed.Byte_Region, Expected.Byte_Region);
   end Check;

   procedure Check
     (Label              : in String;
      Computed           : in Parser_Error_Data;
      Expected           : in Parser_Error_Data;
      Check_Recover_Data : in WisiToken.Semantic_State.AUnit.Check_Recover_Type)
   is
      use WisiToken.AUnit;
      use WisiToken.Semantic_State.AUnit;
   begin
      Check (Label & ".Error_Token", Computed.Error_Token, Expected.Error_Token);
      Check (Label & ".Expecting", Computed.Expecting, Expected.Expecting);
      Check (Label & ".Recover", Computed.Recover, Expected.Recover, Check_Recover_Data);
   end Check;

end WisiToken.Semantic_State.AUnit;
