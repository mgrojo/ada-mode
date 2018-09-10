--  Abstract :
--
--  AUnit utils for parent.
--
--  Copyright (C) 2018 Stephen Leake All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 51 Franklin Street, Suite 500, Boston,
--  MA 02110-1335, USA.

pragma License (GPL);

with Ada.Containers;
with WisiToken.Parse.LR.Parser_Lists;
with WisiToken.Semantic_Checks;
generic
   Descriptor         : in WisiToken.Descriptor;
   Empty_Token_ID_Set : in WisiToken.Token_ID_Set;
package WisiToken.Parse.LR.Parser.Gen_AUnit is

   procedure Parse_Text
     (Parser           : in out WisiToken.Parse.LR.Parser.Parser;
      Text             : in     String;
      Expect_Exception : in     Boolean := False);

   procedure Check_Recover
     (Parser_State            : in WisiToken.Parse.LR.Parser_Lists.Parser_State;
      Label                   : in String                                       := "";
      Parser_Label            : in Integer                                      := -1;
      Errors_Length           : in Ada.Containers.Count_Type                    := 1;
      Checking_Error          : in Ada.Containers.Count_Type                    := 1;
      Error_Token_ID          : in WisiToken.Token_ID;
      Error_Token_Byte_Region : in WisiToken.Buffer_Region                      := WisiToken.Null_Buffer_Region;
      Success                 : in Boolean                                      := True;
      Ops                     : in WisiToken.Parse.LR.Config_Op_Arrays.Vector :=
        WisiToken.Parse.LR.Config_Op_Arrays.Empty_Vector;
      Ops_Race_Condition      : in Boolean                                      := False;
      Enqueue_Low             : in Integer := 0;
      Enqueue_High            : in Integer := 0;
      Check_Low               : in Integer;
      Check_High              : in Integer;
      Cost                    : in Integer := 0;
      Expecting               : in WisiToken.Token_ID_Set                       := Empty_Token_ID_Set;
      Code                    : in WisiToken.Semantic_Checks.Check_Status_Label := WisiToken.Semantic_Checks.Ok);

end WisiToken.Parse.LR.Parser.Gen_AUnit;
