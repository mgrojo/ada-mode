--  Abstract :
--
--  See spec.
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

with AUnit.Checks;
with AUnit.Checks.Containers;
with WisiToken.AUnit;
with WisiToken.LR.AUnit;
with WisiToken.Semantic_Checks.AUnit;
package body WisiToken.LR.Parser.Gen_AUnit is

   procedure Parse_Text
     (Parser           : in out WisiToken.LR.Parser.Parser;
      Text             : in     String;
      Expect_Exception : in     Boolean := False)
   is
      use Standard.AUnit.Checks;
   begin
      if WisiToken.Trace_Parse > WisiToken.Outline then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("input: '" & Text & "'");
      end if;

      Parser.Lexer.Reset_With_String (Text);

      Parser.Parse;

      if WisiToken.Trace_Action > WisiToken.Outline then
         Parser.Put_Errors (File_Name => "<string>");
      end if;

      Check ("exception", False, Expect_Exception);
   exception
   when WisiToken.Syntax_Error =>
      if WisiToken.Trace_Parse > WisiToken.Outline then
         Parser.Put_Errors (File_Name => "<string>");
      end if;

      Check ("exception", True, Expect_Exception);
      if Expect_Exception then
         raise;
      end if;
   end Parse_Text;

   procedure Check_Recover
     (Parser_State            : in WisiToken.LR.Parser_Lists.Parser_State;
      Label                   : in String                                       := "";
      Parser_Label            : in Integer                                      := -1;
      Errors_Length           : in Ada.Containers.Count_Type                    := 1;
      Checking_Error          : in Ada.Containers.Count_Type                    := 1;
      Error_Token_ID          : in WisiToken.Token_ID;
      Error_Token_Byte_Region : in WisiToken.Buffer_Region                      := WisiToken.Null_Buffer_Region;
      Success                 : in Boolean                                      := True;
      Ops                     : in WisiToken.LR.Config_Op_Arrays.Vector := WisiToken.LR.Config_Op_Arrays.Empty_Vector;
      Ops_Race_Condition      : in Boolean                                      := False;
      Enqueue_Low             : in Integer := 0;
      Enqueue_High            : in Integer := 0;
      Check_Low               : in Integer;
      Check_High              : in Integer;
      Cost                    : in Integer := 0;
      Expecting               : in WisiToken.Token_ID_Set                       := Empty_Token_ID_Set;
      Code                    : in WisiToken.Semantic_Checks.Check_Status_Label := WisiToken.Semantic_Checks.Ok)
   is
      use Standard.AUnit.Checks;
      use Standard.AUnit.Checks.Containers;
      use WisiToken.AUnit;
      use WisiToken.LR.AUnit;
      use WisiToken.Semantic_Checks.AUnit;
      use all type WisiToken.Buffer_Region;
      use all type WisiToken.Token_ID;
      use all type WisiToken.Token_ID_Set;
      use all type WisiToken.LR.Parse_Error_Label;

      Label_I : constant String := Label & "." & Ada.Containers.Count_Type'Image (Checking_Error);

      Cursor : WisiToken.LR.Parse_Error_Lists.Cursor := Parser_State.Errors.First;
   begin
      if Parser_Label /= -1 then
         Check (Label & ".parser_label", Parser_State.Label, Parser_Label);
      end if;

      Check (Label_I & ".errors.length", Parser_State.Errors.Length, Errors_Length);

      for I in 2 .. Checking_Error loop
         WisiToken.LR.Parse_Error_Lists.Next (Cursor);
      end loop;

      declare
         use all type WisiToken.Semantic_Checks.Check_Status_Label;
         Error : WisiToken.LR.Parse_Error renames WisiToken.LR.Parse_Error_Lists.Element (Cursor);
      begin
         if Expecting /= Empty_Token_ID_Set then
            Check (Label_I & "expecting", Error.Expecting, Expecting);
         end if;

         if Code = WisiToken.Semantic_Checks.Ok then
            declare
               Token : WisiToken.Recover_Token renames Parser_State.Tree.Recover_Token (Error.Error_Token);
            begin
               Check (Label_I & ".label", Error.Label, Action);
               Check (Label_I & ".error_token.id", Token.ID, Error_Token_ID);
               if Error_Token_ID /= Descriptor.EOF_ID then
                  --  EOF byte_region is unreliable
                  Check (Label_I & ".error_token.byte_region", Token.Byte_Region, Error_Token_Byte_Region);
               end if;
            end;
         else
            Check (Label_I & ".label", Error.Label, Check);
            Check (Label_I & ".code", Error.Check_Status.Label, Code);
            if Error.Check_Status.End_Name.Byte_Region = WisiToken.Null_Buffer_Region then
               --  End_Name is empty; check begin_name
               Check (Label_I & ".begin_name.id", Error.Check_Status.Begin_Name.ID, Error_Token_ID);
               Check (Label_I & ".begin_name.byte_region", Error.Check_Status.Begin_Name.Byte_Region,
                      Error_Token_Byte_Region);
            else
               Check (Label_I & ".end_name.id", Error.Check_Status.End_Name.ID, Error_Token_ID);
               Check (Label_I & ".end_name.byte_region",
                      Error.Check_Status.End_Name.Byte_Region,
                      Error_Token_Byte_Region);
            end if;
         end if;

         Check (Label_I & ".success", Parser_State.Recover.Success, Success);

         if Success then
            if not Ops_Race_Condition then
               Check (Label_I & ".recover.ops", Error.Recover.Ops, Ops);
            end if;
         end if;

         --  The enqueue count depends on a race condition; configs with costs
         --  higher than the final solution may or may not be enqueued. So we
         --  test a range; we want to know if it gets a lot higher when we
         --  change something. Similarly for Check_Low, _High.
         --
         --  Recover does not come from the same parser as Error if the
         --  succeeding parser was spawned after error recovery, but we copy
         --  Enqueue_Count and Check_Count in Prepend_Copy just for this check.
         if not (Enqueue_Low = 0 and Enqueue_High = 0) then
            Check_Range (Label_I & ".enqueue", Parser_State.Recover.Enqueue_Count, Enqueue_Low, Enqueue_High);
         end if;

         Check_Range (Label_I & ".check", Parser_State.Recover.Check_Count, Check_Low, Check_High);

         if Cost > 0 then
            Check (Label_I & ".cost", Error.Recover.Cost, Cost);
         end if;
      end;
   end Check_Recover;

end WisiToken.LR.Parser.Gen_AUnit;
