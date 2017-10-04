--  Abstract :
--
--  See spec.
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

with Ada.Containers.Indefinite_Vectors;
with WisiToken.Token_Line_Comment;
package body WisiToken.Wisi_Runtime is


   ----------
   --  public subprograms

   procedure Initialize (Buffer_Data : in out Buffer_Data_Type; Line_Count : in Ada.Containers.Count_Type)
   is begin
      Buffer_Data.Lines.Set_Length (Line_Count);
   end Initialize;

   procedure Statement_Action
     (Nonterm : in Augmented_Token'Class;
      Source  : in Augmented_Token_Array;
      Params  : in Statement_Param_Array)
   is
   begin
      for Pair of Params loop
         declare
            Token : WisiToken.Token_Line_Comment.Token (Source (Pair.Index));
            Cache : Cache_Type;
         begin
            if Token.Char_Region /= Null_Buffer_Region then
               Cache := Get_Cache (Buffer_Data);
            else
            end if;
         exception
         end;

      end loop;
   end Statement_Action;

   procedure Containing_Action
     (Nonterm   : in Augmented_Token'Class;
      Source    : in Augmented_Token_Array;
      Container : in Integer;
      Contained : in Integer)
   is begin
      --  FIXME:
      null;
   end Containing_Action;

   procedure Motion_Action
     (Nonterm : in Augmented_Token'Class;
      Source  : in Augmented_Token_Array;
      Params  : in Motion_Param_Array)
   is begin
      --  FIXME:
      null;
   end Motion_Action;

   procedure Face_Apply_Action
     (Nonterm : in Augmented_Token'Class;
      Source  : in Augmented_Token_Array;
      Params  : in Face_Apply_Param_Array)
   is begin
      --  FIXME:
      null;
   end Face_Apply_Action;

   procedure Indent_Action
     (Nonterm : in Augmented_Token'Class;
      Source  : in Augmented_Token_Array;
      Params  : in Indent_Param_Array)
   is begin
      --  FIXME:
      null;
   end Indent_Action;

   function Anchored_0 (Index : in Integer; Indent_Delta : in Integer) return Integer
   is begin
      --  FIXME:
      return 0;
   end Anchored_0;

end WisiToken.Wisi_Runtime;
