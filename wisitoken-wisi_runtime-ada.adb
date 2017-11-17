--  Abstract :
--
--  see spec.
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

with Ada.Strings.Fixed;
package body WisiToken.Wisi_Runtime.Ada is

   function Indent_Record
     (Data            : in out Parse_Data_Type;
      Anchor_Token    : in     Token_Line_Comment.Token;
      Record_Token    : in     Token_Line_Comment.Token;
      Indenting_Token : in     Token_Line_Comment.Token;
      Offset          : in     Integer)
     return WisiToken.Wisi_Runtime.Delta_Type
   is
      Delta_Indent : constant Delta_Type := Indent_Anchored_2
        (Data, Anchor_Token.Line, Record_Token.Last_Indent_Line, Ada_Indent_Record_Rel_Type,
         Accumulate => True);
   begin
      --  [1] ada-wisi-elisp-parse--indent-record-1, modified to use
      --  Indenting_Token instead of relying on the usage in ada.wy.

      if Anchor_Token.Virtual or Record_Token.Virtual or Indenting_Token.Virtual then
         return Null_Delta;
      end if;

      if Indenting_Token.ID = Data.Record_ID then
         --  Indenting 'record'
         return Delta_Indent;

      else
         --  Indenting comment, component or 'end'
         --
         --  Ensure 'record' line is anchored.
         if not (Data.Indents (Record_Token.Line).Label = Anchor or
                   Data.Indents (Record_Token.Line).Label = Anchor_Anchored)
         then
            if Anchor_Token.Line /= Record_Token.Line then
               Indent_Token_1 (Data, Record_Token.First_Indent_Line, Record_Token.Last_Indent_Line, Delta_Indent);
            end if;
         end if;

         --  from [2] wisi-elisp-parse--anchored-1
         return Indent_Anchored_2
           (Data,
            Anchor_Line => Anchor_Token.Line,
            Last_Line   =>
              (if Data.Indenting_Comment
               then Indenting_Token.Last_Trailing_Comment_Line
               else Indenting_Token.Last_Indent_Line),
            Offset     => Current_Indent_Offset
              (Data, Anchor_Token,
               Offset =>
                 (if Anchor_Token.Line = Record_Token.Line
                  then Offset
                  else Offset + Ada_Indent_Record_Rel_Type)),
            Accumulate => True);
      end if;
   end Indent_Record;

   ----------
   --  Public subprograms

   overriding
   procedure Initialize
     (Data             : in out Parse_Data_Type;
      Semantic_State   : in     WisiToken.Token_Line_Comment.State_Access;
      Lexer            : in     WisiToken.Lexer.Handle;
      Source_File_Name : in     String;
      Parse_Action     : in     Parse_Action_Type;
      Line_Count       : in     Line_Number_Type;
      Params           : in     String)
   is
      use Standard.Ada.Strings.Fixed;
      First : Integer := Params'First;
      Last  : Integer := Index (Params, " ");
   begin
      Wisi_Runtime.Initialize
        (Wisi_Runtime.Parse_Data_Type (Data), Semantic_State, Lexer, Source_File_Name, Parse_Action, Line_Count, "");

      if Params /= "" then
         Ada_Indent := Integer'Value (Params (First .. Last - 1));

         First := Last + 1;
         Last := Index (Params, " ", First);
         Ada_Indent_Broken := Integer'Value (Params (First .. Last - 1));

         First := Last + 1;
         Last := First + 1;
         Ada_Indent_Comment_Col_0 := Params (First) = '1';

         First := Last + 1;
         Last := First + 1;
         Ada_Indent_Comment_GNAT := Params (First) = '1';

         First := Last + 1;
         Last := Index (Params, " ", First);
         Ada_Indent_Label := Integer'Value (Params (First .. Last - 1));

         First := Last + 1;
         Last := Index (Params, " ", First);
         Ada_Indent_Record_Rel_Type := Integer'Value (Params (First .. Last - 1));

         First := Last + 1;
         Last := Index (Params, " ", First);
         Ada_Indent_Renames := Integer'Value (Params (First .. Last - 1));

         First := Last + 1;
         Last := Index (Params, " ", First);
         Ada_Indent_Return := Integer'Value (Params (First .. Last - 1));

         First := Last + 1;
         Last := Index (Params, " ", First);
         Ada_Indent_Use := Integer'Value (Params (First .. Last - 1));

         First := Last + 1;
         Last := Index (Params, " ", First);
         Ada_Indent_When := Integer'Value (Params (First .. Last - 1));

         First := Last + 1;
         Last := Index (Params, " ", First);
         Ada_Indent_With := Integer'Value (Params (First .. Last - 1));

         First := Last + 1;
         Ada_Indent_Hanging_Rel_Exp := Params (First) = '1';
      end if;

      Data.Record_ID := Find_ID (Data.Semantic_State.Trace.Descriptor.all, "RECORD");
   end Initialize;

   function Ada_Indent_Aggregate
     (Data      : in out Wisi_Runtime.Parse_Data_Type'Class;
      Tokens    : in     Augmented_Token_Array;
      Indenting : in     Token_Line_Comment.Token;
      Args      : in     WisiToken.Wisi_Runtime.Indent_Arg_Arrays.Vector)
     return WisiToken.Wisi_Runtime.Delta_Type
   is begin
      raise SAL.Not_Implemented;
      return Null_Delta;
   end Ada_Indent_Aggregate;

   function Ada_Indent_Renames_0
     (Data      : in out Wisi_Runtime.Parse_Data_Type'Class;
      Tokens    : in     Augmented_Token_Array;
      Indenting : in     Token_Line_Comment.Token;
      Args      : in     WisiToken.Wisi_Runtime.Indent_Arg_Arrays.Vector)
     return WisiToken.Wisi_Runtime.Delta_Type
   is begin
      raise SAL.Not_Implemented;
      return Null_Delta;
   end Ada_Indent_Renames_0;

   function Ada_Indent_Return_0
     (Data      : in out Wisi_Runtime.Parse_Data_Type'Class;
      Tokens    : in     Augmented_Token_Array;
      Indenting : in     Token_Line_Comment.Token;
      Args      : in     WisiToken.Wisi_Runtime.Indent_Arg_Arrays.Vector)
     return WisiToken.Wisi_Runtime.Delta_Type
   is begin
      raise SAL.Not_Implemented;
      return Null_Delta;
   end Ada_Indent_Return_0;

   function Ada_Indent_Record_0
     (Data      : in out Wisi_Runtime.Parse_Data_Type'Class;
      Tokens    : in     Augmented_Token_Array;
      Indenting : in     Token_Line_Comment.Token;
      Args      : in     WisiToken.Wisi_Runtime.Indent_Arg_Arrays.Vector)
     return WisiToken.Wisi_Runtime.Delta_Type
   is begin
      raise SAL.Not_Implemented;
      return Null_Delta;
   end Ada_Indent_Record_0;

   function Ada_Indent_Record_1
     (Data      : in out Wisi_Runtime.Parse_Data_Type'Class;
      Tokens    : in     Augmented_Token_Array;
      Indenting : in     Token_Line_Comment.Token;
      Args      : in     WisiToken.Wisi_Runtime.Indent_Arg_Arrays.Vector)
   return WisiToken.Wisi_Runtime.Delta_Type
   is
      Anchor_Tok : Token_Line_Comment.Token renames Find_Token_On_Stack (Data, Token_ID (Integer'(Args (1))));
      Record_Tok : Token_Line_Comment.Token renames Token_Line_Comment.Token
        (Tokens (Positive_Index_Type (Integer'(Args (2)))).Element.all);
   begin
      return Indent_Record (Parse_Data_Type (Data), Anchor_Tok, Record_Tok, Indenting, Args (3));
   end Ada_Indent_Record_1;

end WisiToken.Wisi_Runtime.Ada;
