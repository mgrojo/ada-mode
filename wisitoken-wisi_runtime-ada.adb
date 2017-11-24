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

with Ada.Containers;
with Ada.Strings.Fixed;
with Ada_Process; -- FIXME: module? move token_enum_id (and more?) to separate package?
package body WisiToken.Wisi_Runtime.Ada is

   function Find_ID_On_Stack
     (Data : in Wisi_Runtime.Parse_Data_Type'Class;
      ID   : in Token_ID)
     return Token_Line_Comment.Token
   is begin
      for Tok of reverse Data.Semantic_State.Stack loop
         if Tok.ID = ID then
            return Token_Line_Comment.Token (Tok);
         end if;
      end loop;
      raise Programmer_Error with Image (Data.Semantic_State.Trace.Descriptor.all, ID) & " not found on parse stack";
   end Find_ID_On_Stack;

   function Indent_Record
     (Data            : in out Parse_Data_Type;
      Anchor_Token    : in     Token_Line_Comment.Token;
      Record_Token    : in     Token_Line_Comment.Token;
      Indenting_Token : in     Token_Line_Comment.Token;
      Offset          : in     Integer)
     return WisiToken.Wisi_Runtime.Delta_Type
   is
   begin
      --  [1] ada-wisi-elisp-parse--indent-record-1, modified to use
      --  Indenting_Token instead of relying on the usage in ada.wy.

      if Anchor_Token.Virtual or Record_Token.Virtual or Indenting_Token.Virtual then
         return Null_Delta;
      end if;

      if Indenting_Token.ID = Data.Record_ID then
         --  Indenting 'record'
         return Indent_Anchored_2
           (Data, Anchor_Token.Line, Record_Token.Last_Line (Data.Indenting_Comment), Ada_Indent_Record_Rel_Type,
            Accumulate => True);

      else
         --  Indenting comment, component or 'end'
         --
         --  Ensure 'record' line is anchored.
         if not (Data.Indents (Record_Token.Line).Label = Anchor or
                   Data.Indents (Record_Token.Line).Label = Anchor_Anchored)
         then
            if Anchor_Token.Line /= Record_Token.Line then
               Indent_Token_1
                 (Data,
                  Record_Token.First_Line (Data.Indenting_Comment),
                  Record_Token.Last_Line (Data.Indenting_Comment),
                  Indent_Anchored_2
                    (Data, Anchor_Token.Line,
                     Record_Token.Last_Line (Data.Indenting_Comment),
                     Ada_Indent_Record_Rel_Type,
                     Accumulate => True));
            end if;
         end if;

         --  from [2] wisi-elisp-parse--anchored-1
         return Indent_Anchored_2
           (Data,
            Anchor_Line => Anchor_Token.Line,
            Last_Line   => Indenting_Token.Last_Line (Data.Indenting_Comment),
            Offset      => Current_Indent_Offset
              (Data, Anchor_Token,
               Offset   =>
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
        (Wisi_Runtime.Parse_Data_Type (Data), Semantic_State, Source_File_Name, Parse_Action, Line_Count, "");

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
   is
      pragma Unreferenced (Args);
      pragma Unreferenced (Indenting);
      pragma Unreferenced (Tokens);
      use Ada_Process;

      function Peek (N : in Positive_Index_Type) return Token_Enum_ID
      is
         use all type Standard.Ada.Containers.Count_Type;
      begin
         return -Data.Semantic_State.Stack (Data.Semantic_State.Stack.Last_Index - N).ID;
      end Peek;

   begin
      --  [1] ada-indent-aggregate
      case Peek (1) is
      when ELSE_ID =>
         case Peek (2) is
         when OR_ID =>
            return Null_Delta;
         when others =>
            return (Simple, (Int, Ada_Indent_Broken - Ada_Indent));
         end case;

      when EQUAL_GREATER_ID =>
         case Peek (3) is
         when WHEN_ID =>
            return (Simple, (Int, Ada_Indent_Broken - Ada_Indent));
         when others =>
            return Null_Delta;
         end case;

      when THEN_ID =>
         case Peek (2) is
         when AND_ID =>
            return Null_Delta;
         when others =>
            return (Simple, (Int, Ada_Indent_Broken - Ada_Indent));
         end case;

      when others =>
         return Null_Delta;
      end case;
   end Ada_Indent_Aggregate;

   function Ada_Indent_Renames_0
     (Data      : in out Wisi_Runtime.Parse_Data_Type'Class;
      Tokens    : in     Augmented_Token_Array;
      Indenting : in     Token_Line_Comment.Token;
      Args      : in     WisiToken.Wisi_Runtime.Indent_Arg_Arrays.Vector)
     return WisiToken.Wisi_Runtime.Delta_Type
   is
      use all type Standard.Ada.Containers.Count_Type;
      Subp_Tok    : Token_Line_Comment.Token renames Token_Line_Comment.Token
        (Tokens (Positive_Index_Type (Args (1).Element.all)).Element.all);
      Renames_Tok : Token_Line_Comment.Token renames Indenting;
      Paren_I     : constant Standard.Ada.Containers.Count_Type := Token_Line_Comment.Find
        (Data.Semantic_State.Grammar_Tokens, Data.Semantic_State.Trace.Descriptor.Left_Paren_ID, Subp_Tok.Char_Region);
   begin
      if Paren_I /= Tokens.First_Index - 1 then
         --  paren is present
         declare
            Paren_Tok : Token_Line_Comment.Token renames Token_Line_Comment.Token (Tokens (Paren_I).Element.all);
         begin
            if Ada_Indent_Renames > 0 then
               return Indent_Anchored_2
                 (Data,
                  Anchor_Line => Paren_Tok.Line,
                  Last_Line   => Renames_Tok.Last_Line (Data.Indenting_Comment),
                  Offset      => Current_Indent_Offset (Data, Paren_Tok, abs Ada_Indent_Renames),
                  Accumulate  => True);
            else
               return Indent_Anchored_2
                 (Data,
                  Anchor_Line => Subp_Tok.Line,
                  Last_Line   => Renames_Tok.Last_Line (Data.Indenting_Comment),
                  Offset      => Ada_Indent_Renames,
                  Accumulate  => True);
            end if;
         end;
      else
         return Indent_Anchored_2
           (Data,
            Anchor_Line => Subp_Tok.Line,
            Last_Line   => Renames_Tok.Last_Line (Data.Indenting_Comment),
            Offset      => Ada_Indent_Broken,
            Accumulate  => True);
      end if;
   end Ada_Indent_Renames_0;

   function Ada_Indent_Return_0
     (Data      : in out Wisi_Runtime.Parse_Data_Type'Class;
      Tokens    : in     Augmented_Token_Array;
      Indenting : in     Token_Line_Comment.Token;
      Args      : in     WisiToken.Wisi_Runtime.Indent_Arg_Arrays.Vector)
     return WisiToken.Wisi_Runtime.Delta_Type
   is
      use all type Ada_Process.Token_Enum_ID;
   begin
      --  'return-tok' is Indenting,
      if Indenting.Line = Indenting.First_Indent_Line then
         if Ada_Indent_Return = 0 then
            return Indent_Anchored_2
              (Data,
               Anchor_Line => Token_Line_Comment.Token
                 (Tokens (Positive_Index_Type (Args (1).Element.all)).Element.all).Line,
               Last_Line   => Indenting.Last_Line (Data.Indenting_Comment),
               Offset      => Args (2) + abs Ada_Indent_Return,
               Accumulate  => True);
         else
            return Indent_Anchored_2
              (Data,
               Anchor_Line => Find_ID_On_Stack (Data, +FUNCTION_ID).Line,
               Last_Line   => Indenting.Last_Line (Data.Indenting_Comment),
               Offset      => Args (2) + abs Ada_Indent_Return,
               Accumulate  => True);
         end if;

      else
         return Null_Delta;
      end if;
   end Ada_Indent_Return_0;

   function Ada_Indent_Record_0
     (Data      : in out Wisi_Runtime.Parse_Data_Type'Class;
      Tokens    : in     Augmented_Token_Array;
      Indenting : in     Token_Line_Comment.Token;
      Args      : in     WisiToken.Wisi_Runtime.Indent_Arg_Arrays.Vector)
     return WisiToken.Wisi_Runtime.Delta_Type
   is begin
      return Indent_Record
        (Parse_Data_Type (Data),
         Anchor_Token    => Token_Line_Comment.Token (Tokens (Positive_Index_Type (Integer'(Args (1)))).Element.all),
         Record_Token    => Token_Line_Comment.Token (Tokens (Positive_Index_Type (Integer'(Args (2)))).Element.all),
         Offset          => Args (3),
         Indenting_Token => Indenting);
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
