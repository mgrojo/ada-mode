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

with WisiToken.Token_Line_Comment;
package body WisiToken.Wisi_Runtime is

   procedure Set_End
     (Data           : in out Buffer_Data_Type;
      Containing_Pos : in     Natural;
      End_Pos        : in     Natural)
   is
      I            : Cursor_Lists.Cursor := Data.End_Positions.First;
      Delete_Cache : Boolean;
   begin
      loop
         exit when not Cursor_Lists.Has_Element (I);
         declare
            Cache : Cache_Type renames Data.Caches (Cursor_Lists.Element (I));
         begin
            if Cache.Pos in Containing_Pos .. End_Pos then
               Cache.End_Pos := (True, End_Pos);
               Delete_Cache := True;
            else
               Delete_Cache := False;
            end if;
         end;
         if Delete_Cache then
            Cursor_Lists.Delete (Data.End_Positions, I);
         end if;

         Cursor_Lists.Next (I);
      end loop;
   end Set_End;

   ----------
   --  public subprograms

   procedure Initialize
     (Data         : in out Buffer_Data_Type;
      Parse_Action : in     Parse_Action_Type;
      Line_Count   : in     Ada.Containers.Count_Type := 0)
   is begin
      case Parse_Action is
      when Navigate | Face =>
         Data.Parse_Action := Parse_Action;
         Data.Caches.Finalize;
         Data.End_Positions.Clear;

      when Indent =>
         Data.Parse_Action := Indent;
         Data.Lines.Set_Length (Line_Count);
         for Line of Data.Lines loop
            Line :=
              (Begin_Pos => 0,
               Indent => 0);
         end loop;
      end case;
   end Initialize;

   procedure Statement_Action
     (Data    : in out Buffer_Data_Type;
      Nonterm : in     Augmented_Token'Class;
      Source  : in     Augmented_Token_Array;
      Params  : in     Statement_Param_Array)
   is
      First_Item         : Boolean     := True;
      Override_Start_Set : Boolean     := False;
      Override_Start     : Class_Type;
      Containing_Pos     : Nil_Natural := Nil; --  wisi first-keyword-pos
   begin
      for Pair of Params loop
         declare
            Token : WisiToken.Token_Line_Comment.Token renames WisiToken.Token_Line_Comment.Token
              (Source.Constant_Reference (Pair.Index).Element.all);
         begin
            if Token.Char_Region /= Null_Buffer_Region then
               declare
                  Cursor : Cache_Trees.Cursor := Data.Caches.Find (Token.Char_Region.First);
               begin
                  if Cache_Trees.Has_Element (Cursor) then
                     declare
                        Cache : Cache_Type renames Data.Caches (Cursor);
                     begin
                        Cache.Class          := (if Override_Start_Set then Override_Start else Pair.Class);
                        Cache.Statement_ID   := Nonterm.ID;
                        Cache.Containing_Pos := Containing_Pos;
                     end;
                  else
                     Cursor := Data.Caches.Insert
                       ((Label          => Navigate,
                         Pos            => Token.Char_Region.First,
                         Statement_ID   => Nonterm.ID,
                         ID             => Token.ID,
                         Length         => Length (Token.Char_Region),
                         Class          => Pair.Class,
                         Containing_Pos => Containing_Pos,
                         others         => Nil));
                  end if;

                  Data.End_Positions.Append (Cursor);
               end;

               if First_Item then
                  First_Item := False;
                  if Override_Start_Set or Pair.Class = Statement_Start then
                     Override_Start_Set := False;
                     Containing_Pos     := (True, Token.Char_Region.First);
                  end if;
               end if;

               if Pair.Class = Statement_End and Containing_Pos.Set then
                  Set_End (Data, Containing_Pos.Item, Token.Char_Region.First);
               end if;

            else
               --  Token.Region is null
               if First_Item and Pair.Class = Statement_Start then
                  --  We don't reset First_Item here; next token may also be a start, if
                  --  this one is empty.
                  Override_Start_Set := True;
                  Override_Start     := Pair.Class;
               end if;
            end if;
         end;
      end loop;
   end Statement_Action;

   procedure Containing_Action
     (Data      : in out Buffer_Data_Type;
      Nonterm   : in     Augmented_Token'Class;
      Source    : in     Augmented_Token_Array;
      Container : in     Integer;
      Contained : in     Integer)
   is begin
      --  FIXME:
      null;
   end Containing_Action;

   function "&" (List : in Token_ID_Lists.List; Item : in Token_ID) return Token_ID_Lists.List
   is begin
      return Result : Token_ID_Lists.List := List do
         Result.Append (Item);
      end return;
   end "&";

   function "&" (Left, Right : in Token_ID) return Token_ID_Lists.List
   is begin
      return Result : Token_ID_Lists.List do
         Result.Append (Left);
         Result.Append (Right);
      end return;
   end "&";

   procedure Motion_Action
     (Data    : in out Buffer_Data_Type;
      Nonterm : in     Augmented_Token'Class;
      Source  : in     Augmented_Token_Array;
      Params  : in     Motion_Param_Array)
   is begin
      --  FIXME:
      null;
   end Motion_Action;

   procedure Face_Apply_Action
     (Data    : in out Buffer_Data_Type;
      Nonterm : in     Augmented_Token'Class;
      Source  : in     Augmented_Token_Array;
      Params  : in     Face_Apply_Param_Array)
   is begin
      --  FIXME:
      null;
   end Face_Apply_Action;

   procedure Indent_Action
     (Data    : in out Buffer_Data_Type;
      Nonterm : in     Augmented_Token'Class;
      Source  : in     Augmented_Token_Array;
      Params  : in     Indent_Param_Array)
   is begin
      --  FIXME:
      null;
   end Indent_Action;

   function Anchored_0
     (Data         : in out Buffer_Data_Type;
      Index        : in     Integer;
      Indent_Delta : in     Integer)
     return Integer
   is
      pragma Unreferenced (Indent_Delta);
      pragma Unreferenced (Index);
      pragma Unreferenced (Data);
   begin
      --  FIXME:
      return 0;
   end Anchored_0;

end WisiToken.Wisi_Runtime;
