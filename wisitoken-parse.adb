--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018 - 2022 Free Software Foundation, Inc.
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

with WisiToken.In_Parse_Actions;
package body WisiToken.Parse is

   Delims : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set (" (),");
   --  For Error_Data'Input.

   --  Body subprograms

   function Input_Op
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
     return Recover_Op
   is begin
      return Result : Recover_Op (Recover_Op_Label'Value (Next_Value (Stream, Delims))) do
         case Result.Op is
         when Fast_Forward =>
            Result.FF_First_Index := Syntax_Trees.Sequential_Index'Value (Next_Value (Stream, Delims));
            Result.FF_Next_Index := Syntax_Trees.Sequential_Index'Value (Next_Value (Stream, Delims));

         when Undo_Reduce =>
            Result.Nonterm        := Token_ID'Value (Next_Value (Stream, Delims));
            Result.Token_Count    := SAL.Base_Peek_Type'Value (Next_Value (Stream, Delims));
            Result.UR_Token_Index := Syntax_Trees.Base_Sequential_Index'Value
              (Next_Value (Stream, Delims));

         when Push_Back =>
            Result.PB_ID := Token_ID'Value (Next_Value (Stream, Delims));
            Result.PB_Token_Index := Syntax_Trees.Base_Sequential_Index'Value
              (Next_Value (Stream, Delims));

         when Insert =>
            Result.Ins_ID := Token_ID'Value (Next_Value (Stream, Delims));
            Result.Ins_Before := Syntax_Trees.Sequential_Index'Value (Next_Value (Stream, Delims));

         when Delete =>
            Result.Del_ID := Token_ID'Value (Next_Value (Stream, Delims));
            Result.Del_Token_Index := Syntax_Trees.Sequential_Index'Value (Next_Value (Stream, Delims));
         end case;

      end return;
   end Input_Op;

   function Input_Recover_Ops
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
     return Recover_Op_Arrays.Vector
   is
      Length : constant Positive_Index_Type := Positive_Index_Type'Value (Next_Value (Stream, Delims));
   begin
      return Result : Recover_Op_Arrays.Vector do
         for I in 1 .. Length loop
            Recover_Op_Arrays.Append (Result, Input_Op (Stream));
         end loop;
      end return;
   end Input_Recover_Ops;

   procedure Output_Recover_Op
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : in              Recover_Op)
   is begin
      Character'Write (Stream, '(');
      String'Write (Stream, Item.Op'Image);
      case Item.Op is
      when Fast_Forward =>
         String'Write (Stream, Item.FF_First_Index'Image);
         String'Write (Stream, Item.FF_Next_Index'Image);

      when Undo_Reduce =>
         String'Write (Stream, Item.Nonterm'Image);
         String'Write (Stream, Item.Token_Count'Image);
         String'Write (Stream, Item.UR_Token_Index'Image);

      when Push_Back =>
         String'Write (Stream, Item.PB_ID'Image);
         String'Write (Stream, Item.PB_Token_Index'Image);

      when Insert =>
         String'Write (Stream, Item.Ins_ID'Image);
         String'Write (Stream, Item.Ins_Before'Image);

      when Delete =>
         String'Write (Stream, Item.Del_ID'Image);
         String'Write (Stream, Item.Del_Token_Index'Image);
      end case;
      Character'Write (Stream, ')');
   end Output_Recover_Op;

   procedure Output_Recover_Ops
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : in              Recover_Op_Arrays.Vector)
   is
      use Recover_Op_Arrays;
   begin
      String'Write (Stream, Recover_Op_Arrays.Length (Item)'Image);
      Character'Write (Stream, '(');
      for I in First_Index (Item) .. Last_Index (Item) loop
         Output_Recover_Op (Stream, Element (Item, I));
      end loop;
      Character'Write (Stream, ')');
   end Output_Recover_Ops;

   procedure Process_Grammar_Token
     (Parser : in out Base_Parser'Class;
      Token  : in     Lexer.Token;
      Node   : in     Syntax_Trees.Valid_Node_Access)
   is
      use all type Syntax_Trees.User_Data_Access;
   begin
      if Parser.User_Data /= null then
         Parser.User_Data.Lexer_To_Augmented (Parser.Tree, Token, Node);
      end if;
   end Process_Grammar_Token;

   procedure Process_Non_Grammar_Token
     (Parser       : in out Base_Parser'Class;
      Grammar_Node : in     Syntax_Trees.Valid_Node_Access;
      Token        : in     Lexer.Token)
   is
      use all type Syntax_Trees.Node_Access;
      use all type Syntax_Trees.User_Data_Access;
   begin
      Parser.Tree.Non_Grammar_Var (Grammar_Node).Append (Token);
      if Parser.User_Data /= null then
         Parser.User_Data.Lexer_To_Augmented (Parser.Tree, Token, Grammar_Node);
      end if;
   end Process_Non_Grammar_Token;

   ----------
   --  Package public subprograms, declaration order

   overriding function Dispatch_Equal (Left : in Lexer_Error; Right : in Syntax_Trees.Error_Data'Class) return Boolean
   is
      use all type WisiToken.Lexer.Error;
   begin
      return Right in Lexer_Error and then Left.Error = Lexer_Error (Right).Error;
   end Dispatch_Equal;

   overriding function To_Message
     (Data       : in Lexer_Error;
      Tree       : in Syntax_Trees.Tree'Class;
      Error_Node : in Syntax_Trees.Valid_Node_Access)
     return Syntax_Trees.Error_Data'Class
   is begin
      return Error_Message'
        (+Image (Data, Tree, Error_Node),
         Recover_Ops  => <>,
         Recover_Cost => 0);
   end To_Message;

   overriding function Image
     (Data       : in Lexer_Error;
      Tree       : in Syntax_Trees.Tree'Class;
      Error_Node : in Syntax_Trees.Valid_Node_Access)
     return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
   begin
      Append (Result, "lexer error:" & Data.Error.Char_Pos'Image & ", '");
      for C of Data.Error.Recover_Char loop
         if C /= ASCII.NUL then
            Append (Result, C);
         end if;
      end loop;
      Append (Result, "'");
      return To_String (Result);
   end Image;

   function Input_Lexer_Error (Stream : not null access Ada.Streams.Root_Stream_Type'Class) return Lexer_Error
   is begin
      declare
         Recover_Char_Count : constant Integer := Integer'Value (Next_Value (Stream, Delims));
      begin
         return Result : Lexer_Error do
            Result.Error.Char_Pos := Buffer_Pos'Value (Next_Value (Stream, Delims));
            for I in 1 .. Recover_Char_Count loop
               Character'Read (Stream, Result.Error.Recover_Char (I));
            end loop;
         end return;
      end;
   end Input_Lexer_Error;

   procedure Output_Lexer_Error (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : in Lexer_Error)
   is
      Recover_Char_Count : Integer := 0;
   begin
      for Char of Item.Error.Recover_Char loop
         if Char /= ASCII.NUL then
            Recover_Char_Count := @ + 1;
         end if;
      end loop;
      String'Write (Stream, "(" & Trimmed_Image (Item.Error.Char_Pos) & Recover_Char_Count'Image);
      for I in 1 .. Recover_Char_Count loop
         Character'Write (Stream, Item.Error.Recover_Char (I));
      end loop;
      Character'Write (Stream, ')');
   end Output_Lexer_Error;

   overriding function Dispatch_Equal (Left : in Parse_Error; Right : in Syntax_Trees.Error_Data'Class) return Boolean
   is begin
      if not (Right in Parse_Error) then
         return False;
      else
         declare
            Right_Parse : Parse_Error renames Parse_Error (Right);
         begin
            --  Allow updating recover info after error recovery; current value
            --  may have no or different recovery information, so don't check
            --  that.
            return Left.Expecting = Right_Parse.Expecting;
         end;
      end if;
   end Dispatch_Equal;

   overriding function To_Message
     (Data       : in Parse_Error;
      Tree       : in Syntax_Trees.Tree'Class;
      Error_Node : in Syntax_Trees.Valid_Node_Access)
     return Syntax_Trees.Error_Data'Class
   is begin
      return Error_Message'
        (+Image (Data, Tree, Error_Node), Data.Recover_Ops, Data.Recover_Cost);
   end To_Message;

   overriding function Image
     (Data       : in Parse_Error;
      Tree       : in Syntax_Trees.Tree'Class;
      Error_Node : in Syntax_Trees.Valid_Node_Access)
     return String
   is
      use Syntax_Trees;
      use all type Ada.Containers.Count_Type;

      First_Term : constant Node_Access := Tree.First_Terminal (Error_Node);

      Item_Byte_Region : constant Buffer_Region :=
        (if First_Term = Invalid_Node_Access
         then Null_Buffer_Region
         else Tree.Byte_Region (First_Term, Trailing_Non_Grammar => False));

      Msg : constant String :=
        "syntax_error: expecting " & Image (Data.Expecting, Tree.Lexer.Descriptor.all) &
        ", found " &
        (if First_Term = Invalid_Node_Access
         then "empty nonterm " & Image (Tree.ID (Error_Node), Tree.Lexer.Descriptor.all)
         else "'" & Tree.Lexer.Buffer_Text (Item_Byte_Region) & "'");
   begin
      if Recover_Op_Arrays.Length (Data.Recover_Ops) /= 0 then
         return Msg & ASCII.LF & "   recovered: " & Image (Data.Recover_Ops, Tree.Lexer.Descriptor.all);
      else
         return Msg;
      end if;
   end Image;

   function Input_Parse_Error (Stream : not null access Ada.Streams.Root_Stream_Type'Class) return Parse_Error
   is begin
      declare
         First_Terminal : constant Token_ID := Token_ID'Value (Next_Value (Stream, Delims));
         Last_Terminal  : constant Token_ID := Token_ID'Value (Next_Value (Stream, Delims));

         procedure Get_Token_ID_Set (Item : in out Token_ID_Set)
         is begin
            for I in Item'Range loop
               Item (I) := Boolean'Value (Next_Value (Stream, Delims));
            end loop;
         end Get_Token_ID_Set;

      begin
         return Result : Parse_Error (First_Terminal, Last_Terminal)
         do
            Get_Token_ID_Set (Result.Expecting);
            Result.Recover_Ops  := Input_Recover_Ops (Stream);
            Result.Recover_Cost := Natural'Value (Next_Value (Stream, Delims));
         end return;
      end;
   end Input_Parse_Error;

   procedure Output_Parse_Error (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : in Parse_Error)
   is begin
      String'Write (Stream, "(" & Trimmed_Image (Item.First_Terminal) & Item.Last_Terminal'Image);
      for B of Item.Expecting loop
         String'Write (Stream, " " & B'Image);
      end loop;
      Output_Recover_Ops (Stream, Item.Recover_Ops);
      String'Write (Stream, Trimmed_Image (Item.Recover_Cost));
      Character'Write (Stream, ')');
   end Output_Parse_Error;

   overriding function Dispatch_Equal
     (Left  : in In_Parse_Action_Error;
      Right : in Syntax_Trees.Error_Data'Class)
     return Boolean
   is begin
      if not (Right in In_Parse_Action_Error) then
         return False;
      else
         declare
            use all type WisiToken.Syntax_Trees.In_Parse_Actions.Status;
            Right_In_Parse : In_Parse_Action_Error renames In_Parse_Action_Error (Right);
         begin
            --  Allow updating recover info after error recovery.
            return Left.Status = Right_In_Parse.Status;
         end;
      end if;
   end Dispatch_Equal;

   overriding function To_Message
     (Data       : in In_Parse_Action_Error;
      Tree       : in Syntax_Trees.Tree'Class;
      Error_Node : in Syntax_Trees.Valid_Node_Access)
     return Syntax_Trees.Error_Data'Class
   is begin
      return Error_Message'
        (+Image (Data, Tree, Error_Node), Data.Recover_Ops, Data.Recover_Cost);
   end To_Message;

   overriding function Image
     (Data       : in In_Parse_Action_Error;
      Tree       : in Syntax_Trees.Tree'Class;
      Error_Node : in Syntax_Trees.Valid_Node_Access)
     return String
   is
      use Ada.Strings.Unbounded;
      use all type Ada.Containers.Count_Type;

      Result : Unbounded_String;
   begin
      Result := +"in_parse_action_error: " & WisiToken.In_Parse_Actions.Image (Data.Status, Tree, Error_Node);

      if Recover_Op_Arrays.Length (Data.Recover_Ops) /= 0 then
         Append (Result, ASCII.LF & "   recovered: " & Image (Data.Recover_Ops, Tree.Lexer.Descriptor.all));
      end if;

      return -Result;
   end Image;

   function Input_In_Parse_Action_Error
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class) return In_Parse_Action_Error
   is begin
      declare
         use Syntax_Trees.In_Parse_Actions;
         Label       : constant Status_Label := Status_Label'Value (Next_Value (Stream, Delims));
      begin
         return Result : In_Parse_Action_Error
         do
            case Label is
            when Ok =>
               Result.Status := (Label => Ok);
            when Error =>
               case Error'(Label) is
               when Missing_Name_Error =>
                  Result.Status := (Label => Missing_Name_Error, others => 1);
               when Extra_Name_Error =>
                  Result.Status := (Label => Extra_Name_Error, others => 1);
               when Match_Names_Error =>
                  Result.Status := (Label => Match_Names_Error, others => 1);
               end case;

               Result.Status.Begin_Name := Positive_Index_Type'Value (Next_Value (Stream, Delims));
               Result.Status.End_Name   := Positive_Index_Type'Value (Next_Value (Stream, Delims));
            end case;
            Result.Recover_Ops  := Input_Recover_Ops (Stream);
            Result.Recover_Cost := Natural'Value (Next_Value (Stream, Delims));
         end return;
      end;
   end Input_In_Parse_Action_Error;

   procedure Output_In_Parse_Action_Error
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : in In_Parse_Action_Error)
   is
      use Syntax_Trees.In_Parse_Actions;
   begin
      String'Write (Stream, "((" & Item.Status.Label'Image);
      case Item.Status.Label is
      when Ok =>
         Character'Write (Stream, ')');
      when Error =>
         String'Write (Stream, Item.Status.Begin_Name'Image & Item.Status.End_Name'Image & ")");
      end case;

      Output_Recover_Ops (Stream, Item.Recover_Ops);
      String'Write (Stream, Trimmed_Image (Item.Recover_Cost));
      Character'Write (Stream, ')');
   end Output_In_Parse_Action_Error;

   overriding function Dispatch_Equal (Left : in Error_Message; Right : in Syntax_Trees.Error_Data'Class) return Boolean
   is begin
      if not (Right in Error_Message) then
         return False;
      else
         declare
            use all type Ada.Strings.Unbounded.Unbounded_String;
            Right_Message : Error_Message renames Error_Message (Right);
         begin
            --  Allow updating recover info after error recovery.
            return Left.Msg = Right_Message.Msg;
         end;
      end if;
   end Dispatch_Equal;

   overriding function To_Message
     (Data       : in Error_Message;
      Tree       : in Syntax_Trees.Tree'Class;
      Error_Node : in Syntax_Trees.Valid_Node_Access)
     return Syntax_Trees.Error_Data'Class
   is
      pragma Unreferenced (Tree, Error_Node);
   begin
      return Data;
   end To_Message;

   overriding function Image
     (Data       : in Error_Message;
      Tree       : in Syntax_Trees.Tree'Class;
      Error_Node : in Syntax_Trees.Valid_Node_Access)
     return String
   is begin
      return "message: " & (-Data.Msg);
   end Image;

   function Input_Error_Message (Stream : not null access Ada.Streams.Root_Stream_Type'Class) return Error_Message
   is
      Msg_Length : constant Integer := Integer'Value (Next_Value (Stream, Delims));
      Msg : String (1 .. Msg_Length);
   begin
      String'Read (Stream, Msg);
      return Result : Error_Message
      do
         Result.Msg          := Ada.Strings.Unbounded.To_Unbounded_String (Msg);
         Result.Recover_Ops  := Input_Recover_Ops (Stream);
         Result.Recover_Cost := Natural'Value (Next_Value (Stream, Delims));
      end return;
   end Input_Error_Message;

   procedure Output_Error_Message
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : in Error_Message)
   is begin
      String'Write (Stream, "(");
      String'Write (Stream, Ada.Strings.Unbounded.Length (Item.Msg)'Image);
      Character'Write (Stream, '"');
      String'Write (Stream, Ada.Strings.Unbounded.To_String (Item.Msg));
      Character'Write (Stream, '"');
      Output_Recover_Ops (Stream, Item.Recover_Ops);
      String'Write (Stream, Trimmed_Image (Item.Recover_Cost));
      Character'Write (Stream, ')');
   end Output_Error_Message;

   function Error_Pred_Parse (Cur : in Syntax_Trees.Error_Data_Lists.Cursor) return Boolean
   is
      use Syntax_Trees.Error_Data_Lists;
   begin
      return
        (if Element (Cur) in Parse_Error then True
         else False);
   end Error_Pred_Parse;

   function Error_Pred_Lexer (Cur : in Syntax_Trees.Error_Data_Lists.Cursor) return Boolean
   is
      use Syntax_Trees.Error_Data_Lists;
   begin
      return
        (if Element (Cur) in Lexer_Error then True
         else False);
   end Error_Pred_Lexer;

   function Error_Pred_Lexer_Parse_Message (Cur : in Syntax_Trees.Error_Data_Lists.Cursor) return Boolean
   is
      use Syntax_Trees.Error_Data_Lists;
   begin
      return
        (if Element (Cur) in Lexer_Error then False
         --  Lexer errors are only cleared by re-lexing in Edit_Tree.
         --  test_incremental.adb Lexer_Errors_1

         elsif Element (Cur) in Parse_Error then True
         --  A previous Parse_Error; test_incremental.adb Recover_1,
         --  test_incremental.adb Multiple_Errors_On_One_Token_1, _2,
         --  ada_mode-interactive_06.adb

         elsif Element (Cur) in Error_Message then True
         --  A moved In_Parse_Error.

         else False);
   end Error_Pred_Lexer_Parse_Message;

   function Find_Parse_In_Parse_Action_Error
     (Tree : in Syntax_Trees.Tree;
      Node : in Syntax_Trees.Valid_Node_Access)
     return Syntax_Trees.Error_Data'Class
   is
      use Syntax_Trees;
      Found : Boolean := False;
   begin
      --  test_mckenzie_recover.adb String_Quote_0 has lexer and parse error
      --  on same node. There should only be one Parse or In_Parse_Action
      --  error on a node.
      for Err of Tree.Error_List (Node) loop
         if Err in Parse_Error or Err in In_Parse_Action_Error then
            Found := True;
         end if;
      end loop;

      if not Found then
         raise SAL.Programmer_Error;
      end if;

      for Err of Tree.Error_List (Node) loop
         if Err in Parse_Error or Err in In_Parse_Action_Error then
            return Err;
         end if;
      end loop;

      raise SAL.Programmer_Error; -- keep the compiler happy
   end Find_Parse_In_Parse_Action_Error;

   function Find_Non_Lexer_Error
     (Tree : in Syntax_Trees.Tree;
      Node : in Syntax_Trees.Valid_Node_Access)
     return Syntax_Trees.Error_Data'Class
   is
      use Syntax_Trees;
      Found : Boolean := False;
   begin
      for Err of Tree.Error_List (Node) loop
         if not (Err in Lexer_Error) then
            Found := True;
         end if;
      end loop;

      if not Found then
         raise SAL.Programmer_Error;
      end if;

      for Err of Tree.Error_List (Node) loop
         if not (Err in Lexer_Error) then
            return Err;
         end if;
      end loop;

      raise SAL.Programmer_Error; -- keep the compiler happy
   end Find_Non_Lexer_Error;

   function Equal (Left : in Recover_Op; Right : in Insert_Op) return Boolean
   is
      use all type WisiToken.Syntax_Trees.Sequential_Index;
   begin
      return Left.Op = Insert and then
        Left.Ins_ID = Right.Ins_ID and then
        Left.Ins_Before = Right.Ins_Before;
   end Equal;

   function None (Ops : aliased in Recover_Op_Arrays.Vector; Op : in Recover_Op_Label) return Boolean
   is
      use Recover_Op_Arrays, Recover_Op_Array_Refs;
   begin
      for I in First_Index (Ops) .. Last_Index (Ops) loop
         if Constant_Ref (Ops, I).Op = Op then
            return False;
         end if;
      end loop;
      return True;
   end None;

   function None_Since_FF (Ops : aliased in Recover_Op_Arrays.Vector; Op : in Recover_Op_Label) return Boolean
   is
      use Recover_Op_Arrays, Recover_Op_Array_Refs;
   begin
      for I in reverse First_Index (Ops) .. Last_Index (Ops) loop
         declare
            O : Recover_Op renames Constant_Ref (Ops, I);
         begin
            exit when O.Op = Fast_Forward;
            if O.Op = Op then
               return False;
            end if;
         end;
      end loop;
      return True;
   end None_Since_FF;

   function Image (KMN : in WisiToken.Parse.KMN) return String
   is begin
      return "(" & KMN.Stable_Bytes'Image & "," &
        KMN.Stable_Chars'Image & "," &
        KMN.Inserted_Bytes'Image & "," &
        KMN.Inserted_Chars'Image & "," &
        KMN.Deleted_Bytes'Image & "," &
        KMN.Deleted_Chars'Image & ")";
   end Image;

   procedure Validate_KMN
     (KMN                       : in WisiToken.Parse.KMN;
      Initial_Stable_Byte_First : in Buffer_Pos;
      Initial_Stable_Char_First : in Buffer_Pos;
      Edited_Stable_Byte_First  : in Buffer_Pos;
      Edited_Stable_Char_First  : in Buffer_Pos;
      Initial_Text_Byte_Region  : in Buffer_Region;
      Initial_Text_Char_Region  : in Buffer_Region;
      Edited_Text_Byte_Region   : in Buffer_Region;
      Edited_Text_Char_Region   : in Buffer_Region)
   is
      Stable_Byte_Region : constant Buffer_Region :=
        (Initial_Stable_Byte_First, Initial_Stable_Byte_First + KMN.Stable_Bytes - 1);
      Stable_Char_Region : constant Buffer_Region :=
        (Initial_Stable_Char_First, Initial_Stable_Char_First + KMN.Stable_Chars - 1);

      Inserted_Byte_Region : constant Buffer_Region :=
        (Edited_Stable_Byte_First + KMN.Stable_Bytes,
         Edited_Stable_Byte_First + KMN.Stable_Bytes + KMN.Inserted_Bytes - 1);
      Inserted_Char_Region : constant Buffer_Region :=
        (Edited_Stable_Char_First + KMN.Stable_Chars,
         Edited_Stable_Char_First + KMN.Stable_Chars + KMN.Inserted_Chars - 1);

      Deleted_Byte_Region : constant Buffer_Region :=
        (Stable_Byte_Region.Last + 1, Stable_Byte_Region.Last + KMN.Deleted_Bytes);
      Deleted_Char_Region : constant Buffer_Region :=
        (Stable_Char_Region.Last + 1, Stable_Char_Region.Last + KMN.Deleted_Chars);
   begin
      if not Contains (Outer => Initial_Text_Byte_Region, Inner => Stable_Byte_Region) then
         raise User_Error with "KMN stable byte region outside initial source text";
      end if;
      if not Contains (Outer => Initial_Text_Char_Region, Inner => Stable_Char_Region) then
         raise User_Error with "KMN stable char region outside initial source text";
      end if;

      if KMN.Inserted_Bytes > 0 then
         if not Contains (Outer => Edited_Text_Byte_Region, Inner => Inserted_Byte_Region) then
            raise User_Error with "KMN inserted byte region outside initial source text";
         end if;
         if not Contains (Outer => Edited_Text_Char_Region, Inner => Inserted_Char_Region) then
            raise User_Error with "KMN inserted char region outside edited source text";
         end if;
      end if;


      if KMN.Deleted_Bytes > 0 then
         if not Contains (Outer => Initial_Text_Byte_Region, Inner => Deleted_Byte_Region) then
            raise User_Error with "KMN deleted byte region outside initial source text";
         end if;
         if not Contains (Outer => Initial_Text_Char_Region, Inner => Deleted_Char_Region) then
            raise User_Error with "KMN deleted char region outside initial source text";
         end if;
      end if;
   end Validate_KMN;

   procedure Validate_KMN
     (List                     : in KMN_Lists.List;
      Initial_Text_Byte_Region : in Buffer_Region;
      Initial_Text_Char_Region : in Buffer_Region;
      Edited_Text_Byte_Region  : in Buffer_Region;
      Edited_Text_Char_Region  : in Buffer_Region)
   is
      Initial_Byte_First : Base_Buffer_Pos := Initial_Text_Byte_Region.First;
      Initial_Char_First : Base_Buffer_Pos := Initial_Text_Char_Region.First;
      Edited_Byte_First  : Base_Buffer_Pos := Edited_Text_Byte_Region.First;
      Edited_Char_First  : Base_Buffer_Pos := Edited_Text_Char_Region.First;
   begin
      for KMN of List loop
         Validate_KMN
           (KMN,
            Initial_Stable_Byte_First => Initial_Byte_First,
            Initial_Stable_Char_First => Initial_Char_First,
            Edited_Stable_Byte_First  => Edited_Byte_First,
            Edited_Stable_Char_First  => Edited_Char_First,
            Initial_Text_Byte_Region  => Initial_Text_Byte_Region,
            Initial_Text_Char_Region  => Initial_Text_Char_Region,
            Edited_Text_Byte_Region   => Edited_Text_Byte_Region,
            Edited_Text_Char_Region   => Edited_Text_Char_Region);

         Initial_Byte_First := @ + KMN.Stable_Bytes + KMN.Deleted_Bytes;
         Initial_Char_First := @ + KMN.Stable_Chars + KMN.Deleted_Chars;
         Edited_Byte_First  := @ + KMN.Stable_Bytes + KMN.Inserted_Bytes;
         Edited_Char_First  := @ + KMN.Stable_Chars + KMN.Inserted_Chars;
      end loop;

      if Initial_Byte_First - 1 /= Initial_Text_Byte_Region.Last then
         raise User_Error with "KMN list (deleted last" & Base_Buffer_Pos'Image (Initial_Byte_First - 1) &
           ") does not match initial text (last" & Initial_Text_Byte_Region.Last'Image & ")";
      end if;
      if Edited_Byte_First - 1 /= Edited_Text_Byte_Region.Last then
         raise User_Error with "KMN list (inserted last" & Base_Buffer_Pos'Image (Edited_Byte_First - 1) &
           ") does not match edited text (last" & Edited_Text_Byte_Region.Last'Image & ")";
      end if;
   end Validate_KMN;

end WisiToken.Parse;
