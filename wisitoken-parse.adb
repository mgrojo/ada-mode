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
