--  Abstract :
--
--  Subprograms common to more than one parser, higher-level than in wisitoken.ads
--
--  The parser type is declared in wisitoken-parse-parser.ads, so it
--  can combine packrat and LR, for packrat error recovery that uses
--  LR.
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

with Ada.Streams;
with SAL.Gen_Bounded_Definite_Vectors.Gen_Image_Aux;
with SAL.Gen_Bounded_Definite_Vectors.Gen_Refs;
with SAL.Gen_Definite_Doubly_Linked_Lists.Gen_Image;
with WisiToken.Lexer;
with WisiToken.Syntax_Trees;
package WisiToken.Parse is

   type Recover_Op_Label is (Fast_Forward, Undo_Reduce, Push_Back, Insert, Delete);
   subtype Insert_Delete_Op_Label is Recover_Op_Label range Insert .. Delete;
   --  Fast_Forward is a placeholder to mark a fast_forward parse; that
   --  resets what operations are allowed to be done on a config.
   --
   --  Undo_Reduce is the inverse of Reduce.
   --
   --  Push_Back pops the top stack item, and moves the input stream
   --  pointer back to the first shared_terminal contained by that item.
   --
   --  Insert inserts a new token in the token input stream, before the
   --  given point in Terminals.
   --
   --  Delete deletes one item from the token input stream, at the given
   --  point.

   --  WORKAROUND: GNAT Community 2020 with -gnat2020 S'Image outputs
   --  integer when S is a subtype. Fixed in Community 2021.
   function Image (Item : in Recover_Op_Label) return String
   is (case Item is
       when Fast_Forward => "FAST_FORWARD",
       when Undo_Reduce  => "UNDO_REDUCE",
       when Push_Back    => "PUSH_BACK",
       when Insert       => "INSERT",
       when Delete       => "DELETE");

   type Recover_Op (Op : Recover_Op_Label := Fast_Forward) is record
      --  An error recover operation. We store enough information to perform
      --  the operation on the main parser stack and input stream when error
      --  recover succeeds.

      case Op is
      when Fast_Forward =>
         FF_First_Index : Syntax_Trees.Sequential_Index;
         --  First token in fast forward region

         FF_Next_Index : Syntax_Trees.Sequential_Index;
         --  Config current_token after the operation is done; next after the
         --  last token in the fast forward region.
         --
         --  If FF_First_Index = FF_Next_Index, no tokens were actually parsed
         --  for the fast_forward; it is just a marker to allow error recovery
         --  to reset op order restrictions.

      when Undo_Reduce =>
         Nonterm : Token_ID;
         --  The nonterminal popped off the stack.

         Token_Count : SAL.Base_Peek_Type;
         --  The number of tokens pushed on the stack.

         UR_Token_Index : Syntax_Trees.Base_Sequential_Index;
         --  First terminal in the undo_reduce token; Invalid_Sequential_Index if
         --  empty. Used to check that successive Undo_Reduce are valid.

      when Push_Back =>
         PB_ID : Token_ID;
         --  The nonterm ID popped off the stack.

         PB_Token_Index : Syntax_Trees.Base_Sequential_Index;
         --  First terminal in the pushed_back token; Invalid_Sequential_Index if
         --  empty. Used to check that successive Push_Backs are valid.

      when Insert =>
         Ins_ID : Token_ID;
         --  The token ID inserted.

         Ins_Before : Syntax_Trees.Sequential_Index;
         --  Ins_ID is inserted before Ins_Before.

      when Delete =>
         Del_ID : Token_ID;
         --  The token ID deleted; a terminal token.

         Del_Token_Index : Syntax_Trees.Sequential_Index;
         --  Token at Del_Token_Index is deleted.

      end case;
   end record;
   subtype Insert_Delete_Op is Recover_Op with Dynamic_Predicate => (Insert_Delete_Op.Op in Insert_Delete_Op_Label);
   subtype Insert_Op is Recover_Op with Dynamic_Predicate => (Insert_Op.Op = Insert);

   function Token_Index (Op : in Insert_Delete_Op) return Syntax_Trees.Sequential_Index
   is (case Insert_Delete_Op_Label'(Op.Op) is
       when Insert => Op.Ins_Before,
       when Delete => Op.Del_Token_Index);

   function ID (Op : in Insert_Delete_Op) return WisiToken.Token_ID
   is (case Insert_Delete_Op_Label'(Op.Op) is
       when Insert => Op.Ins_ID,
       when Delete => Op.Del_ID);

   function Equal (Left : in Recover_Op; Right : in Insert_Op) return Boolean;

   package Recover_Op_Arrays is new SAL.Gen_Bounded_Definite_Vectors
     (Positive_Index_Type, Recover_Op, Default_Element =>
        (Fast_Forward, Syntax_Trees.Sequential_Index'Last, Syntax_Trees.Sequential_Index'First), Capacity => 80);
   --  Using a fixed size vector significantly speeds up
   --  McKenzie_Recover. The capacity is determined by the maximum number
   --  of repair operations, which is limited by the cost_limit McKenzie
   --  parameter plus an arbitrary number from the language-specific
   --  repairs; in practice, a capacity of 80 is enough so far. If a
   --  config does hit that limit, it is abandoned; some other config is
   --  likely to be cheaper.

   package Recover_Op_Array_Refs is new Recover_Op_Arrays.Gen_Refs;

   function Recover_Op_Image (Item : in Recover_Op; Descriptor : in WisiToken.Descriptor) return String
   is ("(" & Image (Item.Op) & ", " &
         (case Item.Op is
          when Fast_Forward => Syntax_Trees.Trimmed_Image (Item.FF_First_Index) & ", " &
            Syntax_Trees.Trimmed_Image (Item.FF_Next_Index),
          when Undo_Reduce  => Image (Item.Nonterm, Descriptor) & "," &
            Item.Token_Count'Image & ", " & Syntax_Trees.Trimmed_Image (Item.UR_Token_Index),
          when Push_Back    => Image (Item.PB_ID, Descriptor) & ", " & Syntax_Trees.Trimmed_Image (Item.PB_Token_Index),
          when Insert       => Image (Item.Ins_ID, Descriptor) & ", " & Syntax_Trees.Trimmed_Image (Item.Ins_Before),
          when Delete       => Image (Item.Del_ID, Descriptor) & ", " &
            Syntax_Trees.Trimmed_Image (Item.Del_Token_Index))
         & ")");

   function Image (Item : in Recover_Op; Descriptor : in WisiToken.Descriptor) return String
     renames Recover_Op_Image;

   function Recover_Op_Array_Image is new Recover_Op_Arrays.Gen_Image_Aux (WisiToken.Descriptor, Image);
   function Image (Item : in Recover_Op_Arrays.Vector; Descriptor : in WisiToken.Descriptor) return String
     renames Recover_Op_Array_Image;

   function None (Ops : aliased in Recover_Op_Arrays.Vector; Op : in Recover_Op_Label) return Boolean;
   --  True if Ops contains no Op.

   function None_Since_FF (Ops : aliased in Recover_Op_Arrays.Vector; Op : in Recover_Op_Label) return Boolean;
   --  True if Ops contains no Op after the last Fast_Forward (or ops.first, if
   --  no Fast_Forward).

   type Lexer_Error is new Syntax_Trees.Error_Data with record
      Error : WisiToken.Lexer.Error;
   end record;

   overriding function Dispatch_Equal (Left : in Lexer_Error; Right : in Syntax_Trees.Error_Data'Class) return Boolean;
   overriding function To_Message
     (Data       : in Lexer_Error;
      Tree       : in Syntax_Trees.Tree'Class;
      Error_Node : in Syntax_Trees.Valid_Node_Access)
     return Syntax_Trees.Error_Data'Class;

   overriding function Image
     (Data       : in Lexer_Error;
      Tree       : in Syntax_Trees.Tree'Class;
      Error_Node : in Syntax_Trees.Valid_Node_Access)
     return String;

   overriding function Class_Image (Data : in Lexer_Error) return String is ("lexer");

   function Input_Lexer_Error (Stream : not null access Ada.Streams.Root_Stream_Type'Class) return Lexer_Error;
   --  Raises ada.streams.stream_IO.End_Error when first stream element read is ')'
   procedure Output_Lexer_Error (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : in Lexer_Error);
   for Lexer_Error'Input use Input_Lexer_Error;
   for Lexer_Error'Output use Output_Lexer_Error;

   type Parse_Error
     (First_Terminal : Token_ID;
      Last_Terminal  : Token_ID)
   is new Syntax_Trees.Error_Data with record
      Expecting    : Token_ID_Set (First_Terminal .. Last_Terminal);
      Recover_Ops  : Recover_Op_Arrays.Vector;
      --  FIXME: parse_Error does not need recover_op, just insert/delete?
      Recover_Cost : Natural := 0;
   end record;

   overriding function Dispatch_Equal (Left : in Parse_Error; Right : in Syntax_Trees.Error_Data'Class) return Boolean;
   overriding function To_Message
     (Data       : in Parse_Error;
      Tree       : in Syntax_Trees.Tree'Class;
      Error_Node : in Syntax_Trees.Valid_Node_Access)
     return Syntax_Trees.Error_Data'Class;

   overriding function Image
     (Data       : in Parse_Error;
      Tree       : in Syntax_Trees.Tree'Class;
      Error_Node : in Syntax_Trees.Valid_Node_Access)
     return String;

   overriding function Class_Image (Data : in Parse_Error) return String is ("parser");

   function Input_Parse_Error (Stream : not null access Ada.Streams.Root_Stream_Type'Class) return Parse_Error;
   --  Raises ada.streams.stream_IO.End_Error when first stream element read is ')'

   procedure Output_Parse_Error (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : in Parse_Error);
   for Parse_Error'Input use Input_Parse_Error;
   for Parse_Error'Output use Output_Parse_Error;

   type In_Parse_Action_Error is new Syntax_Trees.Error_Data with record
      Status       : WisiToken.Syntax_Trees.In_Parse_Actions.Status;
      Recover_Ops  : Recover_Op_Arrays.Vector;
      Recover_Cost : Natural := 0;
   end record;

   overriding function Dispatch_Equal
     (Left  : in In_Parse_Action_Error;
      Right : in Syntax_Trees.Error_Data'Class)
     return Boolean;
   overriding function To_Message
     (Data       : in In_Parse_Action_Error;
      Tree       : in Syntax_Trees.Tree'Class;
      Error_Node : in Syntax_Trees.Valid_Node_Access)
     return Syntax_Trees.Error_Data'Class;

   overriding function Image
     (Data       : in In_Parse_Action_Error;
      Tree       : in Syntax_Trees.Tree'Class;
      Error_Node : in Syntax_Trees.Valid_Node_Access)
     return String;

   overriding function Class_Image (Data : in In_Parse_Action_Error) return String is ("in_parse_action");

   function Input_In_Parse_Action_Error
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class) return In_Parse_Action_Error;
   --  Raises ada.streams.stream_IO.End_Error when first stream element read is ')'
   procedure Output_In_Parse_Action_Error
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : in In_Parse_Action_Error);
   for In_Parse_Action_Error'Input use Input_In_Parse_Action_Error;
   for In_Parse_Action_Error'Output use Output_In_Parse_Action_Error;

   type Error_Message is new Syntax_Trees.Error_Data with record
      Msg          : Ada.Strings.Unbounded.Unbounded_String;
      Recover_Ops  : Recover_Op_Arrays.Vector;
      Recover_Cost : Natural := 0;
   end record;

   overriding function Dispatch_Equal
     (Left  : in Error_Message;
      Right : in Syntax_Trees.Error_Data'Class)
     return Boolean;
   overriding function To_Message
     (Data       : in Error_Message;
      Tree       : in Syntax_Trees.Tree'Class;
      Error_Node : in Syntax_Trees.Valid_Node_Access)
     return Syntax_Trees.Error_Data'Class;

   overriding function Image
     (Data       : in Error_Message;
      Tree       : in Syntax_Trees.Tree'Class;
      Error_Node : in Syntax_Trees.Valid_Node_Access)
     return String;

   overriding function Class_Image (Data : in Error_Message) return String is ("message");

   function Input_Error_Message (Stream : not null access Ada.Streams.Root_Stream_Type'Class) return Error_Message;
   --  Raises ada.streams.stream_IO.End_Error when first stream element read is ')'
   procedure Output_Error_Message
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : in Error_Message);
   for Error_Message'Input use Input_Error_Message;
   for Error_Message'Output use Output_Error_Message;

   function Error_Pred_Parse (Cur : in Syntax_Trees.Error_Data_Lists.Cursor) return Boolean;
   --  Return True if Cur is a Parse_Error; for
   --  Syntax_Trees.Error_Predicate.

   function Error_Pred_Lexer (Cur : in Syntax_Trees.Error_Data_Lists.Cursor) return Boolean;
   --  Return True if Cur is a Lexer_Error; for
   --  Syntax_Trees.Error_Predicate.

   function Error_Pred_Lexer_Parse_Message (Cur : in Syntax_Trees.Error_Data_Lists.Cursor) return Boolean;
   --  Return True if Cur is one of Lexer_Error, Parse_Error, or
   --  Error_Message; for Syntax_Trees.Error_Predicate.

   function Find_Parse_In_Parse_Action_Error
     (Tree : in Syntax_Trees.Tree;
      Node : in Syntax_Trees.Valid_Node_Access)
     return Syntax_Trees.Error_Data'Class;
   --  Return a Parse_Error or In_Parse_Action_Error from Node.

   function Find_Non_Lexer_Error
     (Tree : in Syntax_Trees.Tree;
      Node : in Syntax_Trees.Valid_Node_Access)
     return Syntax_Trees.Error_Data'Class;

   type KMN is record
      --  Similar to [Lahav 2004] page 6; describes changed and unchanged
      --  regions in a text buffer. We assume the range boundaries do not
      --  break a multi-byte character.

      Stable_Bytes : Zero_Buffer_Pos; -- Count of unmodified bytes before change
      Stable_Chars : Zero_Buffer_Pos; -- "" characters

      Inserted_Bytes : Zero_Buffer_Pos; -- Count of inserted bytes, after Stable.
      Inserted_Chars : Zero_Buffer_Pos;

      Deleted_Bytes : Zero_Buffer_Pos; -- Count of deleted bytes, after Stable
      Deleted_Chars : Zero_Buffer_Pos;
   end record;

   Invalid_KMN : constant KMN := (others => 0);

   function Image (KMN : in WisiToken.Parse.KMN) return String;

   procedure Validate_KMN
     (KMN                      : in WisiToken.Parse.KMN;
      Initial_Stable_Byte_First : in Buffer_Pos;
      Initial_Stable_Char_First : in Buffer_Pos;
      Edited_Stable_Byte_First  : in Buffer_Pos;
      Edited_Stable_Char_First  : in Buffer_Pos;
      Initial_Text_Byte_Region  : in Buffer_Region;
      Initial_Text_Char_Region  : in Buffer_Region;
      Edited_Text_Byte_Region   : in Buffer_Region;
      Edited_Text_Char_Region   : in Buffer_Region);
   --  Raise User_Error if KMN violates text regions.

   package KMN_Lists is new SAL.Gen_Definite_Doubly_Linked_Lists (KMN);

   function Image is new KMN_Lists.Gen_Image (Image);

   procedure Validate_KMN
     (List                     : in KMN_Lists.List;
      Initial_Text_Byte_Region : in Buffer_Region;
      Initial_Text_Char_Region : in Buffer_Region;
      Edited_Text_Byte_Region  : in Buffer_Region;
      Edited_Text_Char_Region  : in Buffer_Region);

end WisiToken.Parse;
