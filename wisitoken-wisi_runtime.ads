--  Abstract :
--
--  Ada implementation of wisi parser actions.
--
--  References
--
--  [1] wisi.el - defines parse action functions.
--
--  [2] wisi-process-parse.el - defines elisp/process API
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

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with SAL.Gen_Unbounded_Definite_Red_Black_Trees;
with WisiToken.Lexer;
with WisiToken.Token_Line_Comment;
with WisiToken.Token_Region;
package WisiToken.Wisi_Runtime is

   type Parse_Action_Type is (Navigate, Face, Indent);

   type Parse_Data_Type is tagged limited private;

   procedure Initialize
     (Data             : in out Parse_Data_Type;
      Semantic_State   : in     WisiToken.Token_Line_Comment.State_Access;
      Lexer            : in     WisiToken.Lexer.Handle;
      Source_File_Name : in     String;
      Parse_Action     : in     Parse_Action_Type;
      Line_Count       : in     Line_Number_Type);
   --  Line_Count only used for Indent

   function Source_File_Name (Data : in Parse_Data_Type) return String;
   function Parse_Action (Data : in Parse_Data_Type) return Parse_Action_Type;

   type Navigate_Class_Type is (Motion, Name, Statement_End, Statement_Override, Statement_Start, Misc);
   --  Matches [1] wisi-class-list.

   type Index_Navigate_Class is record
      Index : Positive_Index_Type; -- into Tokens
      Class : Navigate_Class_Type;
   end record;

   type Statement_Param_Array is array (Natural range <>) of Index_Navigate_Class;

   procedure Statement_Action
     (Data    : in out Parse_Data_Type;
      Nonterm : in     Augmented_Token'Class;
      Tokens  : in     Augmented_Token_Array;
      Params  : in     Statement_Param_Array);

   procedure Containing_Action
     (Data       : in out Parse_Data_Type;
      Nonterm    : in     Augmented_Token'Class;
      Tokens     : in     Augmented_Token_Array;
      Containing : in     Positive_Index_Type;
      Contained  : in     Positive_Index_Type);

   package Token_ID_Lists is new Ada.Containers.Doubly_Linked_Lists (Token_ID);

   Empty_IDs : constant Token_ID_Lists.List := Token_ID_Lists.Empty_List;

   function "&" (List : in Token_ID_Lists.List; Item : in Token_ID) return Token_ID_Lists.List;
   function "&" (Left, Right : in Token_ID) return Token_ID_Lists.List;

   type Index_IDs is record
      Index : Positive_Index_Type; -- into Tokens
      IDs   : Token_ID_Lists.List;
   end record;

   package Index_IDs_Vectors is new Ada.Containers.Vectors (Ada.Containers.Count_Type, Index_IDs);

   subtype Motion_Param_Array is Index_IDs_Vectors.Vector;

   procedure Motion_Action
     (Data    : in out Parse_Data_Type;
      Nonterm : in     Augmented_Token'Class;
      Tokens  : in     Augmented_Token_Array;
      Params  : in     Motion_Param_Array);
   --  Implements [1] wisi-motion-action.

   type Index_Faces is record
      Index       : Positive_Index_Type; -- into Tokens
      Prefix_Face : Integer; -- into grammar.Face_List
      Suffix_Face : Integer; -- into grammar.Face_List
   end record;

   type Face_Apply_Param_Array is array (Natural range <>) of Index_Faces;

   procedure Face_Apply_Action
     (Data    : in out Parse_Data_Type;
      Nonterm : in     Augmented_Token'Class;
      Tokens  : in     Augmented_Token_Array;
      Params  : in     Face_Apply_Param_Array);
   --  Implements [1] wisi-face-apply-action.

   procedure Face_Apply_List_Action
     (Data    : in out Parse_Data_Type;
      Nonterm : in     Augmented_Token'Class;
      Tokens  : in     Augmented_Token_Array;
      Params  : in     Face_Apply_Param_Array);
   --  Implements [1] wisi-face-apply-list-action.

   type Face_Class_Type is (Prefix, Suffix);
   --  Matches wisi-cache-class values set in [1] wisi-face-apply-action.

   type Index_Face_Class is record
      Index : Positive_Index_Type; -- into Tokens
      Class : Face_Class_Type;
   end record;

   type Face_Mark_Param_Array is array (Natural range <>) of Index_Face_Class;

   procedure Face_Mark_Action
     (Data    : in out Parse_Data_Type;
      Nonterm : in     Augmented_Token'Class;
      Tokens  : in     Augmented_Token_Array;
      Params  : in     Face_Mark_Param_Array);
   --  Implements [1] wisi-face-mark-action.

   type Face_Remove_Param_Array is array (Natural range <>) of Positive_Index_Type;

   procedure Face_Remove_Action
     (Data    : in out Parse_Data_Type;
      Nonterm : in     Augmented_Token'Class;
      Tokens  : in     Augmented_Token_Array;
      Params  : in     Face_Remove_Param_Array);
   --  Implements [1] wisi-face-remove-action.

   type Indent_Param_Label is
     (Int,
      Anchored_0, -- wisi-anchored;   accumulate => False
      Anchored_1, -- wisi-anchored%;  accumulate => False
      Anchored_2, -- wisi-anchored%-; accumulate => True
      Anchored_3, -- wisi-anchored*;  accumulate => False
      Anchored_4, -- wisi-anchored*-; accumulate => True
      Hanging     -- wisi-hanging
     );
   subtype Anchored_Label is Indent_Param_Label range Anchored_0 .. Anchored_4;

   type Indent_Param_Type (Label : Indent_Param_Label := Int) is
   record
      case Label is
      when Int =>
         Int_Delta : Integer;
      when Anchored_Label =>
         Anchored_Index : Positive_Index_Type;
         Anchored_Delta : Integer;
      when Hanging =>
         Hanging_Delta : Integer;
      end case;
   end record;

   subtype Int_Indent_Param is Indent_Param_Type (Int);
   subtype Anchored_Indent_Param is Indent_Param_Type
   with Dynamic_Predicate => Anchored_Indent_Param.Label in Anchored_Label;

   Null_Indent_Param : constant Indent_Param_Type := (Int, 0);

   type Indent_Pair (Comment_Present : Boolean := False) is
   record
      Code_Delta : Indent_Param_Type;
      case Comment_Present is
      when True =>
         Comment_Delta : Indent_Param_Type;
      when False =>
         null;
      end case;
   end record;

   type Indent_Param_Array is array (Positive_Index_Type range <>) of Indent_Pair;

   procedure Indent_Action
     (Data    : in out Parse_Data_Type;
      Nonterm : in     Augmented_Token'Class;
      Tokens  : in     Augmented_Token_Array;
      Params  : in     Indent_Param_Array);
   --  Implements [1] wisi-indent-action.

   procedure Resolve_Anchors (Data : in out Parse_Data_Type);

   procedure Put (Data : in Parse_Data_Type);
   --  Put parse result to Ada.Text_IO.Standard_Output, as encoded
   --  set-text-property responses as defined in [2]
   --  wisi-process-parse--execute.

   procedure Put
     (Errors     : in WisiToken.Token_Region.Error_List_Arrays.Vector;
      Descriptor : in WisiToken.Descriptor'Class);
   --  Put Errors to Ada.Text_IO.Standard_Output, as encoded error
   --  responses as defined in [2] wisi-process-parse--execute.

   procedure Put_Error (Data : in Parse_Data_Type; Line_Number : in Line_Number_Type; Message : in String);
   --  Put an error elisp form to Ada.Text_IO.Standard_Output.

private

   type Nil_Buffer_Pos (Set : Boolean := False) is record
      case Set is
      when True =>
         Item : Buffer_Pos;
      when False =>
         null;
      end case;
   end record;

   Nil : constant Nil_Buffer_Pos := (Set => False);

   type Navigate_Cache_Type is record
      Pos            : Buffer_Pos;          -- implicit in wisi-cache
      Statement_ID   : Token_ID;            -- wisi-cache-nonterm
      ID             : Token_ID;            -- wisi-cache-token
      Length         : Natural;             -- wisi-cache-last
      Class          : Navigate_Class_Type; -- wisi-cache-class; one of wisi-class-list
      Containing_Pos : Nil_Buffer_Pos;      -- wisi-cache-containing
      Prev_Pos       : Nil_Buffer_Pos;      -- wisi-cache-prev
      Next_Pos       : Nil_Buffer_Pos;      -- wisi-cache-next
      End_Pos        : Nil_Buffer_Pos;      -- wisi-cache-end
   end record;

   function Key (Cache : in Navigate_Cache_Type) return Buffer_Pos is (Cache.Pos);

   package Navigate_Cache_Trees is new SAL.Gen_Unbounded_Definite_Red_Black_Trees (Navigate_Cache_Type, Buffer_Pos);

   type Nil_Integer (Set : Boolean := False) is record
      case Set is
      when True =>
         Item : Integer;
      when False =>
         null;
      end case;
   end record;

   type Face_Cache_Type is record
      Region : Buffer_Region;
      Class  : Face_Class_Type; -- wisi-cache-class; one of {'prefix | 'suffix}
      Face   : Nil_Integer;     -- not set, or index into *-process-faces-names
   end record;

   function Key (Cache : in Face_Cache_Type) return Buffer_Pos is (Cache.Region.First);

   package Face_Cache_Trees is new SAL.Gen_Unbounded_Definite_Red_Black_Trees (Face_Cache_Type, Buffer_Pos);

   type Indent_Label is (Not_Set, Int, Anchor, Anchored, Anchor_Anchored);

   package Int_Vectors is new Ada.Containers.Vectors (Natural, Natural);

   type Indent_Type (Label : Indent_Label := Not_Set) is record
      --  [1] wisi-ind struct. Indent values may be negative while indents
      --  are being computed.
      case Label is
      when Not_Set =>
         null;

      when Int =>
         Int_Indent : Integer;

      when Anchor =>
         Anchor_IDs    : Int_Vectors.Vector; --  Largest ID first.
         Anchor_Indent : Integer;

      when Anchored =>
         Anchored_ID    : Natural;
         Anchored_Delta : Integer; -- added to Anchor_Indent of Anchor_ID

      when Anchor_Anchored =>
         Anchor_Anchored_IDs   : Int_Vectors.Vector := Int_Vectors.Empty_Vector;
         Anchor_Anchored_ID    : Natural;
         Anchor_Anchored_Delta : Integer;
      end case;
   end record;
   First_Anchor_ID : constant Natural := 0;

   package Indent_Vectors is new Ada.Containers.Vectors (Line_Number_Type, Indent_Type);
   package Navigate_Cursor_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Navigate_Cache_Trees.Cursor, Navigate_Cache_Trees."=");

   type Parse_Data_Type is tagged limited
   record
      Semantic_State   : WisiToken.Token_Line_Comment.State_Access;
      Lexer            : WisiToken.Lexer.Handle;
      Source_File_Name : Ada.Strings.Unbounded.Unbounded_String;
      Parse_Action     : Parse_Action_Type;
      Navigate_Caches  : Navigate_Cache_Trees.Tree;  -- Used for Navigate.
      Face_Caches      : Face_Cache_Trees.Tree;      -- Used for Face.
      End_Positions    : Navigate_Cursor_Lists.List; -- Used for Navigate.
      Indents          : Indent_Vectors.Vector;      -- Used for Indent.
      Max_Anchor_ID    : Integer := First_Anchor_ID - 1;
   end record;

end WisiToken.Wisi_Runtime;
