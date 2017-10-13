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
with WisiToken.Token_Region;
package WisiToken.Wisi_Runtime is

   type Parse_Action_Type is (Navigate, Face, Indent);

   type Base_Data_Type is tagged limited record
      Descriptor       : access constant WisiToken.Descriptor'Class;
      Lexer            : WisiToken.Lexer.Handle;
      Source_File_Name : Ada.Strings.Unbounded.Unbounded_String;
      Parse_Action     : Parse_Action_Type;
   end record;

   type Parse_Data_Type is new Base_Data_Type with private;

   procedure Initialize
     (Data         : in out Parse_Data_Type;
      Parse_Action : in     Parse_Action_Type;
      Line_Count   : in     Ada.Containers.Count_Type := 0);
   --  Line_Count only used for Indent

   type Class_Type is (Motion, Name, Statement_End, Statement_Override, Statement_Start, Misc);
   --  Matches [1] wisi-class-list.

   type Index_Class is record
      Index : Positive_Index_Type; -- into Source
      Class : Class_Type;
   end record;

   type Statement_Param_Array is array (Natural range <>) of Index_Class;

   procedure Statement_Action
     (Data    : in out Parse_Data_Type;
      Nonterm : in     Augmented_Token'Class;
      Source  : in     Augmented_Token_Array;
      Params  : in     Statement_Param_Array);

   procedure Containing_Action
     (Data       : in out Parse_Data_Type;
      Nonterm    : in     Augmented_Token'Class;
      Source     : in     Augmented_Token_Array;
      Containing : in     Positive_Index_Type;
      Contained  : in     Positive_Index_Type);

   package Token_ID_Lists is new Ada.Containers.Doubly_Linked_Lists (Token_ID);

   Empty_IDs : constant Token_ID_Lists.List := Token_ID_Lists.Empty_List;

   function "&" (List : in Token_ID_Lists.List; Item : in Token_ID) return Token_ID_Lists.List;
   function "&" (Left, Right : in Token_ID) return Token_ID_Lists.List;

   type Index_IDs is record
      Index : Positive_Index_Type; -- into Source
      IDs   : Token_ID_Lists.List;
   end record;

   package Index_IDs_Vectors is new Ada.Containers.Vectors (Ada.Containers.Count_Type, Index_IDs);

   subtype Motion_Param_Array is Index_IDs_Vectors.Vector;

   procedure Motion_Action
     (Data    : in out Parse_Data_Type;
      Nonterm : in     Augmented_Token'Class;
      Source  : in     Augmented_Token_Array;
      Params  : in     Motion_Param_Array);
   --  Implements [1] wisi-motion-action.

   type Index_Faces is record
      Index       : Positive_Index_Type; -- into Source
      Prefix_Face : Integer; -- into grammar.Face_List
      Suffix_Face : Integer; -- into grammar.Face_List
   end record;

   type Face_Apply_Param_Array is array (Natural range <>) of Index_Faces;

   procedure Face_Apply_Action
     (Data    : in out Parse_Data_Type;
      Nonterm : in     Augmented_Token'Class;
      Source  : in     Augmented_Token_Array;
      Params  : in     Face_Apply_Param_Array);
   --  Implements [1] wisi-face-apply-action.

   type Indent_Pair (Comment_Present : Boolean := False) is
   record
      Indent : Integer;
      case Comment_Present is
      when True =>
         Comment_Indent : Integer;
      when False =>
         null;
      end case;
   end record;

   type Indent_Param_Array is array (Natural range <>) of Indent_Pair;

   procedure Indent_Action
     (Data    : in out Parse_Data_Type;
      Nonterm : in     Augmented_Token'Class;
      Source  : in     Augmented_Token_Array;
      Params  : in     Indent_Param_Array);
   --  Implements [1] wisi-indent-action.

   function Anchored_0
     (Data         : in out Parse_Data_Type;
      Index        : in     Integer;
      Indent_Delta : in     Integer)
     return Integer;
   --  Implements [1] wisi-anchored.

   procedure Put (Data : in Parse_Data_Type);
   --  Put parse result to Ada.Text_IO.Standard_Output, as encoded
   --  set-text-property responses as defined in [2]
   --  wisi-process-parse--execute.

   procedure Put
     (Errors     : in WisiToken.Token_Region.Error_List_Arrays.Vector;
      Descriptor : in WisiToken.Descriptor'Class);
   --  Put errors to Ada.Text_IO.Standard_Output, as encoded error
   --  responses as defined in [2] wisi-process-parse--execute.

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

   type Cache_Type is record
      Label : Parse_Action_Type; -- text-property
      Pos   : Buffer_Pos;           -- implicit in wisi-cache

      Statement_ID   : Token_ID;    -- wisi-cache-nonterm
      ID             : Token_ID;    -- wisi-cache-token
      Length         : Natural;     -- wisi-cache-last
      Class          : Class_Type;  -- wisi-cache-class
      Containing_Pos : Nil_Buffer_Pos; -- wisi-cache-containing
      Prev_Pos       : Nil_Buffer_Pos; -- wisi-cache-prev
      Next_Pos       : Nil_Buffer_Pos; -- wisi-cache-next
      End_Pos        : Nil_Buffer_Pos; -- wisi-cache-end
   end record;

   function Key (Cache : in Cache_Type) return Buffer_Pos is (Cache.Pos);

   package Cache_Trees is new SAL.Gen_Unbounded_Definite_Red_Black_Trees (Cache_Type, Buffer_Pos);

   type Indent_Labels is (Int, Anchor, Anchored, Nested_Anchor);

   package Int_Vectors is new Ada.Containers.Vectors (Natural, Natural);

   type Indent_Type (Label : Indent_Labels := Indent_Labels'First) is record
      --  [1] wisi-ind struct
      Begin_Pos : Buffer_Pos; -- in line-begin in [1]
      case Label is
      when Int =>
         Int_Indent : Natural;

      when Anchor =>
         Anchor_IDs    : Int_Vectors.Vector;
         Anchor_Indent : Natural;

      when Anchored =>
         Anchored_ID    : Natural;
         Anchored_Delta : Integer;

      when Nested_Anchor =>
         Nested_Anchor_IDs     : Int_Vectors.Vector;
         Nested_Anchored_ID    : Natural;
         Nested_Anchored_Delta : Integer;
      end case;
   end record;

   package Indent_Vectors is new Ada.Containers.Vectors (Natural, Indent_Type);
   package Cursor_Lists is new Ada.Containers.Doubly_Linked_Lists (Cache_Trees.Cursor, Cache_Trees."=");

   type Parse_Data_Type is new Base_Data_Type with
   record
      Caches        : Cache_Trees.Tree;      -- Used for Navigate, Face.
      End_Positions : Cursor_Lists.List;     -- Used for Navigate.
      Indents       : Indent_Vectors.Vector; -- Used for Indent.
   end record;

end WisiToken.Wisi_Runtime;
