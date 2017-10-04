--  Abstract :
--
--  Ada implementation of wisi parser actions.
--
--  References
--
--  [1] wisi.el - defines parse action functions.
--
--  [2] ada-indent-user-options.el - defines user settable indent parameters.
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
with Ada.Containers.Vectors;
package WisiToken.Wisi_Runtime is

   type Buffer_Data_Type is tagged private;

   procedure Initialize (Buffer_Data : in out Buffer_Data_Type; Line_Count : in Ada.Containers.Count_Type);

   type Class_Type is (Motion, Name, Statement_End, Statement_Override, Statement_Start, Misc);
   --  Matches [1] wisi-class-list.

   type Index_Class is record
      Index : Integer; -- into Source
      Class : Class_Type;
   end record;

   type Statement_Param_Array is array (Natural range <>) of Index_Class;

   procedure Statement_Action
     (Buffer_Data : in out Buffer_Data_Type;
      Nonterm     : in     Augmented_Token'Class;
      Source      : in     Augmented_Token_Array;
      Params      : in     Statement_Param_Array);

   procedure Containing_Action
     (Nonterm   : in Augmented_Token'Class;
      Source    : in Augmented_Token_Array;
      Container : in Integer;
      Contained : in Integer);

   type Token_ID_Array is array (Natural range <>) of Token_ID;
   Empty_IDs : constant Token_ID_Array (1 .. 0) := (others => Invalid_Token_ID);

   pragma Warnings (Off, "creation of ""Index_IDs"" object may raise Storage_Error");
   --  We only create these objects in grammar actions via aggregate
   --  literals for Index_IDs_Vectors.
   type Index_IDs (ID_Count : Natural := 0) is record
      Index : Integer; -- into Source
      IDs   : Token_ID_Array (1 .. ID_Count);
   end record;
   pragma Warnings (On);

   package Index_IDs_Vectors is new Ada.Containers.Indefinite_Vectors (Natural, Index_IDs);

   subtype Motion_Param_Array is Index_IDs_Vectors.Vector;

   procedure Motion_Action
     (Nonterm : in Augmented_Token'Class;
      Source  : in Augmented_Token_Array;
      Params  : in Motion_Param_Array);
   --  Implements [1] wisi-motion-action.

   type Index_Faces is record
      Index       : Integer; -- into Source
      Prefix_Face : Integer; -- into grammar.Face_List
      Suffix_Face : Integer; -- into grammar.Face_List
   end record;

   type Face_Apply_Param_Array is array (Natural range <>) of Index_Faces;

   procedure Face_Apply_Action
     (Nonterm : in Augmented_Token'Class;
      Source  : in Augmented_Token_Array;
      Params  : in Face_Apply_Param_Array);
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
     (Nonterm : in Augmented_Token'Class;
      Source  : in Augmented_Token_Array;
      Params  : in Indent_Param_Array);
   --  Implements [1] wisi-indent-action.

   function Anchored_0 (Index : in Integer; Indent_Delta : in Integer) return Integer;
   --  Implements [1] wisi-anchored.

   --  Indent parameters from [2]
   Ada_Indent_Broken : Integer;

private

   type Cache_Type is record
      Label : Parse_Action_Type; -- text-property
      Pos   : Natural;           -- implicit in wisi-cache

      Containing_ID  : Token_ID;   -- wisi-cache-nonterm
      ID             : Token_ID;   -- wisi-cache-token
      Length         : Natural;    -- wisi-cache-last
      Class          : Class_Type; -- wisi-cache-class
      Containing_Pos : Natural;    -- wisi-cache-containing
      Prev_Pos       : Natural;    -- wisi-cache-prev
      Next_Pos       : Natural;    -- wisi-cache-next
      End_Pos        : Natural;    -- wisi-cache-end
   end record;

   package Cache_Lists is Ada.Containiners.Doubly_Linked_List (Cache_Type);

   type Line_Type is record
      Begin_Pos : Natural;
      Indent    : Natural;
      Caches    : Cache_Lists.List;
   end record;

   package Line_Vectors is Ada.Containers.Vectors (Line_Type);

   type Buffer_Data is tagged record
      Lines : Line_Vectors.Vector;
   end record;

end WisiToken.Wisi_Runtime;
