--  Abstract :
--
--  Ada implementation of wisi parser actions.
--
--  References
--
--  [1] wisi.el - defines parse action functions.
--
--  [2] wisi-elisp-parse.el - defines parse action functions.
--
--  [3] wisi-process-parse.el - defines elisp/process API
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
with WisiToken.Token_Line_Comment;
with WisiToken.Token_Region;
package WisiToken.Wisi_Runtime is

   type Parse_Action_Type is (Navigate, Face, Indent);

   type Parse_Data_Type is tagged limited private;

   procedure Initialize
     (Data             : in out Parse_Data_Type;
      Semantic_State   : in     WisiToken.Token_Line_Comment.State_Access;
      Source_File_Name : in     String;
      Parse_Action     : in     Parse_Action_Type;
      Line_Count       : in     Line_Number_Type;
      Params           : in     String);
   --  Line_Count only used for Indent. Params contains language-specific
   --  indent parameter values.

   procedure Reset (Data : in out Parse_Data_Type; Init_Done : in Boolean := False);
   --  Reset for a new parse, with data from previous Initialize.

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

   package Token_ID_Lists is new Ada.Containers.Doubly_Linked_Lists (WisiToken.Token_ID);

   Empty_IDs : constant Token_ID_Lists.List := Token_ID_Lists.Empty_List;

   function "+" (Item : in WisiToken.Token_ID) return Token_ID_Lists.List;
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
   --  Implements [2] wisi-motion-action.

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
   --  Implements [2] wisi-face-apply-action.

   procedure Face_Apply_List_Action
     (Data    : in out Parse_Data_Type;
      Nonterm : in     Augmented_Token'Class;
      Tokens  : in     Augmented_Token_Array;
      Params  : in     Face_Apply_Param_Array);
   --  Implements [2] wisi-face-apply-list-action.

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
   --  Implements [2] wisi-face-mark-action.

   type Face_Remove_Param_Array is array (Natural range <>) of Positive_Index_Type;

   procedure Face_Remove_Action
     (Data    : in out Parse_Data_Type;
      Nonterm : in     Augmented_Token'Class;
      Tokens  : in     Augmented_Token_Array;
      Params  : in     Face_Remove_Param_Array);
   --  Implements [2] wisi-face-remove-action.

   ----------
   --  Indent
   --
   --  elisp indent functions are represented by the Indent_Param type,
   --  not Ada functions. This is to get the execution time right; in
   --  elisp, the array of parameters to wisi-indent-action is not
   --  evaluated when wisi-indent-action is called; each parameter is
   --  evaluated by wisi-elisp-parse--indent-compute-delta.

   type Simple_Indent_Param_Label is -- not hanging
     (Int,
      Anchored_0, -- wisi-anchored
      Anchored_1, -- wisi-anchored%
      Anchored_2, -- wisi-anchored%-
      Anchored_3, -- wisi-anchored*
      Anchored_4, -- wisi-anchored*-
      Language    -- language-specific function
     );
   subtype Anchored_Label is Simple_Indent_Param_Label range Anchored_0 .. Anchored_4;

   --  Arguments to language-specific functions are integers; one of
   --  delta, Token_Number, or Token_ID - the syntax does not distinguish
   --  among these three types.

   package Indent_Arg_Arrays is new Ada.Containers.Vectors (Positive_Index_Type, Integer);

   function "+" (Item : in Integer) return Indent_Arg_Arrays.Vector;
   function "&" (List : in Indent_Arg_Arrays.Vector; Item : in Integer) return Indent_Arg_Arrays.Vector;
   function "&" (Left, Right : in Integer) return Indent_Arg_Arrays.Vector;

   type Delta_Type (<>) is private;

   type Language_Indent_Function is access function
     (Data          : in out Parse_Data_Type'Class;
      Tokens        : in     Augmented_Token_Array;
      Indenting     : in     Token_Line_Comment.Token;
      Args          : in     Indent_Arg_Arrays.Vector)
     return Delta_Type;

   Null_Args : Indent_Arg_Arrays.Vector renames Indent_Arg_Arrays.Empty_Vector;

   type Simple_Indent_Param (Label : Simple_Indent_Param_Label := Int) is
   record
      case Label is
      when Int =>
         Int_Delta : Integer;

      when Anchored_Label =>
         Anchored_Index : Positive_Index_Type;
         Anchored_Delta : Integer;

      when Language =>
         Function_Ptr : Language_Indent_Function;
         Args         : Indent_Arg_Arrays.Vector;
      end case;
   end record;

   type Indent_Param_Label is
     (Simple,
      Hanging_0, -- wisi-hanging
      Hanging_1, -- wisi-hanging%
      Hanging_2  -- wisi-hanging%-
     );
   subtype Hanging_Label is Indent_Param_Label range Hanging_0 .. Hanging_2;

   type Indent_Param (Label : Indent_Param_Label := Simple) is
   record
      case Label is
      when Simple =>
         Param : Simple_Indent_Param;

      when Hanging_Label =>
         Hanging_Delta_1 : Simple_Indent_Param;
         Hanging_Delta_2 : Simple_Indent_Param;

      end case;
   end record;

   type Indent_Pair (Comment_Present : Boolean := False) is
   record
      Code_Delta : Indent_Param;
      case Comment_Present is
      when True =>
         Comment_Delta : Indent_Param;
      when False =>
         null;
      end case;
   end record;

   type Indent_Param_Array is array (Positive_Index_Type range <>) of Indent_Pair;

   procedure Indent_Action_0
     (Data    : in out Parse_Data_Type'Class;
      Nonterm : in     Augmented_Token'Class;
      Tokens  : in     Augmented_Token_Array;
      Params  : in     Indent_Param_Array);
   --  Implements [2] wisi-indent-action.

   procedure Indent_Action_1
     (Data    : in out Parse_Data_Type'Class;
      Nonterm : in     Augmented_Token'Class;
      Tokens  : in     Augmented_Token_Array;
      N       : in     Positive_Index_Type;
      Params  : in     Indent_Param_Array);
   --  Implements [2] wisi-indent-action*.

   function Indent_Hanging_1
     (Data            : in out Parse_Data_Type;
      Tokens          : in     Augmented_Token_Array;
      Indenting_Token : in     Token_Line_Comment.Token;
      Delta_1         : in     Simple_Indent_Param;
      Delta_2         : in     Simple_Indent_Param;
      Option          : in     Boolean;
      Accumulate      : in     Boolean)
     return Delta_Type;
   --  [2] wisi-elisp-parse--hanging-1
   --
   --  Language specific child packages override this to implement
   --  wisi-elisp-parse-indent-hanging-function.

   procedure Put (Data : in out Parse_Data_Type);
   --  Perform some post-parse actions, then put parse result to
   --  Ada.Text_IO.Current_Output, as encoded responses as defined in [3]
   --  wisi-process-parse--execute.

   procedure Put
     (Errors     : in WisiToken.Token_Region.Error_List_Arrays.Vector;
      Descriptor : in WisiToken.Descriptor'Class);
   --  Put Errors to Ada.Text_IO.Current_Output, as encoded error
   --  responses as defined in [3] wisi-process-parse--execute.

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
      Statement_ID   : WisiToken.Token_ID;  -- wisi-cache-nonterm
      ID             : WisiToken.Token_ID;  -- wisi-cache-token
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

   package Anchor_ID_Vectors is new Ada.Containers.Vectors (Natural, Positive);

   type Indent_Type (Label : Indent_Label := Not_Set) is record
      --  [2] wisi-elisp-parse--indent elements. Indent values may be
      --  negative while indents are being computed.
      case Label is
      when Not_Set =>
         null;

      when Int =>
         Int_Indent : Integer;

      when Anchor =>
         Anchor_IDs    : Anchor_ID_Vectors.Vector; --  Largest ID first.
         Anchor_Indent : Integer;

      when Anchored =>
         Anchored_ID    : Positive;
         Anchored_Delta : Integer; -- added to Anchor_Indent of Anchor_ID

      when Anchor_Anchored =>
         Anchor_Anchored_IDs   : Anchor_ID_Vectors.Vector;
         Anchor_Anchored_ID    : Natural;
         Anchor_Anchored_Delta : Integer;
      end case;
   end record;
   First_Anchor_ID : constant Positive := Positive'First;

   package Indent_Vectors is new Ada.Containers.Vectors (Line_Number_Type, Indent_Type);
   package Navigate_Cursor_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Navigate_Cache_Trees.Cursor, Navigate_Cache_Trees."=");

   type Parse_Data_Type is tagged limited
   record
      Semantic_State   : WisiToken.Token_Line_Comment.State_Access;
      Source_File_Name : Ada.Strings.Unbounded.Unbounded_String;
      Parse_Action     : Parse_Action_Type;
      Navigate_Caches  : Navigate_Cache_Trees.Tree;  -- Set by Navigate.
      End_Positions    : Navigate_Cursor_Lists.List; -- Dynamic data for Navigate.
      Face_Caches      : Face_Cache_Trees.Tree;      -- Set by Face.
      Indents          : Indent_Vectors.Vector;      -- Set by Indent.

      --  Copied from language-specific parameters
      Indent_Comment_Col_0 : Boolean := False;

      --  Dynamic data for Indent
      Max_Anchor_ID     : Integer;
      Indenting_Comment : Boolean;
   end record;

   type Simple_Delta_Labels is (Int, Anchored);

   type Simple_Delta_Type (Label : Simple_Delta_Labels := Int) is
   record
      case Label is
      when Int =>
         Int_Delta : Integer;

      when Anchored =>
         Anchored_ID         : Natural;
         Anchored_Delta      : Integer;
         Anchored_Accumulate : Boolean;

      end case;
   end record;
   subtype Anchored_Delta is Simple_Delta_Type (Anchored);

   type Delta_Labels is (Simple, Hanging);

   type Delta_Type (Label : Delta_Labels := Simple) is
   record
      --  Matches DELTA input to wisi--indent-token-1
      case Label is
      when Simple =>
         Simple_Delta : Simple_Delta_Type;

      when Hanging =>
         Hanging_First_Line  : Line_Number_Type;
         Hanging_Paren_State : Natural;
         Hanging_Delta_1     : Simple_Delta_Type; -- indentation of first line
         Hanging_Delta_2     : Simple_Delta_Type; -- indentation of continuation lines
         Hanging_Accumulate  : Boolean;
      end case;
   end record;

   Null_Delta : constant Delta_Type := (Simple, (Int, 0));

   ----------
   --  Utilities for language-specific child packages

   function Current_Indent_Offset
     (Data         : in Parse_Data_Type;
      Anchor_Token : in Token_Line_Comment.Token;
      Offset       : in Integer)
     return Integer;

   function Find_Token_On_Stack (Data : in Parse_Data_Type; ID : in Token_ID) return Token_Line_Comment.Token;

   function Indent_Anchored_2
     (Data        : in out Parse_Data_Type;
      Anchor_Line : in     Line_Number_Type;
      Last_Line   : in     Line_Number_Type;
      Offset      : in     Integer;
      Accumulate  : in     Boolean)
     return Delta_Type;
   --  [2] wisi-elisp-parse--anchored-2

   function Indent_Compute_Delta
     (Data            : in out Parse_Data_Type'Class;
      Tokens          : in     Augmented_Token_Array;
      Param           : in     Indent_Param;
      Indenting_Token : in     Token_Line_Comment.Token)
     return Delta_Type;

   procedure Indent_Token_1
     (Data            : in out Parse_Data_Type;
      Indenting_Token : in     Token_Line_Comment.Token;
      Delta_Indent    : in     Delta_Type);
   --  [2] wisi-elisp-parse--indent-token-1. Sets Data.Indents, so caller
   --  may not be in a renames for a Data.Indents element.

end WisiToken.Wisi_Runtime;
