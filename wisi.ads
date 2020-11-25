--  Abstract :
--
--  Ada implementation of wisi parser actions.
--
--  References
--
--  [1] wisi-parse-common.el - defines common stuff.
--
--  [2] wisi.texi - defines parse action functions.
--
--  [3] wisi-process-parse.el - defines elisp/process API
--
--  Copyright (C) 2017 - 2020 Free Software Foundation, Inc.
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

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;
with SAL.Gen_Unbounded_Definite_Red_Black_Trees;
with SAL.Gen_Unbounded_Definite_Vectors.Gen_Image;
with WisiToken.Lexer;
with WisiToken.Parse.LR;
with WisiToken.Syntax_Trees;
package Wisi is
   use all type WisiToken.Syntax_Trees.Augmented_Class_Access;
   use all type WisiToken.Base_Buffer_Pos;

   function Image_Augmented (Aug : in WisiToken.Syntax_Trees.Augmented_Class_Access_Constant) return String;
   function Image_Action (Action : in WisiToken.Syntax_Trees.Post_Parse_Action) return String;
   --  For Image_Augmented, Image_Action in Syntax_Trees.Print_Tree, Parser.Execute_Action

   type Post_Parse_Action_Type is (Navigate, Face, Indent);

   type Parse_Data_Type
     (Line_Begin_Token    : not null access WisiToken.Parse.Line_Token_Vectors.Vector;
      Line_Begin_Char_Pos : not null access constant WisiToken.Line_Pos_Vectors.Vector)
     is new WisiToken.Syntax_Trees.User_Data_Type with private;

   procedure Initialize
     (Data              : in out Parse_Data_Type;
      Lexer             : in     WisiToken.Lexer.Handle;
      Descriptor        : access constant WisiToken.Descriptor;
      Post_Parse_Action : in     Post_Parse_Action_Type;
      Begin_Line        : in     WisiToken.Line_Number_Type;
      End_Line          : in     WisiToken.Line_Number_Type;
      Begin_Indent      : in     Integer;
      Params            : in     String);
   --  Begin_Line, Begin_Indent only used for Indent. Params
   --  contains language-specific indent parameter values.

   overriding procedure Reset (Data : in out Parse_Data_Type);
   --  Reset for a new parse, with data from previous Initialize.

   function Source_File_Name (Data : in Parse_Data_Type) return String;
   function Post_Parse_Action (Data : in Parse_Data_Type) return Post_Parse_Action_Type;

   overriding
   procedure Lexer_To_Augmented
     (Data          : in out Parse_Data_Type;
      Tree          : in out WisiToken.Syntax_Trees.Tree'Class;
      Token         : in     WisiToken.Base_Token;
      Grammar_Token : in     WisiToken.Syntax_Trees.Node_Access);

   overriding
   function Copy_Augmented
     (User_Data : in Parse_Data_Type;
      Augmented : in WisiToken.Syntax_Trees.Augmented_Class_Access)
     return WisiToken.Syntax_Trees.Augmented_Class_Access;

   overriding
   procedure Initialize_Actions
     (Data : in out Parse_Data_Type;
      Tree : in     WisiToken.Syntax_Trees.Tree'Class);

   overriding
   procedure Insert_Token
     (Data           : in out Parse_Data_Type;
      Tree           : in out WisiToken.Syntax_Trees.Tree'Class;
      Inserted_Token : in     WisiToken.Syntax_Trees.Valid_Node_Access);

   overriding
   procedure Delete_Token
     (Data          : in out Parse_Data_Type;
      Tree          : in out WisiToken.Syntax_Trees.Tree'Class;
      Deleted_Token : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Prev_Token    : in     WisiToken.Syntax_Trees.Node_Access);

   overriding
   procedure Reduce
     (Data    : in out Parse_Data_Type;
      Tree    : in out WisiToken.Syntax_Trees.Tree'Class;
      Nonterm : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Tokens  : in     WisiToken.Syntax_Trees.Node_Access_Array);

   type Navigate_Class_Type is (Motion, Statement_End, Statement_Override, Statement_Start, Misc);
   --  Matches [1] wisi-class-list.

   type Index_Navigate_Class is record
      Index : WisiToken.Positive_Index_Type; -- into Tokens
      Class : Navigate_Class_Type;
   end record;

   type Statement_Param_Array is array (Natural range <>) of Index_Navigate_Class;

   procedure Statement_Action
     (Data    : in out Parse_Data_Type;
      Tree    : in     WisiToken.Syntax_Trees.Tree;
      Nonterm : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Tokens  : in     WisiToken.Syntax_Trees.Valid_Node_Access_Array;
      Params  : in     Statement_Param_Array);
   --  Implements [2] wisi-statement-action.

   procedure Name_Action
     (Data    : in out Parse_Data_Type;
      Tree    : in     WisiToken.Syntax_Trees.Tree;
      Nonterm : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Tokens  : in     WisiToken.Syntax_Trees.Valid_Node_Access_Array;
      Name    : in     WisiToken.Positive_Index_Type);
   --  Implements [2] wisi-name-action.

   type Index_ID is record
      Index : WisiToken.Positive_Index_Type; -- into Tokens
      ID    : WisiToken.Token_ID;
      --  If ID is not Invalid_Token_ID, it is the first token in the
      --  nonterm that Index points to that should have a navigate cache for
      --  Motion_Action to link to; an error is reported by Motion_Action if
      --  it does not.
      --
      --  If ID is Invalid_Token_ID, and the token at Index is a
      --  nonterminal, the first token in that nonterminal must have a
      --  navigate cache; an error is reported by Motion_Action if not.
   end record;

   package Index_ID_Vectors is new Ada.Containers.Vectors (Ada.Containers.Count_Type, Index_ID);

   subtype Motion_Param_Array is Index_ID_Vectors.Vector;

   Invalid_Token_ID : WisiToken.Token_ID := WisiToken.Invalid_Token_ID;
   --  So Create_Parser can just use "Invalid_Token_ID".

   procedure Motion_Action
     (Data    : in out Parse_Data_Type;
      Tree    : in     WisiToken.Syntax_Trees.Tree;
      Nonterm : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Tokens  : in     WisiToken.Syntax_Trees.Valid_Node_Access_Array;
      Params  : in     Motion_Param_Array);
   --  Implements [2] wisi-motion-action.

   type Index_Faces is record
      Index       : WisiToken.Positive_Index_Type; -- into Tokens
      Prefix_Face : Integer; -- into grammar.Face_List
      Suffix_Face : Integer; -- into grammar.Face_List
   end record;

   type Face_Apply_Param_Array is array (Natural range <>) of Index_Faces;

   procedure Face_Apply_Action
     (Data    : in out Parse_Data_Type;
      Tree    : in     WisiToken.Syntax_Trees.Tree;
      Nonterm : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Tokens  : in     WisiToken.Syntax_Trees.Valid_Node_Access_Array;
      Params  : in     Face_Apply_Param_Array);
   --  Implements [2] wisi-face-apply-action.

   procedure Face_Apply_List_Action
     (Data    : in out Parse_Data_Type;
      Tree    : in     WisiToken.Syntax_Trees.Tree;
      Nonterm : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Tokens  : in     WisiToken.Syntax_Trees.Valid_Node_Access_Array;
      Params  : in     Face_Apply_Param_Array);
   --  Implements [2] wisi-face-apply-list-action.

   type Face_Class_Type is (Prefix, Suffix);

   type Index_Face_Class is record
      Index : WisiToken.Positive_Index_Type; -- into Tokens
      Class : Face_Class_Type;
   end record;

   type Face_Mark_Param_Array is array (Natural range <>) of Index_Face_Class;

   procedure Face_Mark_Action
     (Data    : in out Parse_Data_Type;
      Tree    : in     WisiToken.Syntax_Trees.Tree;
      Nonterm : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Tokens  : in     WisiToken.Syntax_Trees.Valid_Node_Access_Array;
      Params  : in     Face_Mark_Param_Array);
   --  Implements [2] wisi-face-mark-action.

   type Face_Remove_Param_Array is array (Natural range <>) of WisiToken.Positive_Index_Type;

   procedure Face_Remove_Action
     (Data    : in out Parse_Data_Type;
      Tree    : in     WisiToken.Syntax_Trees.Tree;
      Nonterm : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Tokens  : in     WisiToken.Syntax_Trees.Valid_Node_Access_Array;
      Params  : in     Face_Remove_Param_Array);
   --  Implements [2] wisi-face-remove-action.

   ----------
   --  Indent
   --
   --  Indent functions are represented by the Indent_Param type.

   type Simple_Indent_Param_Label is
     --  Not hanging
     (None,
      Int,
      Anchored_0, -- [2] wisi-anchored
      Anchored_1, -- [2] wisi-anchored%
      Block,      -- [2] wisi-block
      Language    -- [2] language-specific function
     );
   subtype Simple_Param_Anchored is Simple_Indent_Param_Label range Anchored_0 .. Anchored_1;

   --  Arguments to language-specific functions are integers; one of
   --  delta, Token_Number, or Token_ID - the syntax does not distinguish
   --  among these three types.

   package Indent_Arg_Arrays is new Ada.Containers.Vectors (WisiToken.Positive_Index_Type, Integer);

   function "+" (Item : in Integer) return Indent_Arg_Arrays.Vector;
   function "&" (List : in Indent_Arg_Arrays.Vector; Item : in Integer) return Indent_Arg_Arrays.Vector;
   function "&" (Left, Right : in Integer) return Indent_Arg_Arrays.Vector;

   type Delta_Type (<>) is private;

   type Language_Indent_Function is access function
     (Data              : in out Parse_Data_Type'Class;
      Tree              : in     WisiToken.Syntax_Trees.Tree;
      Nonterm           : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Tree_Tokens       : in     WisiToken.Syntax_Trees.Valid_Node_Access_Array;
      Tree_Indenting    : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Indenting_Comment : in     Boolean;
      Args              : in     Indent_Arg_Arrays.Vector)
     return Delta_Type;

   Null_Args : Indent_Arg_Arrays.Vector renames Indent_Arg_Arrays.Empty_Vector;

   type Simple_Indent_Param (Label : Simple_Indent_Param_Label := None) is
   record
      case Label is
      when None =>
         null;

      when Block | Int =>
         Int_Delta : Integer;

      when Simple_Param_Anchored =>
         Anchored_Index : WisiToken.Positive_Index_Type;
         Anchored_Delta : Integer;

      when Language =>
         Function_Ptr : Language_Indent_Function;
         Args         : Indent_Arg_Arrays.Vector;
      end case;
   end record;

   function Image (Item : in Simple_Indent_Param) return String;

   function Add_Simple_Indent_Param (Left, Right : in Simple_Indent_Param) return Simple_Indent_Param;

   type Indent_Param_Label is
     (Simple,
      Hanging_0, -- [2] wisi-hanging
      Hanging_1, -- [2] wisi-hanging%
      Hanging_2  -- [2] wisi-hanging*
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

   function Image (Item : in Indent_Param) return String;

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

   function Image (Item : in Indent_Pair) return String;

   type Indent_Param_Array is array (WisiToken.Positive_Index_Type range <>) of Indent_Pair;

   procedure Indent_Action_0
     (Data    : in out Parse_Data_Type'Class;
      Tree    : in     WisiToken.Syntax_Trees.Tree;
      Nonterm : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Tokens  : in     WisiToken.Syntax_Trees.Valid_Node_Access_Array;
      Params  : in     Indent_Param_Array);
   --  Implements [2] wisi-indent-action.

   function Indent_Hanging_1
     (Data              : in out Parse_Data_Type;
      Tree              : in     WisiToken.Syntax_Trees.Tree;
      Nonterm           : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Tokens            : in     WisiToken.Syntax_Trees.Valid_Node_Access_Array;
      Tree_Indenting    : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Indenting_Comment : in     Boolean;
      Delta_1           : in     Simple_Indent_Param;
      Delta_2           : in     Simple_Indent_Param;
      Label             : in     Hanging_Label)
     return Delta_Type;
   --  Implements [2] wisi-hanging, wisi-hanging%, wisi-hanging*
   --
   --  Language specific child packages may override this to implement
   --  language-specific cases.

   ----------
   --  Other

   procedure Refactor_Help (Data : in Parse_Data_Type) is null;

   procedure Refactor
     (Data       : in out Parse_Data_Type;
      Tree       : in out WisiToken.Syntax_Trees.Tree;
      Action     : in     Positive;
      Edit_Begin : in     WisiToken.Buffer_Pos) is null;

   type Arg_Index_Array is array (Positive range <>) of WisiToken.Positive_Index_Type;

   procedure Put_Language_Action
     (Data    : in Parse_Data_Type;
      Content : in String);
   --  Send a Language_Action message to Emacs.

   procedure Put (Data : in out Parse_Data_Type; Parser : in WisiToken.Parse.Base_Parser'Class);
   --  Perform additional post-parse actions, then put result to
   --  Ada.Text_IO.Current_Output, as encoded responses as defined in [3]
   --  wisi-process-parse--execute.

   procedure Put (Lexer_Errors : in WisiToken.Lexer.Error_Lists.List);
   procedure Put
     (Data         : in Parse_Data_Type;
      Lexer_Errors : in WisiToken.Lexer.Error_Lists.List;
      Parse_Errors : in WisiToken.Parse.LR.Parse_Error_Lists.List;
      Recover      : in WisiToken.Parse.LR.Recover_Op_Arrays.Vector;
      Tree         : in WisiToken.Syntax_Trees.Tree);
   --  Put Lexer_Errors, Parse_Errors, Recover to Ada.Text_IO.Current_Output,
   --  as encoded error responses as defined in [3]
   --  wisi-process-parse--execute.

   procedure Put_Error (Data : in Parse_Data_Type; Line_Number : in WisiToken.Line_Number_Type; Message : in String);
   --  Put an error elisp form to Ada.Text_IO.Standard_Output.

private

   type Augmented is new WisiToken.Syntax_Trees.Base_Augmented with record
      --  Most fields are set by Lexer_To_Augmented at parse time; others
      --  are set by Reduce for nonterminals.

      Deleted : Boolean := False;
      --  Set True by Parse_Data_Type.Delete_Token; Non_Grammar tokens are
      --  moved to the previous non-deleted token.

      --  The following fields are only needed for indent.

      Paren_State : Integer := 0;
      --  Parenthesis nesting count, before token.

      First_Indent_Line : WisiToken.Line_Number_Type := WisiToken.Invalid_Line_Number;
      Last_Indent_Line  : WisiToken.Line_Number_Type := WisiToken.Invalid_Line_Number;
      --  Lines that need indenting; first token on these lines is contained
      --  in this token. If token has no lines that need indenting, these
      --  are Invalid_Line_Number.
      --
      --  First_, Last_Indent_Line include blank and comment lines between
      --  grammar tokens, but exclude trailing blanks and comments after the
      --  last token, so they can be indented differently.

      First_Trailing_Comment_Line : WisiToken.Line_Number_Type := WisiToken.Invalid_Line_Number;
      Last_Trailing_Comment_Line  : WisiToken.Line_Number_Type := WisiToken.Invalid_Line_Number;
      --  Trailing comment or blank lines (after the last contained grammar
      --  token) that need indenting. Excludes comments following code on a
      --  line. If there are no such lines, these are Invalid_Line_Number.

      Inserted_After : Boolean := False;
      --  True if Insert_Token moved the token from before the next to
      --  after the previous.
   end record;
   type Augmented_Access is access all Augmented;
   type Augmented_Access_Constant is access constant Augmented;

   type Augmented_Const_Ref (Element : not null access constant Augmented) is null record with
     Implicit_Dereference => Element;

   function To_Augmented_Const_Ref (Item : in WisiToken.Syntax_Trees.Augmented_Class_Access) return Augmented_Const_Ref
   is (Element => Augmented_Access_Constant (Item));

   type Augmented_Var_Ref (Element : not null access Augmented) is null record with
     Implicit_Dereference => Element;

   function To_Augmented_Var_Ref (Item : in WisiToken.Syntax_Trees.Augmented_Class_Access) return Augmented_Var_Ref
   is (Element => Augmented_Access (Item))
   with Pre => Item /= null;

   type Augmented_Token is record
      Base : WisiToken.Base_Token;
      Aug  : Augmented;
   end record;

   function First_Line
     (Token             : in Augmented_Token;
      Indenting_Comment : in Boolean)
     return WisiToken.Line_Number_Type;
   function Last_Line
     (Token             : in Augmented_Token;
      Indenting_Comment : in Boolean)
     return WisiToken.Line_Number_Type;
   --  Return first and last line that need indenting in Token's region.

   package Line_Paren_Vectors is new SAL.Gen_Unbounded_Definite_Vectors
     (WisiToken.Line_Number_Type, Integer, Default_Element => Integer'Last);

   function Image is new Line_Paren_Vectors.Gen_Image (WisiToken.Trimmed_Image);

   type Nil_Buffer_Pos (Set : Boolean := False) is record
      case Set is
      when True =>
         Item : WisiToken.Buffer_Pos;
      when False =>
         null;
      end case;
   end record;

   Nil : constant Nil_Buffer_Pos := (Set => False);

   function Image (Item : in Nil_Buffer_Pos) return String
   is (if Item.Set then Item.Item'Image else " nil");

   type Navigate_Cache_Type is record
      Pos : WisiToken.Buffer_Pos;
      --  Implicit in [1] wisi-cache. This is a character position in the
      --  source text; it must be on a Shared_Terminal (not a virtual terminal).

      Statement_ID   : WisiToken.Token_ID;   -- [1] wisi-cache-nonterm
      ID             : WisiToken.Token_ID;   -- [1] wisi-cache-token
      Length         : Natural;              -- [1] wisi-cache-last
      Class          : Navigate_Class_Type;  -- [1] wisi-cache-class
      Containing_Pos : Nil_Buffer_Pos;       -- [1] wisi-cache-containing
      Prev_Pos       : Nil_Buffer_Pos;       -- [1] wisi-cache-prev
      Next_Pos       : Nil_Buffer_Pos;       -- [1] wisi-cache-next
      End_Pos        : Nil_Buffer_Pos;       -- [1] wisi-cache-end
   end record;

   function Key (Cache : in Navigate_Cache_Type) return WisiToken.Buffer_Pos is (Cache.Pos);

   function Key_Compare (Left, Right : in WisiToken.Buffer_Pos) return SAL.Compare_Result is
     (if Left > Right then SAL.Greater
      elsif Left = Right then SAL.Equal
      else SAL.Less);

   package Navigate_Cache_Trees is new SAL.Gen_Unbounded_Definite_Red_Black_Trees
     (Navigate_Cache_Type, WisiToken.Buffer_Pos);

   function Key (Cache : in WisiToken.Buffer_Region) return WisiToken.Buffer_Pos is (Cache.First);

   package Name_Cache_Trees is new SAL.Gen_Unbounded_Definite_Red_Black_Trees
     (WisiToken.Buffer_Region, WisiToken.Buffer_Pos);

   type Nil_Integer (Set : Boolean := False) is record
      case Set is
      when True =>
         Item : Integer;
      when False =>
         null;
      end case;
   end record;

   type Face_Cache_Type is record
      Char_Region : WisiToken.Buffer_Region;
      Class       : Face_Class_Type;
      Face        : Nil_Integer; -- not set, or index into *-process-faces-names
   end record;

   function Key (Cache : in Face_Cache_Type) return WisiToken.Buffer_Pos is (Cache.Char_Region.First);

   package Face_Cache_Trees is new SAL.Gen_Unbounded_Definite_Red_Black_Trees (Face_Cache_Type, WisiToken.Buffer_Pos);

   type Indent_Label is (Not_Set, Int, Anchor_Nil, Anchor_Int, Anchored, Anchor_Anchored);
   subtype Anchored_Indent_Label is Indent_Label range Anchored .. Anchor_Anchored;

   package Anchor_ID_Vectors is new Ada.Containers.Vectors (Natural, Positive);

   type Indent_Type (Label : Indent_Label := Not_Set) is record
      --  Indent values may be negative while indents are being computed.
      Controlling_Token_Line : WisiToken.Line_Number_Type := WisiToken.Invalid_Line_Number;

      case Label is
      when Not_Set =>
         null;

      when Int =>
         Int_Indent : Integer;

      when Anchor_Nil =>
         Anchor_Nil_IDs : Anchor_ID_Vectors.Vector; --  Largest ID first.

      when Anchor_Int =>
         Anchor_Int_IDs    : Anchor_ID_Vectors.Vector; --  Largest ID first.
         Anchor_Int_Indent : Integer; --  Indent for this token.

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

   package Indent_Vectors is new SAL.Gen_Unbounded_Definite_Vectors
     (WisiToken.Line_Number_Type, Indent_Type, Default_Element => (others => <>));
   package Navigate_Cursor_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Navigate_Cache_Trees.Cursor, Navigate_Cache_Trees."=");

   type Parse_Data_Type
     (Line_Begin_Token    : not null access WisiToken.Parse.Line_Token_Vectors.Vector;
      Line_Begin_Char_Pos : not null access constant WisiToken.Line_Pos_Vectors.Vector)
     is new WisiToken.Syntax_Trees.User_Data_Type with
   record
      --  Aux token info
      First_Comment_ID : WisiToken.Token_ID := WisiToken.Invalid_Token_ID;
      Last_Comment_ID  : WisiToken.Token_ID := WisiToken.Invalid_Token_ID;
      Left_Paren_ID    : WisiToken.Token_ID := WisiToken.Invalid_Token_ID;
      Right_Paren_ID   : WisiToken.Token_ID := WisiToken.Invalid_Token_ID;

      Embedded_Quote_Escape_Doubled : Boolean := False;

      --  Data from parsing

      Line_Paren_State : Line_Paren_Vectors.Vector;
      --  Parenthesis nesting state at the start of each line; used by
      --  Indent. Set by Initialize_Actions.

      Current_Paren_State : Integer;
      --  Current parenthesis nesting state; used by Indent. Set by
      --  Initialize_Actions.

      --  Data for post-parse actions

      Lexer             : WisiToken.Lexer.Handle;
      Descriptor        : access constant WisiToken.Descriptor;
      Post_Parse_Action : Post_Parse_Action_Type;
      Navigate_Caches   : Navigate_Cache_Trees.Tree;  -- Set by Navigate.
      Name_Caches       : Name_Cache_Trees.Tree;      -- Set by Navigate.
      End_Positions     : Navigate_Cursor_Lists.List; -- Dynamic data for Navigate.
      Face_Caches       : Face_Cache_Trees.Tree;      -- Set by Face.
      Indents           : Indent_Vectors.Vector;      -- Set by Indent.
      Begin_Indent      : Integer;                    -- Indentation of line at start of parse.

      --  Copied from language-specific parameters
      Indent_Comment_Col_0 : Boolean := False;

      --  Dynamic data for Indent
      Max_Anchor_ID : Integer;
   end record;

   type Simple_Delta_Labels is (None, Int, Anchored);

   type Simple_Delta_Type (Label : Simple_Delta_Labels := None) is
   record
      Controlling_Token_Line : WisiToken.Line_Number_Type;
      --  If Invalid_Line_Number, delta should not be ignored.

      case Label is
      when None =>
         null;

      when Int =>
         Int_Delta : Integer;

      when Anchored =>
         Anchored_ID    : Natural;
         Anchored_Delta : Integer;

      end case;
   end record;

   function Image (Item : in Simple_Delta_Type) return String;
   --  For debugging

   type Delta_Labels is (Simple, Hanging);

   type Delta_Type (Label : Delta_Labels := Simple) is
   record
      case Label is
      when Simple =>
         Simple_Delta : Simple_Delta_Type;

      when Hanging =>
         Hanging_First_Line  : WisiToken.Line_Number_Type;
         Hanging_Paren_State : Integer;

         Hanging_Delta_1 : Simple_Delta_Type;
         --  Indentation of first line in token; Null_Delta if first line does
         --  not need indenting

         Hanging_Delta_2 : Simple_Delta_Type; -- indentation of continuation lines
      end case;
   end record;

   Null_Delta : constant Delta_Type := (Simple, (None, WisiToken.Invalid_Line_Number));

   function Image (Item : in Delta_Type) return String;
   --  For debugging

   ----------
   --  Utilities for language-specific child packages

   function First
     (Data  : in Parse_Data_Type'Class;
      Tree  : in WisiToken.Syntax_Trees.Tree'Class;
      Token : in WisiToken.Syntax_Trees.Node_Access)
     return Boolean;
   --  True if Token is first token on Token.Line; False if Token is Invalid_Node_Access

   function Current_Indent_Offset
     (Data         : in Parse_Data_Type'Class;
      Tree         : in WisiToken.Syntax_Trees.Tree'Class;
      Anchor_Token : in WisiToken.Base_Token;
      Offset       : in Integer)
     return Integer;
   --  Return offset from beginning of first token on line containing
   --  Anchor_Token, to beginning of Anchor_Token, plus Offset.

   function Get_Augmented_Token
     (Tree  : in WisiToken.Syntax_Trees.Tree'Class;
      Token : in WisiToken.Syntax_Trees.Valid_Node_Access)
     return Augmented_Token
   is ((Base => Tree.Base_Token (Token),
        Aug  => Augmented_Access (Tree.Augmented (Token)).all))
   with Pre => Tree.Augmented (Token) /= null;

   function Get_Augmented_Const
     (Tree  : in WisiToken.Syntax_Trees.Tree'Class;
      Token : in WisiToken.Syntax_Trees.Valid_Node_Access)
     return Augmented_Const_Ref
   with Pre => Tree.Augmented (Token) /= null;

   function Get_Augmented_Var
     (Tree  : in WisiToken.Syntax_Trees.Tree'Class;
      Token : in WisiToken.Syntax_Trees.Valid_Node_Access)
     return Augmented_Var_Ref
   with Pre => Tree.Augmented (Token) /= null;

   function Get_Text
     (Data       : in Parse_Data_Type;
      Tree       : in WisiToken.Syntax_Trees.Tree;
      Tree_Index : in WisiToken.Syntax_Trees.Valid_Node_Access)
     return String;
   --  Return text contained by Tree_Index token in source file
   --  (lexer.buffer).

   function Elisp_Escape_Quotes (Item : in String) return String;
   --  Prefix any '"' in Item with '\' for elisp.

   function Indent_Anchored_2
     (Data        : in out Parse_Data_Type;
      Anchor_Line : in     WisiToken.Line_Number_Type;
      Last_Line   : in     WisiToken.Line_Number_Type;
      Offset      : in     Integer)
     return Delta_Type;

   function Indent_Compute_Delta
     (Data              : in out Parse_Data_Type'Class;
      Tree              : in     WisiToken.Syntax_Trees.Tree;
      Nonterm           : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Tokens            : in     WisiToken.Syntax_Trees.Valid_Node_Access_Array;
      Param             : in     Indent_Param;
      Tree_Indenting    : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Indenting_Comment : in     Boolean)
     return Delta_Type;

   procedure Indent_Token_1
     (Data              : in out Parse_Data_Type;
      Tree              : in     WisiToken.Syntax_Trees.Tree;
      Indenting_Token   : in     Augmented_Token;
      Delta_Indent      : in     Delta_Type;
      Indenting_Comment : in     Boolean);
   --  Sets Data.Indents, so caller may not be in a renames for a
   --  Data.Indents element.

   --  Visible for language-specific children. Must match list in
   --  [3] wisi-process-parse--execute.
   Navigate_Cache_Code  : constant String := "1";
   Face_Property_Code   : constant String := "2";
   Indent_Code          : constant String := "3";
   Lexer_Error_Code     : constant String := "4";
   Parser_Error_Code    : constant String := "5";
   Check_Error_Code     : constant String := "6";
   Recover_Code         : constant String := "7 ";
   End_Code             : constant String := "8";
   Name_Property_Code   : constant String := "9";
   Edit_Action_Code     : constant String := "10";
   Language_Action_Code : constant String := "11 ";

end Wisi;
