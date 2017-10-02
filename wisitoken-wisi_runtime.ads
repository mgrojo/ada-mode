--  Abstract :
--
--  Ada implementation of wisi parser actions.
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
package WisiToken.Wisi_Runtime is

   type Index_Class is record
      Index : Integer; -- into Source
      Class : Integer; -- into grammar.Class_List
   end record;

   type Statement_Param_Array is array (Natural range <>) of Index_Class;

   procedure Statement_Action
     (Nonterm : in Token_Line_Comment.Token;
      Source  : in Augmented_Token_Array;
      Params  : in Statement_Param_Array);

   procedure Containing_Action
     (Nonterm   : in Token_Line_Comment.Token;
      Source    : in Augmented_Token_Array;
      Container : in Integer;
      Contained : in Integer);

   type Token_ID_Array is array (Natural range <>) of Token_ID;
   Empty_IDs : constant Token_ID_Array (1 .. 0) := (others => Invalid_Token_ID);

   type Index_IDs (ID_Count : Integer) is record
      Index : Integer; -- into Source
      IDs   : Token_ID_Array (1 .. ID_Count);
   end record;

   type Motion_Param_Array is array (Natural range <>) of Index_IDs;

   procedure Motion_Action
     (Nonterm : in Token_Line_Comment.Token;
      Source  : in Augmented_Token_Array;
      Params  : in Motion_Param_Array);

   type Index_Faces is record
      Index       : Integer; -- into Source
      Prefix_Face : Integer; -- into grammar.Face_List
      Suffix_Face : Integer; -- into grammar.Face_List
   end record;

   type Face_Apply_Param_Array is array (Natural range <>) of Index_Faces;

   procedure Face_Apply_Action
     (Nonterm : in Token_Line_Comment.Token;
      Source  : in Augmented_Token_Array;
      Params  : in Face_Apply_Param_Array);

   type Indent_Pair (Comment_Present : Boolean) is
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
     (Nonterm : in Token_Line_Comment.Token;
      Source  : in Augmented_Token_Array;
      Params  : in Indent_Param_Array);

end WisiToken.Wisi_Runtime;
