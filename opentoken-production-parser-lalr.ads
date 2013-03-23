-------------------------------------------------------------------------------
--
-- Copyright (C) 2002, 2003, 2009, 2010, 2013 Stephe Leake
-- Copyright (C) 1999 Ted Dennison
--
-- This file is part of the OpenToken package.
--
-- The OpenToken package is free software; you can redistribute it and/or
-- modify it under the terms of the  GNU General Public License as published
-- by the Free Software Foundation; either version 3, or (at your option)
-- any later version. The OpenToken package is distributed in the hope that
-- it will be useful, but WITHOUT ANY WARRANTY; without even the implied
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for  more details.  You should have received
-- a copy of the GNU General Public License  distributed with the OpenToken
-- package;  see file GPL.txt.  If not, write to  the Free Software Foundation,
-- 59 Temple Place - Suite 330,  Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
--  This package provides an implementation of a LALR (Look-Ahead
--  Left-to-right scanning Rightmost-deriving) parser for grammars
--  defined by a production list. This is probably the most popular
--  method due to it being a good trade-off between the amount of
--  grammars handled and the size of its parse table.
-----------------------------------------------------------------------------
generic
package OpenToken.Production.Parser.LALR is

   type Instance is new OpenToken.Production.Parser.Instance with private;

   overriding function Generate
     (Grammar              : in Production_List.Instance;
      Analyzer             : in Tokenizer.Instance;
      Trace                : in Boolean := False;
      Put_Grammar          : in Boolean := False;
      Ignore_Unused_Tokens : in Boolean := False;
      First_State_Index    : in Integer := 1)
     return Instance;
   --  We don't use OpenToken.Trace here; we often want to see a trace
   --  of the parser execution without the parser generation.
   --  Analyzer is copied.

   overriding procedure Parse (Parser : in out Instance);

   procedure Cleanup (Parser : in out Instance) is null;
   --  Free any resources used by Parser.

   procedure Put_Table (Parser : in Instance);

private

   --  Type for parser states
   type State_Index is new Integer;

   type Parse_Action_Verbs is (Shift, Reduce, Accept_It, Error);
   type Parse_Action_Rec (Verb : Parse_Action_Verbs := Shift) is record
      case Verb is
      when Shift =>
         State : State_Index;
      when Reduce | Accept_It =>
         Production : OpenToken.Production.Instance;
         Length     : Natural;
      when Error =>
         null;
      end case;
   end record;

   type Parse_Action_Node;
   type Parse_Action_Node_Ptr is access Parse_Action_Node;

   type Parse_Action_Node is record
      Item : Parse_Action_Rec;
      Next : Parse_Action_Node_Ptr; -- non-null only for conflicts
   end record;

   type Action_Node;
   type Action_Node_Ptr is access Action_Node;

   type Action_Node is record
      Symbol : Tokenizer.Terminal_ID;
      Action : Parse_Action_Node_Ptr;
      Next   : Action_Node_Ptr;
   end record;

   type Reduction_Node;
   type Reduction_Node_Ptr is access Reduction_Node;

   type Reduction_Node is record
      Symbol : Token.Token_ID;
      State  : State_Index;
      Next   : Reduction_Node_Ptr;
   end record;

   type Parse_State is record
      Action_List    : Action_Node_Ptr;
      Reduction_List : Reduction_Node_Ptr;
   end record;

   type Parse_Table is array (State_Index range <>) of Parse_State;

   type Parse_Table_Ptr is access Parse_Table;

   type Instance is new OpenToken.Production.Parser.Instance with record
      Table : Parse_Table_Ptr;
   end record;

end OpenToken.Production.Parser.LALR;
