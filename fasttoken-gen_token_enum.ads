--  Abstract :
--
--  Support for an enumerated token type
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

pragma License (GPL);

with FastToken.Lexer.Regexp;
with FastToken.Production;
with FastToken.Token;
generic
   type Token_Enum_ID is (<>);
   First_Terminal    : Token_Enum_ID;
   Last_Terminal     : Token_Enum_ID;
   First_Nonterminal : Token_Enum_ID;
   Last_Nonterminal  : Token_Enum_ID;
   EOF_ID            : Token_Enum_ID;
   Accept_ID         : Token_Enum_ID;
package FastToken.Gen_Token_Enum is

   function "+" (Item : in Token_Enum_ID) return Token_ID
     is (Token_ID'First + Token_Enum_ID'Pos (Item));

   function "-" (Item : in Token_ID) return Token_Enum_ID
     is (Token_Enum_ID'Val (Item - Token_ID'First));

   function Token_Enum_Image return Token_Array_String;

   LR1_Descriptor : aliased FastToken.Descriptor :=
     (First_Terminal    =>  +First_Terminal,
      Last_Terminal     =>  +Last_Terminal,
      First_Nonterminal =>  +First_Nonterminal,
      Last_Nonterminal  =>  +Last_Nonterminal,
      EOF_ID            =>  +EOF_ID,
      Accept_ID         =>  +Accept_ID,
      Image             => Token_Enum_Image,
      Image_Width       => Token_Enum_ID'Width);

   LALR_Descriptor : aliased FastToken.LALR_Descriptor :=
     (First_Terminal    =>  +First_Terminal,
      Last_Terminal     =>  +Last_Terminal,
      First_Nonterminal =>  +First_Nonterminal,
      Last_Nonterminal  =>  +Last_Nonterminal,
      EOF_ID            =>  +EOF_ID,
      Accept_ID         =>  +Accept_ID,
      Propagate_ID      =>  +First_Nonterminal,
      Image             => Token_Enum_Image,
      Image_Width       => Token_Enum_ID'Width);

   type Enum_Syntax is array (Token_Enum_ID range Token_Enum_ID'First .. Last_Terminal) of
     FastToken.Lexer.Regexp.Syntax_Item;

   function To_Syntax (Item : in Enum_Syntax) return FastToken.Lexer.Regexp.Syntax;

   function "&" (Left, Right : in Token_Enum_ID) return FastToken.Token.List.Instance;

   function "&"
     (Left  : in FastToken.Token.List.Instance;
      Right : in Token_Enum_ID)
     return FastToken.Token.List.Instance;

   function "+" (Left : in Token_Enum_ID; Right : in Semantic_Action) return FastToken.Production.Right_Hand_Side;

   function "<="
     (Left  : in Token_Enum_ID;
      Right : in FastToken.Production.Right_Hand_Side)
     return FastToken.Production.Instance;

   type Augmented_Token is new FastToken.Augmented_Token with record
      Enum_ID : Token_Enum_ID;
   end record;

   type State_Type is new FastToken.Token.Semantic_State with null record;

   overriding procedure Reset (State : access State_Type) is null;

   overriding procedure Input_Token
     (Token : in     Token_ID;
      State : access State_Type;
      Lexer : in     FastToken.Lexer.Handle)
     is null;

   overriding procedure Push_Token
     (Token : in     Token_ID;
      State : access State_Type)
     is null;

   overriding procedure Merge_Tokens
     (Nonterm : in     Token_ID;
      Index   : in     Natural;
      Tokens  : in     Token.List.Instance;
      Action  : in     Semantic_Action;
      State   : access State_Type);
   --  Puts trace of production, and calls Action if non-null;
   --  otherwise does nothing.

   overriding procedure Recover
     (Popped_Tokens  : in     Token.List.Instance;
      Skipped_Tokens : in     Token.List.Instance;
      Pushed_Token   : in     Token_ID;
      State          : access State_Type)
     is null;

   ----------
   --  For unit tests

   subtype Terminal_ID is Token_Enum_ID range First_Terminal .. Last_Terminal;
   subtype Nonterminal_ID is Token_Enum_ID range First_Nonterminal .. Last_Nonterminal;
   subtype Grammar_ID is Token_Enum_ID range First_Terminal .. Last_Nonterminal;

   type Nonterminal_Array_Token_Set is array (Nonterminal_ID, Grammar_ID) of Boolean;

   function To_Nonterminal_Array_Token_Set
     (Item : in Nonterminal_Array_Token_Set)
     return FastToken.Token_Array_Token_Set;

   type Nonterminal_Array_Terminal_Set is array (Nonterminal_ID, Terminal_ID) of Boolean;

   function To_Nonterminal_Array_Terminal_Set
     (Item : in Nonterminal_Array_Terminal_Set)
     return FastToken.Token_Array_Token_Set;

   type Token_Array is array (Positive range <>) of Token_Enum_ID;

   function "+" (Item : in Token_Array) return FastToken.Token_ID_Set;
   function "+" (Item : in Token_Enum_ID) return FastToken.Token_ID_Set;

end FastToken.Gen_Token_Enum;
