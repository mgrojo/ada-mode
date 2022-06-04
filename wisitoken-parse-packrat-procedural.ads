--  Abstract :
--
--  Procedural packrat parser, supporting only direct left recursion.
--
--  Coding style, algorithm is the same as generated by
--  wisi-generate_packrat, but in procedural form.
--
--  References:
--
--  See parent.
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

with WisiToken.Productions;
with WisiToken.Parse.Parser;
package WisiToken.Parse.Packrat.Procedural is

   type Parser (First_Nonterminal, Last_Nonterminal : Token_ID) is new WisiToken.Parse.Parser.Parser
     (First_Nonterminal => First_Nonterminal,
      Last_Nonterminal  => Last_Nonterminal)
   with record

      Grammar               : WisiToken.Productions.Prod_Arrays.Vector;
      Start_ID              : Token_ID;
      Direct_Left_Recursive : Token_ID_Set (First_Nonterminal .. Last_Nonterminal);
   end record;
   type Parser_Access is access Parser;

   function Create
     (Grammar               : in WisiToken.Productions.Prod_Arrays.Vector;
      Direct_Left_Recursive : in Token_ID_Set;
      Start_ID              : in Token_ID;
      Lexer                 : in WisiToken.Lexer.Handle;
      Productions           : in WisiToken.Syntax_Trees.Production_Info_Trees.Vector;
      User_Data             : in WisiToken.Syntax_Trees.User_Data_Access)
     return Procedural.Parser;

   overriding
   procedure Packrat_Parse_No_Recover
     (Parser : in out Procedural.Parser;
      Resume : in     Boolean);

end WisiToken.Parse.Packrat.Procedural;
