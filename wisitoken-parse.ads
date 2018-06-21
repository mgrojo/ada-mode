--  Abstract :
--
--  Subprograms common to more than one parser, higher-level than in wisitoken.ads
--
--  Copyright (C) 2018 Stephen Leake All Rights Reserved.
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

with WisiToken.Lexer;
with WisiToken.Syntax_Trees;
package WisiToken.Parse is

   function Next_Grammar_Token
     (Terminals        : in out          Base_Token_Arrays.Vector;
      Line_Begin_Token : in out          Line_Begin_Token_Vectors.Vector;
      Descriptor       : in              WisiToken.Descriptor'Class;
      Lexer            : not null access WisiToken.Lexer.Instance'Class;
      User_Data        : in              WisiToken.Syntax_Trees.User_Data_Access)
     return Token_ID;
   --  Get next token from Lexer, call User_Data.Lexer_To_Augmented. If
   --  it is a grammar token, store in Terminals and return its id.
   --  Otherwise, repeat.
   --
   --  Propagates Fatal_Error from Lexer.

   procedure Lex_All
     (Lexer            : in     WisiToken.Lexer.Handle;
      Terminals        : in out Base_Token_Arrays.Vector;
      Line_Begin_Token : in out Line_Begin_Token_Vectors.Vector;
      User_Data        :        WisiToken.Syntax_Trees.User_Data_Access;
      Trace            : access WisiToken.Trace'Class);
   --  Clear Terminals, Line_Begin_Token; reset User_Data. Then call
   --  Next_Grammar_Token repeatedly until EOF_ID is returned.

end WisiToken.Parse;
