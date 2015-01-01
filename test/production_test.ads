-------------------------------------------------------------------------------
--
--  Copyright (C) 2013, 2014, 2015 Stephen Leake.  All Rights Reserved.
--  Copyright (C) 2000 Ted Dennison
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
--
-------------------------------------------------------------------------------

with OpenToken.Token.Analyzer;
with OpenToken.Token.Nonterminal;
with OpenToken.Production;
with OpenToken.Production.List;
package Production_Test is

   type Token_IDs is (Int_ID, Real_ID, String_ID, Keyword_ID, Expression_ID, Literal_ID);

   Token_Image_Width : Integer := Token_IDs'Width;
   package Master_Token is new OpenToken.Token (Token_IDs, Int_ID, Keyword_ID, Token_IDs'Image);
   package Tokenizer is new Master_Token.Analyzer;
   package Nonterminal is new Master_Token.Nonterminal;
   package Production is new OpenToken.Production (Master_Token, Nonterminal);
   package Production_List is new Production.List;

end Production_Test;
